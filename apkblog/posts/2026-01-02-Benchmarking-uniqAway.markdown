---
title: "uniqAway: Benchmarking odd things"
author: Andreas Klebinger
categories: programming, haskell, ghc
---

GHC uses a nifty `uniqAway` function to make up new variables "names" on the fly.
Today I woke up wondering if we could replace the current mechanism with atomics,
and here are my musings and some benchmarks on the topic.

# The contenders

### uniqAway

When GHC inlines code it needs to ensure the variables introduced by the inlined code
don't clash with existing variables in scope. In order to accomplish that we
rely on a set of variables already in scope, potentially
cloning the variable in all but it's "name". GHC internally uses `Uniques` to
represent variables which are basically just integers.

The algorithm to do this is implemented by a function called `uniqAway` and is fairly
simple:

* If the variables unique doesn't clash with the existing scope simply use it as-is.
* If the variables unique clashes with something already in scope replace the unique
  of the variable with the next free one.

We "simply" determine the next free unique by taking the highest unique in scope and adding one.

The actual code, omitting some of the irrelevant details, looks like this:

```haskell
uniqAway :: InScopeSet -> Var -> Var
uniqAway in_scope var
  -- Make a new one
  | var `elemInScopeSet` in_scope = setVarUnique var (unsafeGetFreshLocalUnique in_scope)
  -- Reuse variable as-is.
  | otherwise                     = var

unsafeGetFreshLocalUnique :: InScopeSet -> Unique
unsafeGetFreshLocalUnique (InScope set)
  | Just (uniq,_) <- IntMap.lookupLT maxLocalUnique set
  , let uniq' = mkLocalUnique uniq
  , not $ uniq' `ltUnique` minLocalUnique
  = incrUnique uniq'

  | otherwise
  = minLocalUnique
```

### Atomics

Since Word64 for our purposes is basically infinite we could achieve the same thing using
a counter and unconditionally incrementing the unique whenever we call `uniqAway`.

To make this sound in a multithreaded setting we have to use atomics. So the logic would be something like:

* Have a global variable holding the next unique.
* Simply do an atomic increment every time we call uniqAway, using the new value as a unique.

That is something like:

```
uniqAway :: InScopeSet -> Var -> Var
uniqAway in_scope var = setVarUnique var (atomic_increment some_ref)
```

## Performance estimates

### Atomic performance

To a first approximation the cost of the atomic variant is easy to quantify.
It's always allocating a fresh `Var` plus the cost of atomic increment.

Ignoring GC for now this mean a few memory writes for the allocation and then somewhere in the range of 25-400 cycles for the atomic.
This is a huge range, but it depends a lot on the number of cores, contention and so on.

Let's just hand wave slightly and assume we get somewhere around 30 cycles in the common case.

### IntMap performance

What is the cost of a set lookup? IntMaps are essentially binary trees, they have some tricks but
treating them like simple binary trees we get a depth of `log(n)`.

Assuming InScopeSet is of a modest size we assume we need to recurse 8 steps into the tree to check for membership
or find the largest/smallest element. Finding the smallest/largest element works very similar to
membership tests perf wise so let's only consider performance of membership testing for now.

When checking for membership we have to:

* Compare the key with the current node in the tree.
* Branch on the result. Either descending into one of the subtrees or returning right away.

This means for every level we incur:
* Two branches, one of them being unpredictable
* Chasing down one pointer for a subtree
* Some reads/bit fiddling.

For a relatively new CPU let's assume we pay:
* 4 cycles for a read (from L1 cache)
* 10 cycles for a stall upon failed branch prediction.

We incur the read delay once per level, and with 50% hit rate for branch prediction the miss prediction penalty
for every other level. That means we can expect a cost of `~(10*4 + 8*4)` = \~72 cycles as a lower bound for the average cost of membership tests.

This is about twice our hand wavy estimate for atomics. So perhaps promising! And that only covers the good case where
there is no shadowing. We have to do about twice as much work if there actually is shadowing.

Seems like there is a good chance for atomics to outperform the `IntMap` approach.

## Benchmarking

The first rule of any performance work is **measure**. It's all good to huff and puff
about our cool new way to speed things up. But let's come up with some concrete benchmarks
to check our assumptions.

### IntMap benchmarks

Let's start by benchmarking IntMap performance:

```haskell
import Criterion.Main
import Data.IntMap.Strict as IM
import System.Random
import Control.Monad
import Control.DeepSeq

main :: IO ()
main = do
    let m = IM.fromList $ zip [1..256] (repeat ())
    rl <- replicateM 256 (randomIO :: IO Int)
    let rm = IM.fromList $ zip rl (repeat ())

    pure $! rnf (m,rm)

    let lookupm x = IM.lookup x m
    let lookupr x = IM.lookup x m

    defaultMain
        [   bench "overhead" $ whnf id (),
        bgroup
            "lookups-det"
            [ bench "1"  $ whnf lookupm 1
            , bench "11" $ whnf lookupm 11
            ],
         bgroup
            "lookups-map-random"
            [ bench "1"  $ whnf lookupr 1
            , bench "11" $ whnf lookupr 11
            ]
        ]
```

We are doing this for fun so we don't follow all good rules of benchmarking.
This means we have relatively noisy results from my notebook which criterion dutifully warns us about:

```
cabal run bench -- -L10
benchmarking overhead
time                 3.106 ns   (3.019 ns .. 3.217 ns)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 3.204 ns   (3.145 ns .. 3.272 ns)
std dev              242.5 ps   (211.9 ps .. 289.6 ps)
variance introduced by outliers: 88% (severely inflated)
```

If we were the ensure a quite stable system we could eliminate this noise. But since
this is mostly for fun and we only care about orders of magnitude for now this is fine.

While criterion will keep complaining I will omit the noise and only report means
for the rest of this post. The notebook rocks some 2.3Ghz CPU but as I said we don't
care too much about the exact numbers right now.

Here are typical results of the above benchmark on that machine:

```
benchmarking overhead
mean                 3.204 ns   (3.145 ns .. 3.272 ns)

benchmarking lookups-det/1
mean                 12.38 ns   (12.25 ns .. 12.57 ns)

benchmarking lookups-det/11
mean                 15.38 ns   (15.21 ns .. 15.74 ns)

benchmarking lookups-map-random/1
mean                 12.31 ns   (12.11 ns .. 12.60 ns)

benchmarking lookups-map-random/11
mean                 15.11 ns   (14.96 ns .. 15.31 ns)
```

Given 2.3Ghz this seems too good to be true. I claimed a lower bound of ~70cycles for the average run.
But the 9ns here amount to something closer to 20cycles. What is going on?

My prediction is fairly simple: We are running the *same* lookup in a loop for our benchmark.
This means branch prediction and prefetching will likely be optimal voiding our assumptions.

Let's beef up our benchmark to confirm this. Instead of looking for the same value in every iteration we can
look up a random value in every iteration:

```
    let lookupm, lookupr :: IO (Maybe ())
        lookupm = randomIO >>= \x -> pure (IM.lookup x m)
        lookupr = randomIO >>= \x -> pure (IM.lookup x rm)


    defaultMain
        [   bench "overhead" $ whnfIO (randomIO :: IO Int),
        bgroup
            "lookups-det"
            [ bench "r1"  $ whnfIO lookupm
            , bench "r2" $ whnfIO lookupm
            ],
         bgroup
            "lookups-map-random"
            [ bench "r1"  $ whnfIO lookupr
            , bench "r2" $ whnfIO lookupr
            ]
        ]
```

And if we look at the results:

```
benchmarking overhead
mean                 32.66 ns   (31.67 ns .. 33.91 ns)

benchmarking lookups-det/r1
mean                 66.32 ns   (65.41 ns .. 67.52 ns)

benchmarking lookups-det/r2
mean                 66.11 ns   (64.91 ns .. 67.35 ns)

benchmarking lookups-map-random/r1
mean                 79.40 ns   (78.24 ns .. 81.20 ns)

benchmarking lookups-map-random/r2
mean                 77.47 ns   (76.80 ns .. 78.51 ns)
```

We can notice a few things. First generating random numbers is expensive! ~32ns just for that.
We could do better but we will accept that for now.

Beyond this we can see cost of lookups went from 9-12ns to 34-47ns after we consider overhead!
That's both a huge increase but also nicely in line with our estimates. On my notebooks this
was ~93 cycles which is surprisingly close to my estimation of the lower bound
at ~70 cycles! So it seems to be in the right ballpark.

### Atomics

Now let's see if our ideas about atomics also hold up.

We can construct a similar benchmark for atomics:

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Criterion.Main
import GHC.Exts
import GHC.IO

data BA = BA (MutableByteArray# RealWorld)

atomicIncrementByteArray :: BA -> IO Int
atomicIncrementByteArray (BA ba) = IO $ \s ->
    case fetchAddIntArray# ba 0# 1# s of
        (# s2, i #) -> (# s2, I# i #)

baAddr :: BA -> IO Word
baAddr (BA ba) = IO $ \s ->
    case mutableByteArrayContents# ba of
        addr -> (# s, W# (int2Word# (addr2Int# addr)) #)

main :: IO ()
main = do
    ba <- IO (\s -> case newPinnedByteArray# 128# s of (# s2, ba #) -> (# s2, BA ba #))

    defaultMain
        [   bench "overhead" $ whnfIO (pure 32 :: IO Int)
        ,   bench "atomicIncrement" $ whnfIO (atomicIncrementByteArray ba)
        ]
```

And the results:

```
benchmarking overhead
mean                 3.836 ns   (3.766 ns .. 3.912 ns)

benchmarking atomicIncrement
mean                 8.593 ns   (8.421 ns .. 8.800 ns)
```

Not bad. This is equivalent to ~11 cycles anf a decent chunk faster than doing the lookups.
It's also a good deal faster than I expected!

But besides of a lack of multithreading it also doesn't account for unconditionally allocating a new `Var`.
Lets construct one more benchmark to measure them all.

## One Benchmark to rule them all:

I'll just lead with the code. You don't have to read it in depth we will discuss below.

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Criterion.Main
import GHC.Exts
import GHC.IO
import GHC.Types.Var
import GHC.Builtin.Types
import GHC.Types.Id
import GHC.Types.Unique
import GHC.Data.FastString
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import System.Random
import Control.Monad
import GHC.Word
import Control.DeepSeq

data BA = BA (MutableByteArray# RealWorld)

atomicIncrementByteArray :: BA -> IO Int
atomicIncrementByteArray (BA ba) = IO $ \s ->
    case fetchAddIntArray# ba 0# 1# s of
        (# s2, i #) -> (# s2, I# i #)

myUniqAway :: BA -> Var -> IO Var
myUniqAway ba var = do
    (mkUniqueGrimily . fromIntegral <$> atomicIncrementByteArray ba)
        >>= (pure . setVarUnique var)

{-# NOINLINE mkVar #-}
mkVar = randomIO >>= \x -> pure $ mkSysLocal (fsLit "ds") (mkUniqueGrimily x) intTy intTy

main :: IO ()
main = do
    ba <- IO (\s -> case newPinnedByteArray# 128# s of (# s2, ba #) -> (# s2, BA ba #))
    let mk_sized_r_scope n = do
            in_scope_uniques <- replicateM n (fromIntegral <$> (randomIO:: IO Word)) :: IO [Word64]
            return $ mkInScopeSetList $
                map
                    (\x -> mkSysLocal (fsLit "ds") (mkUniqueGrimily x) intTy intTy)
                    in_scope_uniques
    let mk_seq_scope n = mkInScopeSetList $
                map
                    (\x -> mkSysLocal (fsLit "ds") (mkUniqueGrimily x) intTy intTy)
                    [0..n-1]

    r_scope_64 <- mk_sized_r_scope 64
    r_scope_256 <- mk_sized_r_scope 256
    r_scope_512 <- mk_sized_r_scope 512
    r_scope_1024 <- mk_sized_r_scope 1024

    let s_scope_256 = mk_seq_scope 256
    let s_scope_512 = mk_seq_scope 512
    let s_scope_1024 = mk_seq_scope 1024

    let seq_in_scope (InScope vs) = seqVarSet vs

    pure $! rnf $ map seq_in_scope [r_scope_64, r_scope_256, r_scope_512, r_scope_1024]
    pure $! rnf $ map seq_in_scope [s_scope_256, s_scope_512, s_scope_1024]

    defaultMain
        [   bench "overhead" $ whnfIO (mkVar)
        ,   bench "atomicIncrement" $ whnfIO (atomicIncrementByteArray ba)
        ,   bench "myUniqAway" $ whnfIO (mkVar >>= myUniqAway ba)
        ,   bgroup "uniqAway_random_scope"
            [
                bench "64" $ whnfIO (mkVar >>= pure . uniqAway r_scope_64)
            ,   bench "256" $ whnfIO (mkVar >>= pure . uniqAway r_scope_256)
            ,   bench "512" $ whnfIO (mkVar >>= pure . uniqAway r_scope_512)
            ,   bench "1024" $ whnfIO (mkVar >>= pure . uniqAway r_scope_1024)
            ]
        ,   bgroup "uniqAway_seq_scope"
            [
                bench "256" $ whnfIO (mkVar >>= pure . uniqAway s_scope_256)
            ,   bench "512" $ whnfIO (mkVar >>= pure . uniqAway s_scope_512)
            ,   bench "1024" $ whnfIO (mkVar >>= pure . uniqAway s_scope_1024)
            ]
        ]
```

First off we now allocate a `Var` to clone during each benchmark. This comes with
significant overhead as it involves among other things generating a random unique.

We now have a implementation for `uniqAway` that unconditionally creates a new `Var`
using an atomic operation. It has a constant cost so there is not much point in
constructing more than one benchmark for it.

For the current implementation of `uniqAway` we construct `InScopeSet`s in two ways.
One containing a sequential list of variables with sequential Uniques.

The other variant constructs a `InScopeSet` from randomly sampled Uniques. For both
of these we vary the sizes to get an idea about how performance scales with the size
of the in scope set.

I run this particular benchmark on my desktop so the numbers are a bit different but
we get the same trends. First we can see the allocation of the `Var` doesn't seem to
add much compared to the raw cost of the atomic:

```
benchmarking overhead
mean                 13.72 ns   (13.64 ns .. 13.85 ns)

benchmarking atomicIncrement
mean                 5.327 ns   (5.300 ns .. 5.362 ns)

benchmarking myUniqAway
mean                 19.80 ns   (19.69 ns .. 19.94 ns)
```

We can see that at least in this micro benchmark the cost of allocation is barely
relevant compared to the atomic operation. The whole operation comes in at 20ns
independent of the size of the in scope set.

Not however this doesn't measure any potential degradations resulting from compilation
happening on multiple cores concurrently. I'm not too up to date with my bus locking
knowledge but I assume using the same memory address on multiple cores would come
with some cost even without contention.

But let's forget about concurrency and look at the performance of `uniqAway` as it is today.

```
benchmarking uniqAway_random_scope/64
mean                 41.75 ns   (41.52 ns .. 42.12 ns)

benchmarking uniqAway_random_scope/256
mean                 51.02 ns   (50.70 ns .. 51.48 ns)

benchmarking uniqAway_random_scope/512
mean                 55.13 ns   (54.84 ns .. 55.56 ns)

benchmarking uniqAway_random_scope/1024
mean                 59.22 ns   (59.00 ns .. 59.53 ns)
```

We see an additional ~20ns per invocation for in scope sets as small as 64 elements.
With the runtime increasing as the in scope set grows. We can nicely see that doubling
the size of the in scope set only adds ~4ns of overhead. The beauty of `O(log(n))` in
action! It's also quite a bit slower than using atomics.

But wait there is more:

```
benchmarking uniqAway_seq_scope/256
mean                 19.01 ns   (18.93 ns .. 19.12 ns)

benchmarking uniqAway_seq_scope/512
mean                 18.88 ns   (18.79 ns .. 19.00 ns)

benchmarking uniqAway_seq_scope/1024
mean                 18.94 ns   (18.84 ns .. 19.16 ns)
```

What is going on here. The runtime here is essentially constant independent of the
size. To explain this we have to dive into the implementation of `IntMap`:

```haskell
data IntMap a = Bin {-# UNPACK #-} !Prefix
                    !(IntMap a)
                    !(IntMap a)
              | Tip {-# UNPACK #-} !Key a
              | Nil

...

-- * The Prefix of a Bin indicates the common high-order bits that all keys in
--   the Bin share.

```

The magic here is the `Prefix` value. A random `Int` has about 0% chance to sit
inside a small contiguous number of values. Which means we can immediately tell
a variable is *not* in scope. This is basically the best case for `uniqAway` and it
performs similarly to the variant based on atomics.

# Conclusions

We have seen that a `uniqAway` implementation based on atomics performs as well as the
best case of the current implementation. Does this mean we should replace it?

Probably not. While it's likely to give a tiny performance benefit in the single
threaded case it's unclear how much overhead would be incurred for multi threaded
compilation. The simple atomic approach might be
slower in these cases. But it's quite difficult to reasonably benchmark this.

There is also the option to simple do away with atomics by engineering GHC such that
uniques spawned by `uniqAway` are only unique to a specific thread. This would
reduce the overhead even further, at the cost of some complexity. So that might be
a better way to go about things if we ever decide it's worth the spend actual effort
on this.

Last but not least: Does it even matter?

By instrumenting GHC we can check! When compiling `nofib/spectral/simple` from the nofib
benchmark suite GHC spends ~3s compiling. Invoking `uniqAway` ~500k times in that time.

In the best case, based on these numbers, I expect us shave off at most ~30ns for each call.
Which for 500k calls amounts to 0.015s.

A relative improvement of 0.005%. And it might be worse in practice!
Given these numbers I would argue this isn't the best use of any ones time.

But it was fun thinking about.












