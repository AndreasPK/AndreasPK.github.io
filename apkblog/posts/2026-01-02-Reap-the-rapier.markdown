---
title: "uniqAway: Can Atomics beat IntMaps?"
author: Andreas Klebinger
categories: programming, haskell, ghc
---

GHC uses a nifty `uniqAway` function to make up new variables "names" on the fly.
Today I woke up wondering if we could replace the current mechanism with atomics,
and here are my musings on the topic.

# The contenders

### uniqAway

When GHC inlines code it needs to ensure the variables introduced by the inlined code
don't clash with existing variables in scope.
In order to accomplish that we rely on a set of variables already in scope, potentially
cloning the variable but giving it a new Unique. Uniques being integers GHC internally uses
the distinguish between variables.

The algorithm to do this is implemented by a function called `uniqAway` and is fairly
simple:

* If the variables unique doesn't clash with the existing scope simply use it as-is.
* If the variables unique clashes with something already in scope clone replacing the unique with the next free one.

We simply determine the "next free unique" by taking the highest unique in scope and adding one.

The actual code, ommitting some of the irrelevant details, looks like this:

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

We could however use atomics by:

* Having a global variable holding the latest unique.
* Simply do an atomic increment every time we call uniqAway, using the new value as a unique.

That is something like:

```
uniqAway :: InScopeSet -> Var -> Var
uniqAway in_scope var = setVarUnique var (atomic_increment some_ref)
```

The downside is we must unconditionally allocate a new `Var`.

## Performance considerations

### Atomic performance.

To a first approximation the cost of the atomic variant is easy to quantify. It's always allocating a fresh `Var` plus the cost of atomic increment. Ignoring GC for now this mean a few memory writes for the allocation and then somewhere in the range of 25-400 cycles for the atomic.

This is a huge range, but it depends a lot on the number of cores, contention and so on. Let's just handwave and assume the lower bound is somewhere around 30 cycles.

### IntMap performance

What is the cost of a set lookup? IntMaps are essentially binary trees, they have some tricks but
treating them like simple binary trees we get a depth of `log(n)`.

Assuming a modest inscope set let's assume we need to incur 8 steps deep to check for membership
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
* 4 cycles for a read (from cache)
* 10 cycles for a misspredicted branch.

If we expect a 50% hitrate for branch prediction let's say we incur the read delay once per level, and the missprediction cost for every other level. That means we can expect a cost of
`~(10*4 + 8*4)` = ~72 cycles as a lower bound for the average cost of membership tests.

This means there is a chance for atomic to outperform the `IntMap` approach even if there
is no shadowing at all. Just from the cost of checking for conflicts!

## Benchmarking

The first rule of any performance work is **measure**. It's all good to huff and puff
about our cool new way to speed things up. So for fun let's come up with some things to measure.

###

Let's start by benchmarkin IntMap performance:

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

We are doing this for fun so we don't follow all good rules of benchmarking is resulting
some outliers which criterion dutifully warns us about:

```
cabal run bench -- -L10
benchmarking overhead
time                 3.106 ns   (3.019 ns .. 3.217 ns)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 3.204 ns   (3.145 ns .. 3.272 ns)
std dev              242.5 ps   (211.9 ps .. 289.6 ps)
variance introduced by outliers: 88% (severely inflated)
```

But since this is mostly for fun that's okay. And I will only list the mean in from here on out.
Here is the data from one run:

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

Given that I'm on a 2.3Ghz Machine this seems too good given that I claim a lower bound of ~70 for the average run. What is going on?

My prediction is fairly simple:  We are running the *same* lookup in a loop for our benchmark. This means branch prediction and prefetching will likely be optimal voiding our assumptions.

But we can beef up our benchmark. Instead of looking for the same value in every iteration we can
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

But the cost of lookups went from 9-12ns to 34-47ns! That's a huge increase and only reinforces
the point of how important branch prediction is. Now we are already looking at a cost of ~93 cycles
on this machine. And it's not far off of my estimation of the lower bound of ~70 cycles! So it seems
to be in the right ballpark.

### Atomics

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

Not bad. This is equivalent to ~11 cycles.


A more realistic benchmark:

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
    let !mkVar = randomIO >>= \x -> pure $ mkSysLocal (fsLit "ds") (mkUniqueGrimily x) intTy intTy
    r_scope_16 <- mk_sized_r_scope 16
    r_scope_256 <- mk_sized_r_scope 256
    r_scope_512 <- mk_sized_r_scope 512
    r_scope_1024 <- mk_sized_r_scope 1024

    let s_scope_256 = mk_seq_scope 256
    let s_scope_512 = mk_seq_scope 512
    let s_scope_1024 = mk_seq_scope 1024

    let seq_in_scope (InScope vs) = seqVarSet vs

    pure $! rnf $ map seq_in_scope [r_scope_16, r_scope_256, r_scope_512, r_scope_1024]
    pure $! rnf $ map seq_in_scope [s_scope_256, s_scope_512, s_scope_1024]

    defaultMain
        [   bench "overhead" $ whnfIO (mkVar)
        ,   bench "myUniqAway" $ whnfIO (mkVar >>= myUniqAway ba)
        ,   bgroup "uniqAway_random_scope"
            [
                bench "16" $ whnfIO (mkVar >>= pure . uniqAway r_scope_16)
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


```
benchmarking overhead
time                 43.94 ns   (41.89 ns .. 46.16 ns)
                     0.984 R²   (0.977 R² .. 0.991 R²)
mean                 43.99 ns   (42.45 ns .. 46.29 ns)
std dev              7.096 ns   (5.606 ns .. 9.613 ns)
variance introduced by outliers: 97% (severely inflated)

benchmarking myUniqAway
time                 43.34 ns   (42.00 ns .. 44.82 ns)
                     0.992 R²   (0.987 R² .. 0.996 R²)
mean                 43.47 ns   (42.56 ns .. 44.77 ns)
std dev              4.255 ns   (3.133 ns .. 6.080 ns)
variance introduced by outliers: 91% (severely inflated)

benchmarking uniqAway_random_scope/16
time                 60.79 ns   (59.75 ns .. 62.16 ns)
                     0.996 R²   (0.995 R² .. 0.998 R²)
mean                 64.09 ns   (62.52 ns .. 66.97 ns)
std dev              8.001 ns   (6.010 ns .. 11.76 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking uniqAway_random_scope/256
time                 83.92 ns   (80.77 ns .. 87.42 ns)
                     0.993 R²   (0.987 R² .. 0.999 R²)
mean                 82.09 ns   (80.76 ns .. 83.92 ns)
std dev              5.770 ns   (4.482 ns .. 7.962 ns)
variance introduced by outliers: 84% (severely inflated)

benchmarking uniqAway_random_scope/512
time                 93.29 ns   (91.08 ns .. 95.41 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 91.73 ns   (90.71 ns .. 93.09 ns)
std dev              4.583 ns   (3.751 ns .. 6.565 ns)
variance introduced by outliers: 72% (severely inflated)

benchmarking uniqAway_random_scope/1024
time                 96.16 ns   (95.18 ns .. 97.56 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 98.12 ns   (97.03 ns .. 99.76 ns)
std dev              5.377 ns   (4.062 ns .. 8.085 ns)
variance introduced by outliers: 75% (severely inflated)

benchmarking uniqAway_seq_scope/256
time                 47.83 ns   (46.60 ns .. 49.23 ns)
                     0.994 R²   (0.991 R² .. 0.998 R²)
mean                 47.65 ns   (46.79 ns .. 48.99 ns)
std dev              3.924 ns   (3.056 ns .. 5.120 ns)
variance introduced by outliers: 88% (severely inflated)

benchmarking uniqAway_seq_scope/512
time                 45.59 ns   (44.82 ns .. 46.37 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 45.50 ns   (45.00 ns .. 46.16 ns)
std dev              2.299 ns   (1.856 ns .. 2.954 ns)
variance introduced by outliers: 73% (severely inflated)

benchmarking uniqAway_seq_scope/1024
time                 45.36 ns   (44.85 ns .. 45.94 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 45.66 ns   (45.09 ns .. 46.52 ns)
std dev              2.628 ns   (1.885 ns .. 4.180 ns)
variance introduced by outliers: 78% (severely inflated)
```









