---
title: "Unknown functions and their arity."
author: Andreas Klebinger
categories: programming, haskell, ghc, type systems
---

In this post I will briefly jot down some thoughts I had while investigating ways to improve
the performance of unknown function calls.

# The Problem

## What are unknown function calls.

In GHC a unknown function call is a function call for which the callee is not statically known.

The prime example for this is the mapping function in `map`.

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

Here the function `f` is a unknown function, and `f x` is a unknown function call.
Since `f` will only be known at runtime, unless `map` is inlined.

## The downsides of unknown function calls.

We can contrast unknown function calls with similar code performing a known/regular function call
like the one below to explore the downsides.

```haskell
foo :: Int->Char
foo = ...

mapFoo :: [Int] -> [Char]
mapFoo _ []     = []
mapFoo (x:xs) = foo x : map xs
```

* Looking back at regular definition of map we can see that we don't have to pass `f` around as a function argument.
* In the `map` case the thunk for `(map f xs)` will have to retain a reference to `f` requiring slightly more memory usage.
* When generating code for `foo x` or `f x` the later can't inspect the function to determine it's arity. In fact we can't even ensure the
  function isn't something like `error "Blah"` so we have to (potentially) *evaluate* f before invoking it. Adding additional runtime
  overhead to each call to f.

There is nothing much we can do about the first two points without inlining `map`, so we will not talk about these further.
But we can try to improve on the third point!

# Potential improvements: Kinds Are Calling Conventions

The 2020 paper "Kinds Are Calling Conventions" elaborates on ways to track *Arity* in the type system.
I don't remember if the paper already suggested this but we can further also introduce *unlifted* functions.

With such an approach we would no longer have to verify the arity of unknown functions calls nor check for thunks
at runtime. The call `f x` would be essentially compiled to a straight function call.

## Impact assesment

The paper "Kinds Are Calling Conventions" is sadly rather quiet on the practicality of this approach beyond
stating that:

> "Parts of this work have been implemented already in the Glasgow Haskell Compiler,
> and we intend to further implement the entirety of kinds as calling conventions"

Sadly I don't think any of the work alluded to there was ever upstreamed.
And sadly not much has happened in this direction since either that I know of.

I always found that idea intriguing but before diving into GHC's type system
I set out to do some simple tests to check for potential payoff from such work.

## Constructing a Benchmark

What I *really* wanted to know is how much there is to gain from such a change in the best case.
What I came up with is a benchmark where I benchmark a function which:

* Walks over a unlifted strict list.
* Invokes a passed unknown function with each element of the list as argument.
* Returns nothing

This isn't too hard to create in haskell.

## The baseline

It looks something like this:

```haskell
type StrictList :: Type -> UnliftedType
data StrictList a = SNil | SCons !a !(StrictList a)

{-# OPAQUE  applyFunctionOne #-}
applyFunctionOne :: (Bool -> IO ()) -> StrictList Bool -> IO ()
applyFunctionOne f xs = go f xs
    where
        go _f SNil = pure ()
        go f (SCons x xs) = f x >> go f xs
```

Which we will simply invoke with a function `eval_ !_ = pure ()` as first argument.

We could *actually* map over the list as well, but I've refrained from doing that as it
makes it harder to conjure up the "Kinds are calling convention"-optimized version of this function.

## The optimized version

Assuming we have spent the 50-200+hours required to adjust GHC's type system we can assume that
when generating code for `f x` we can now look at the *type* of `f` to check `f`s arity and generate optimized code.

This would then allow us to get rid of both the need to check the arity (it's in `f`s type), and to evaluate `f` (it can now be a unlifted function).
While we can't have GHC generate this code for us today, it's possible to write the expected code directly in GHC's C-like Cmm language:

```C
applyFunctionDirect(P_ f, P_ xs)
{
    loop:
        W_ tag;
        //case xs of ...
        tag = xs & 7;
        xs = UNTAG(xs);

        //Nil
        if(tag == 1) {
            return (0);
        //Cons
        } else {
            //let x = head xs
            P_ x;
            x = P_[xs+8];
            //let xs' = tail xs
            xs = P_[xs+16];
            //(f x), will jump directly to f's code.
            call (%GET_ENTRY(UNTAG(f))) (f,x);
            //go f xs
            goto loop;
        }
}
```

This will take one function of the correct arity, and apply it to all elements of
the given StrictList without any (redundant) checks. If your not familiar with Cmm don't
worry. The details are not *that* important. They key difference between the optimized and
unoptimized version is:

```diff
-call stg_ap_pv_fast(f,x);
+call (%GET_ENTRY(UNTAG(f))) (f,x);
```

In the old (unoptimized) code we would call `stg_ap_pv_fast`, which in turn would check the
arity of the function, evaluate it if needed, and then would jump to the code for `f`.

In the new version instead we jump to the code for `f` directly. Allowing us to avoid the indirection via stg_ap_pv_fast and
the work it does. Nice!

## Benchmarking results

I've put a benchmark checking some cases online on [GitHub](https://github.com/AndreasPK/ghc-arity-experiment).

There are three kinds of benchmarks:

* `hs_apply` is the function written in Haskell today.
* `cmm_hslike` is a handwritten cmm implementation mirroring what GHC produces today.
* `cmm_direct` is what I expect GHC to produce, given a reasonable implementation of kinds are calling conventions.

I will just give the results first before discussing them:

```
benchmarking apply - Arity 1 Matched/hs_apply
mean                 6.536 μs   (6.536 μs .. 6.537 μs)

benchmarking apply - Arity 1 Matched/cmm_direct
mean                 6.570 μs   (6.570 μs .. 6.571 μs)

benchmarking apply - Arity 1 Matched/cmm_hslike
mean                 7.060 μs   (7.060 μs .. 7.060 μs)


benchmarking apply - Arity 3 Matched/hs_apply
mean                 6.040 μs   (6.039 μs .. 6.040 μs)

benchmarking apply - Arity 3 Matched/cmm_direct
mean                 5.573 μs   (5.573 μs .. 5.573 μs)


benchmarking apply - Arity 10 Matched/hs_apply
mean                 51.04 μs   (51.03 μs .. 51.06 μs)

benchmarking apply - Arity 10 Matched/cmm_direct
mean                 11.54 μs   (11.54 μs .. 11.54 μs)
```

### Benchmark Analysis

The performance of unknown function calls for low arities is pretty similar across all versions.
Most of the difference seems to be due to reasons like code alignment, measuring error and
other irrelevant factors. For example I've observed different versions being the fastest when
fiddling with the -fproc-alignment flag.

However once the Arity goes past some point this is *drastically* different. The optimized version is suddenly
about 5 times as fast! Let's explore this further.

#### The low arity case

I suspect this is because the primary bottleneck in these cases is *memory access*. And there is no difference in
the number of memory accesses between those versions.

This becomes clearer if we look at parts of the code for the function which does the
arity/evaluatedness checking: `stg_ap_pv_fast`

```C
stg_ap_pv_fast
{   W_ info;
    W_ arity;
    // Function is evaluated and has the right arity?
    if (GETTAG(R1)==2) {
        // Simply jump to it!
        jump %GET_ENTRY(R1-2) [R1,R2] ;
    }
    ...
```

Assuming we called it with a function of the right arity all it does is some bit twiddling and one conditional
branch before transfering control to the target function.

That is not to say there is *no* benefit. But when the actual work we perform consists of reading a linked list
from memory the overhead of this sort of indirection will be mostly insignificant.

It's imaginable that there are cases where this makes a meaningful difference. Things like applying a unknown function
to each element of a ByteArray perhaps. However I did not test this yet.

#### The high arity case

Why are things different for higher arity? GHC comes with pre-built functions for applying unknown *functions* to a
known number of *arguments*. However not all call patterns are covered by such functions. There is no pre-built
function for applying ten pointers and as such we are short of luck for applying 10 bools.

GHC will still make this work, but it will do so by building a partial application of some arguments, and
then applying this partial application to the rest of the arguments. So at runtime you can imagine it looking more like
this:

```haskell
let pap = f x1 x2 x3 x4 x5
    pap' = pap x6 x7 x8 x9
in  evaluate pap'
```

This adds up to a significant overhead, which we can completely eliminate if we *know* the arity of the function we
are about to call. Hence the large difference in performance.

# Conclusions?

It's unclear to me if it's actually worth to implement the ideas from the "Kinds are calling conventions"
at this point.

On the one hand it seems clear that for the most common cases GHC already has a way to deal with
unknown function calls in a reasonable effecient way.

However once one leaves the trodden path the performance cost of unknown calls can quickly increase.
And while functions with ten arguments might be rare even something simple like `f :: Int# -> Double# -> <result>`
already lacks a pre-built application function and hence would benefit massively from this optimization.

Additionally tracking arity in types would never come with a runtime cost. So the only thing to worry about is
a modest overhead in compile time and the complexity of the implementation.

Last but not least I assume there are other optimizations that could take advantage of this information if it
were apparent in the types. So I'm not yet discouraged from looking into this further. Although I would have
hoped for a larger benefit.