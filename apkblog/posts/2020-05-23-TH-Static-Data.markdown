---
postNumber: 157
title: Using Template Haskell to generate static data
author: Andreas Klebinger
categories: ghc, performance
postName: th-for-static-data
---

Template Haskell (TH) is a powerful tool for specializing programs
and allows shifting some work from runtime to compile time.

It can be a bit intimidating to use for beginners. So I thought I
would write up how to use TH to turn a certain kind runtime computations
into compile time computations. 

In particular we will turn the initialization of a fully static data
structure into a compile time operation.

This pattern works for many data structures but we will look
at IntSet in particular.

# A working example

As an example consider a function of this sort:

```haskell
isStaticId :: Int -> Bool
isStaticId x =
    x `elem` staticIds
  where
    staticIds = [1,2,3,5,7 :: Int]
```

<!-- more -->

We have a set of known things here represented by a list in the form of `staticIds`.

We use `Int` as it makes the example easier. But these could be Strings or all kinds of things.
In particular I was inspired by GHC's list of known builtin functions. 

## Upsides of the list representation

The advantage of the code as written above is that the list is statically known.
As a result the list will be built into the final object code as static data, and accessing it will not require any allocation/computation.

You can check this by looking at the core dump (`-ddump-simpl`).
Don't forget to enable optimizations or this might not work as expected.
In the core there should be a number of definitions like the one below. 

```haskell
-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
isStaticId3
isStaticId3 = : isStaticId8 isStaticId4
```

Each definition of this form will be compiled to static data.
It will eventually be represented as just a number of words encoding the
constructor and it's fields.

We can conform this by looking at the Cmm output where it will look like this:

```
[section ""data" . isStaticId3_closure" {
     isStaticId3_closure:
         const :_con_info;
         const isStaticId8_closure+1;
         const isStaticId4_closure+2;
         const 3;
 }]
```

I won't go into the details of how to read the `Cmm` but it shows us that the binding will end up in the data section.
`:_con_info;` tells us it's a Cons cell and then we have the actual data stored in the cell.

What is important here is that this is *static* data. The GC won't have to traverse it so having the
data around does not affect GC performance. We also don't need to compute it at runtime as it's present
in the object file in it's fully evaluated form.

## Switching to IntSet

What if we aggregate more data? If we blow up the list to 100, 1000 or more elements it's likely that
performing a linear search will be become a bottleneck for performance.

So we rewrite our function to use a Set as follows:

```
isStaticIdSet :: Int -> Bool
isStaticIdSet x =
    x `S.member` staticIds
  where
    staticIds = S.fromList [1,2,3,5,7 :: Int] :: IntSet
```

This looks perfectly fine on the surface.
Instead of having O(n) lookups we should get O(log(n)) lookups right?

## Pitfalls of runtime initialization

However what happens at runtime? In order to query the set we have to first convert
the list into a set.
This is where disaster strikes. We are no longer querying static data as the list argument
has to be converted into a list. Something that won't happen at compile time.

GHC often manages to share our created set across calls. But depending on the code in question
it might not. And we can end up paying the cost of set construction for each call to `isStaticId`.

So while we reduced the lookup cost from `O(n)` to `O(log(n))` total cost is now 
`O(n*min(n,W)+ log(n))`. `n*min(n,W)` being the cost of [constructing the set](https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-IntSet.html#v:fromList)
from a list.
We could optimize it slightly by making sure the list is sorted and has no duplicates. But this would still
be worse than the list based code we start out with.

It's a shame that GHC can't evaluate `fromList` at compile time. Or can it?

# Template Haskell (TH) to the rescue

What we really want to do is to force GHC to fully evaluate our input data to an `IntSet`.
Then ensure the `IntSet` is stored as static data just like it happens for the list in our initial example.

## How can TH help?

Template haskell allows us to specify parts of the program to compute at compile time.

So we "simply" tell it to compute the Set at compile time and are done.

Like so:

```haskell
{-# NOINLINE isStaticIdSet #-}
isStaticIdSet :: Int -> Bool
isStaticIdSet x =
    x `S.member` notSoStaticIds
  where
    notSoStaticIds = $( lift $ (S.fromList [1,2,3,5,7] :: IntSet))
```

This results in core as simple as this:

```haskell

isStaticIdSet1
isStaticIdSet1 = Tip 0# 174##

-- RHS size: {terms: 7, types: 3, coercions: 0, joins: 0/0}
isStaticIdSet
isStaticIdSet
  = \ x_a5ar ->
      case x_a5ar of { I# ww1_i5r2 -> $wmember ww1_i5r2 isStaticIdSet1 }
```

No longer will we allocate the set at compile time, instead the whole set is encoded in `isStaticIdSet1`.
We only get a single constructor because IntSet can encodes small sets as a single constructor.

### How it works 

From the outside in:

`$( .. )` is TH syntax for splicing. It evaluates a template haskell expression and *splices*
the result into our program.

The next piece of magic is `lift`. It takes a regular *haskell* expression,
evaluates it at *compile time* to a *TH* expressions which when spliced equals
the evaluated value of the haskell expression.

And then we just have regular set creation `S.fromList [1,2,3,5,7]`.

Putting those together during compilation GHC will:
* Evaluate `S.fromList [1,2,3,5,7]`.
* `lift` takes the result of this evaluation and gives us TH expression.
* Using `$( .. )` we splice the resulting TH expression into our program.
  Effectively inserting the fully evaluated expression into our program.

The resulting code will be compiled like any other, in this case resulting in fully static data.

## Full example

Now you might think this was too easy, and you are partially right.
The main issue is that lift requires a instance of the `Lift` typeclass.

Because the compiler needs a way to translate the haskell value into a TH
expression.

But for the case of IntSet we can have GHC derive one for us.
So all it costs us is slightly more boiler plate.

Sadly we need to derive the Lift instance in another module than we use it in.
The derived instances also won't work for infinite data structures like knot tied lists.

Why this is required is probably best explained by a proper TH tutorial.

Here is a full working example for you to play around with:

```haskell
-- First module
{-# LANGUAGE TemplateHaskell #-} -- ^ Enable TH

-- 
{-# LANGUAGE StandaloneDeriving #-} 
{-# LANGUAGE DeriveLift #-}

module TH_Lift  where

import Language.Haskell.TH.Syntax

import Data.IntSet.Internal

deriving instance Lift (IntSet)

---------------------------------
-- Second module
{-# LANGUAGE TemplateHaskell #-}

module M (isStaticIdSet) where

import TH_Lift
import Data.IntSet as S
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
type Id = Int

isStaticIdSet :: Int -> Bool
isStaticIdSet x =
    x `S.member` staticSet
  where
    staticSet = $( lift $ (S.fromList [1,2,3,5,7] :: IntSet))
```

## Why do we require two modules.

We translate `lift $ (S.fromList [1,2,3,5,7] :: IntSet)` into a TH expression at
compile time. For this GHC will call the (already compiled) lift method of the `Lift` instance.

However if we define `isStaticIdSet` and the `Lift` instance in the same module
GHC can't call `lift` as it's not yet compiled by the time we need it.

In practice most packages have companions which already offer `Lift` instances.
For example `th-lift-instances` offers instances for the Container package.

## Disclaimer: This won't work for all data types.

For many data types the result of `lift` will be an expression that
can be compiled to static data as long as the contents are known.

This is in particular true for "simple" ADT's like the ones used by IntSet
or Set.

However certain primitves like arrays can't be allocated at compile time.
This sadly means this trick won't work for Arrays or Vectors.

There is a ticket about lifting this restriction on arrays on [ghc's issue
tracker.](https://gitlab.haskell.org/ghc/ghc/issues/16944).

So hopefully this will work for arrays at some point in the future.

## Haven't I read this before somewhere?

This was initially published on to the [Well Typed blog](http://www.well-typed.com/blog/2020/06/th-for-static-data/).
In the process of publishing it there parts of it have been much improved by contributions from Well-Typed.
