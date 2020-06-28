---
title: "Experience report: Stronger types in GHCs UniqFM"
author: Andreas Klebinger
categories: programming, haskell, ghc
---

# GHC and maps over Uniques

GHC uses a `Unique` type to identify objects. This type is an opaque wrapper over `Int`
which makes it fast and efficient. For Sets/Maps The underlying
implementation is provided by IntSet/IntMap which are well optimized.

Maps over Uniques currently work something like this:

```haskell

data UniqM a = UniqM (IntMap a)

lookup :: HasUnique key => key -> UniqM a -> a
insert :: HasUnique key => key -> a -> UniqM a -> UniqM a
toList :: UniqM a -> [(Unique,a)]
...

lookup key (UniqM m) = IM.lookup (keyToInt key) m
...
```

This has two big drawbacks:

* It's not obvious what kind of object is actually intended to be used as key.
* It's easy to use the wrong key (or map at times!). As maps from `Foo -> a` have the
  same type as maps from `Bar -> a`.

But it has advantages:

* It's a really simple API
* It's as efficient as IntMap

However problems become appearant when one writes code like this:

```haskell
let fooMap = ...                :: UniqM Int
    kfoo   = actuallyABar       :: Bar
    
    -- This is *wrong* but will typecheck.
    thing  = lookup kfoo fooMap :: Int
```

## Inside Information

We can however include the information about the key type in the map type.
This is free at runtime! But it makes for a slightly less versatile API:

```haskell
type Unique = Int

getUnique :: HasUnique a => a -> Unique

data UniqM key a = UniqM (IntMap a)

lookup :: HasUnique key => key -> UniqM key a -> a
insert :: HasUnique key => key -> a -> UniqM key a -> UniqM key a
toList :: UniqM key a -> [(Unique,a)]
...
```

Now if we were to make the same mistake as above:

```haskell
let fooMap = ...                :: UniqM Foo Int
    kfoo   = actuallyABar       :: Bar
    thing  = lookup kfoo fooMap :: Int
```


The compiler will yell at us. Clearly looking up something with a Bar
as key in a Foo keyed map is wrong.  
The code is clearer, bugs are harder to write. It's almost all good.

## There and Back Again

What if for some reason we have code which:

* Converts a map to a list
* Performs a transformation on the elements
* Then builds a new map from the result

It's not clear why [one would do so](https://gitlab.haskell.org/ghc/ghc/-/issues/18387)
but it happens.

The code below illustrates the principle:

```haskell
let xs = toList fooMap    :: [(Unique,a)]
    xs' = map (second f) xs        :: [(Unique,a)]
    -- This won't type check as Unique != Foo.
    fooMap = fromList xs' :: UniqM Foo Int
```

This typechecked/worked with untyped keys. Clearly only being explicit
in the key doesn't make the code worse. However while the meaning might
still be the same it will no longer typecheck. In the process of converting
the map to a list we also lost the key type. This is unfortunate.

As result we need to put in extra work just to satisfy
the type checker. Or use a [loophole](https://hackage.haskell.org/package/ghc-8.10.1/docs/UniqFM.html#v:listToUFM_Directly)
by providing an API which is happy to take a `Unique` without bothering
to check the types of keys.

An alternative approach would be to make the keys themselves typed with something along the lines
of `UniqueOf a = UniqueOf Unique`. But for GHC this would be a major change with, ideally, zero gains
for users. So it's hard to argue for.

## The name of something is not it's own thing in GHC.

GHC often uses the same `Unique` for the `Name` of a thing and
the thing itself. For example variables share a unique with their names.

Both really refer to the same *thing* in a sense. But still they
are different values with different types. As consequence we have to handle lookups
in a `UniqM Var Var` where both a `Name` and a `Var` can be used as key.

Currently GHC simply has a `VarEnv` type/module which abstracts over this and
provides an API providing both. But internally we just end up using loopholes
to avoid the checking of the types to make this work.

# Conclusion

I think this change is a perfect example for the case of stronger types eliminating
certain kinds of bugs.
It also does a good job showcasing that this isn't free. We have to add loopholes to our
map API just to keep the existing code working. Or use other approaches which make it
harder to express certain constructs.

[The change](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3577) in my opinion makes
a lot of code also much clearer.

Sadly this change will also break all plugins who use these types directly. Which is really unfortunate.

However with code being read much more often than written I still think it's the right thing to do and
worth the cost.
