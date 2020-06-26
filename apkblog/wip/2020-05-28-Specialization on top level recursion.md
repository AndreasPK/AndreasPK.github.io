---
title: 2020-02-24 GHC Internals - Specialization on top level recursion
tags: GHC, Optimization
---

# TLDR: Top level recursive can prevent specialization.

Recursive functions often fail to specialize.
Functions recursive at the top level almost always do.

You can avoid this by:
* Adding INLINEABLE pragmas (sensible).
* Writing them such that recursion is pushed in a local
  `go` function. (sensible)
* Using `-fexpose-all-unfoldings`. But this comes at a large
  compile time cost.

With that out of the way:

## Motivating example: `mapAccumLM` from GHCs codebase.

```haskell
-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')
```

Semantically this seems fine. We map over a list of elements updating the state as we go
and get a monad result out at the end.

It's convenient that it's polymorphic, so convenient that it's used with about ten different Monads
throughout GHC'S codebase.

## Where is the catch

While looking at Core output for parts of GHC itself I saw a call to `GHC.Utils.Monad.$wmapAccumLM <args...>`.

This seems harmless at a glance but it's not. The name tells us we call the generic worker for
mapAccumLM contained in GHC.Utils.Monad.

Here is the workers type signature: 

```haskell
GHC.Utils.Monad.$wmapAccumLM :: forall {m :: * -> *} {acc} {x} {y}.
     (forall a b. m a -> (a -> m b) -> m b)
     -> (forall a. a -> m a)
     -> (acc -> x -> m (acc, y))
     -> acc
     -> [x]
     -> m (acc, [y])
```

The last three arguments are just what we expect. But it seems to take
two additional arguments.

What happend here is that GHC tried to compile an efficient yet still
polymorphic version of mapAccumLM. For efficiency reasons this results
in the worker taking a decomposed Monad dictionary (as a result of the constraint)
expecting the two used functions `return` and `>>=` as arguments.

The call to this worker comes from a use site like this:

```haskell
res <- mapAccumLM mappingFunction instr  rsSpillRead :: SpillM _
```

By taking the individual `Monad` methods as arguments we at least avoid
decomposing the Monad dictionary on each invocation. But while this improves
things slightly we already know what these methods will be for `SpillM` so why
can't GHC just inline those or specialize for the given Monad?

The reason is a bit involved.

## Detour: Specialization

What is specialization?

Consider the list version of elem 

```haskell
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys
```

Once compiled it will not only take an element and a list to work through. It will also take a *dictionary* argument which contains the methods to compare two `a`s for equality (or difference).

This is required to be polymorphic so far so good.

Now we could define a monomorphic function by using the polymorphic elem.

```haskell
charElem :: Char -> [Char] -> Bool
charElem = L.elem
```

And the naive expectation is that this should be the same as writing:

```haskell
charElem :: Char -> Char -> Bool
charElem _ []       = False
charElem x (y:ys)   = x==y || elem x ys
```

That is we want to *specialize* elem for the type `Char -> [Char] -> Bool`.

I recently ranted about this not happening for `mapAccumLM` to someone and
it turned out the ways how this can happen are not obvious. Let's go over it.

## "Specialization" via inlining

Sadly this can't happen for `elem` for reasons we will go into later.
Instead we will think about these functions instead:

```haskell
isHead :: Eq a => a -> [a] -> Bool
isHead x xs = x == (head xs)

isHeadChar :: Char -> String -> Bool
isHeadChar x xs = isHead x xs
```

Inlining happens in core, so for this to make sense we will need to know how this
translates to core. I will just give the core in Haskell syntax for simplicity here.

```haskell
isHead :: EqDict a -> [a] -> Bool
isHead eqDict x xs = x `(eq eqDict)` (head xs)

isHeadChar :: Char -> String -> Bool
isHeadChar x xs = isHead eqDictChar x xs
```

Where `eq` is the field selector for the comparison function, not the comparison
function itself. (In actual core the selector is `==`). GHC inserts the dictionary
for Char as argument to `isHead` automatically when it resolves the constraints.

For specialization via inlining we look at isHeadChar and start inlining:

```haskell
isHeadChar x xs = isHead eqDictChar x xs
-- inline isHead
isHeadChar x xs = (\eqDict x xs -> x `(eq eqDict)` (head xs)) eqDictChar x xs
-- inline lambda arguments.
isHeadChar x xs = x (eq eqDictChar) (head xs)
-- inline eqDictChar
isHeadChar x xs = x `eqChar` head xs
```

GHC will also inline head, but at this point the result is just what we would have
gotten had we written isHeadChar explicitly as monomorphic function.

For non-recursive functions this can always happen if the functions are small or
have INLINE pragmas. There is very little magic involved in this kind of specialization.


We saw above it's fairly straight forward to specialize via inlining. But why can't we always do this?

## Recursion and inlining

Consider the following function:

`repeat x = let xs = repeat x in (x:xs)`

So what if we would inline this at some use site:

```haskell
foo x = repeat x
=>
foo x = x : repeat x
=>
foo x = x : x : repeat x
=> 
...
```

How should we decide to stop inlining? Once could decide based on the size of the function
we inline into, or some other more complex metric. What GHC does however is quite simple.
It detects that repeat would lead to a inlining loop because it is recursive and ... just never inlines it.  
Ever.

Now if we never ever inline it why should compiling a module even make the definition of repeat visible
to other modules? After all it would just add compilation overhead to export this information.

So a compiler should probably never even expose the definition of this function to use sites.
After all this just adds overhead at compile time. And that's exactly what GHC does most of the time.

But there is a small issue with this. For polymorphic functions this means we can't specialize them.
After all specializing them involves duplicating the definition one way or another, which requires visibility of the functions definition!

Now GHC developers know this. For this reason GHC tries to decide if exporting the definition would be useful
and exports it if so even if it's not inlined. However it's hard to predict this in general.

Exporting all functions is also often unrealistic for large projects so GHC is somewhat conservative with
exposing the definition to use sites.

Let's recap what we learned so far:
* We can specialize non-recursive functions by inlining them.
* GHC won't inline recursive functions.

So how does GHC manage to specialize recursive functions?

### Call pattern specialization

This form of specialization allows us to specialize *recursive* functions by creation a copy
of the original definition which replaces a dynamic argument with a statically known one.

See also the paper [Call-pattern specialisation for Haskell programs](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/spec-constr.pdf).

Going back to the start that is what we want to happen for `mapAccumLM`. At the core level for various use sites
we statically know which dictionary is passed to `mapAccumLM`. It can't be specialized by inlining as it's recursive so call pattern specialization is the next best option.

As a rule of thumb if we have function and it:
* Takes a constraint
* Is recursive
* Makes it's unfolding available

Then GHC can specialize it to remove the overhead of the constraints.

In the case of `mapAccumLM` the issue was that the unfolding wasn't made available.

## Solutions

The most obvious one is to mark recursive functions as `INLINEABLE` manually.
This forces GHC to keep the definition of the binding around which makes sure that specialization
works as expected.

There is also a brute force approach with the option to compile the defining Module using `-fexpose-all-unfoldings`
which as I understand it behaves like adding INLINEABLE to *every* binding in the module.

The third option is to use local recursion like this:

```haskell
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM f s xs =
  go s xs
  where
    go s [] = return (s, [])
    go s (x:xs) = do
      (s1, x')  <- f s x
      (s2, xs') <- go s1 xs
      return    (s2, x' : xs')

```

Sidenote: It doesn't matter if `go` captures `f` or not for the purpose of specialization.

This way GHC decides mapAccumLM could reasonably be inlined at the call site (as at it's top level it's not recursive).
During inlining we would simply copy the local polymorphic go function into the use site as well.

There it can then be specialized similar to how we would specialize top level recursion when using
an INLINEABLE pragma.

Using local go functions can have other subtle impacts on code generation as well but that's another topic
all in itself.

## Fixing the issue at the source



