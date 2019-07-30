---
title: 2020-02-24 GHC Internals - Dynamic Pointer Tagging
tags: GHC, CodeGen, Optimization
---

# This post will go over the what and why of pointer tagging

This summer I (Andreas Klebinger) am working for Well-Typed on improving GHC, focusing on runtime performance.  
One of the topics I am looking at is pointer tagging. In preparation of another post describing
my work this post gives a rough overview of the current implementation.

Note: There is also a proper paper describing the implementation SPJ: [Faster Laziness Using Dynamic Pointer Tagging](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/ptr-tagging.pdf)  
Which is generally still accurate.

So if you want *all* the details read the paper instead.  
But if you are happy with general overview keep reading.

First we need to cover some background.

## Heap objects

*Heap objects* are things we generate at runtime like unevaluated [thunks](https://wiki.haskell.org/Thunk), evaluated (boxed) values and partial applications.
In GHC these objects share a [common layout in memory](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects).

The same layout is also used for [static objects](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects#static-objects) which are generated
at compile time and stored in the final executable file instead of the heap.

In general values stored as heap objects are called boxed values, since we do not only store the value itself in the heap
but also bookkeeping information - its **box** if you will.

Since static objects are no different than heap objects as far as pointer tagging is concerned
we will refer to both as heap objects for the remainder of this post.

# Heap objects and laziness

Laziness is a major reason why we are often forced to represent things like Ints as heap objects.
Since Haskell is [lazy](https://wiki.haskell.org/Lazy_evaluation) heap objects allow us to represent both
regular values and **unevaluated** values (thunks) in a uniform way.  

Depending on which camp you are in laziness might be Haskell's greatest strength or weakness. No matter where you stand on this issue
one fact is that it has a major impact on the code GHC generates.

## Overheads of lazy evaluation

In strict languages when passing arguments or returning results we either receive a value directly in a register or on the stack
like it is the case in C. Or we might implicitly pass a pointer to an object containing the value like Java does when we have objects as arguments.

However in Haskell any argument with a lifted type like `Bool` could represent a suspended computation.
As a consequence the argument will be allocated on the heap before the call. Then when calling the function
we pass a reference to the heap allocated object.  
Meanwhile in a strict langue we could simple evaluate the argument first and pass `0` or `1` in a register.  

Since passing the argument by reference, and the callee having to dereference (and potentially evaluate!) the argument
before being able to use it is a fair bit of overhead authors of functional languages soon came up with ways to mitigate
this effect.

### Side note - Eliminating the overhead completely

In GHC's case a lot effort went into ensuring we only pay the price for this overhead when we have to.
*Demand Analysis* checks if arguments can be treated just like we would in a strict language. If so
transformations like *worker wrapper* and *constructed product result* very often eliminate the overhead
of laziness/heap allocation for these arguments completely.

If you want to dive deeper into the details you will have to dive into the papers or the source.

* ["Demand analysis"](http://research.microsoft.com/en-us/um/people/simonpj/papers/demand-anal/demand.ps) describes how to determine if arguments are strict.
* ["The Worker/Wrapper Transformation"](https://wiki.haskell.org/Worker_wrapper) explains how worker wrapper works.
* ["Constructed Product Result Analysis for Haskell"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/cpr.pdf) explains a way to avoid allocating a heap object when returning values.

## Impact of laziness on generated code

For the rest of this post we will ignore the presence of optimizations mentioned above.
This allows us to focus on an easier example. However the principle demonstrated still applies
to many cases where the above optimizations don't apply.

### Working example

We will work through this based on this `not` function:

```haskell

not x = 
  case x of {
    False -> True;
    True -> False
  }

```

### Contrasting strict and lazy variants

In a strict language every call site of `not` would be required to evaluate the arguments first, then pass them to the function.
This means the called function can always assume all arguments are already evaluated. For our `not` function in a *strict* language
this would leave us with code that:

1. Compares the argument against `False`
2. Creates a return value based on the comparison
3. Return the result

However in Haskell `x` might be unevaluated (a thunk) so there is more work to do as we have to check for
this fact and first evaluate the thunk if required.

The logic looks roughly like this:

1. Check if argument is evaluated
  1. If not evaluate argument
2. Extract value from argument closure and continue
3. Compare the argument against `False`
4. Create a return value based on the result of the comparison
5. Return the result

In pseudocode this would look something like this.

```C++

class Bool
{
    private:
      /* ClosureStuff */
      ...
    public:
      bool evaluated;
      bool value;
      Bool* evaluate();
      ...
  
};

Bool* not(Bool* x) {
  //Deal with unevaluated arguments.
  if(!x->isEvaluated)
  {
    x = x->evaluate();
  }

  //Perform the actual work.
  if (x->value == true) {
    return new Bool(false);
  } else {
    return new Bool(true);
  }
}
```

This is reasonable, there is no way around having code which performs some form
of `x->evaluate()` in a lazy language. 
We also don't want to evaluate `x` twice so checking if the argument is already 
evaluated is also the right thing to do.

And in fact this corresponds very closely to what GHC did in the past.

In this case the overhead of having a potentially lazy argument is quite significant.
GHC has a lot of tricks to avoid situations like this but some overhead will remain.

# Reducing the overhead with dynamic pointer tagging

We can reduce the overhead due to laziness by taking advantage of an important invariant in
GHC's runtime: **All heap objects start at addresses aligned to the machine word size.**

This allows GHC to use *pointer tagging* to reduce the overhead further.

## What is pointer tagging

Since all heap objects are aligned to word boundaries all pointers to heap objects point
to addresses which are multiples of 8 on 64bit systems.  
Aligning objects on the word boundary then means all pointers will look like "...000" with the last three bits always being zero.

For example we might place two 8-byte sized objects in memory like this:

```
             ╔════╗ Word boundary 
0x1000 +---> ║0x00║ <- Start of first object            
             ║0x00║             
             ║0x00║             
             ║0x00║             
             ║0x00║             
             ║0x00║             
             ║0x00║             
             ║0x00║ <- End of first object 
             ╠════╣ Word boundary  
0x1008 +---> ║0xFF║ <- Start of second object             
             ║0xFF║             
             ║0xFF║             
             ║0xFF║             
             ║0xFF║             
             ║0xFF║             
             ║0xFF║             
             ║0xFF║ <- End of second object
             ╚════╜ Word boundary  
```

The last three bits in any pointer to a heap object are always expected to be zero when we access the object.  
This means we are free to store information in these bits when passing the pointer around.
We only have to remember to zero them when dereferencing the pointer.

Using irrelevant parts of a pointer like this is called *pointer tagging* in GHC. But the
concept is [not exclusive to GHC](https://en.wikipedia.org/wiki/Tagged_pointer).

## Making use of tagged pointers.

Having available 3 bits of storage isn't a lot, but still a lot more than zero.

GHC uses these tag bits to store information about the object a pointer points to:

* A value of zero indicates absence of information.
* When the object is a function the tag bits contain the function's arity
* When the object is a constructor the tag bits contain the constructor's tag.

Depending on a values type a constructors tag is either a 1-based index for the constructor used (e.g. 1 for `Nothing`, 2 for `Just`)
if we can encode all constructors of the type. Otherwise we fall back to 1 simply representing an evaluated value.

Building on this we could generate code taking advantage of this which corresponds
something like this in pseudo C++:

```C++

Bool* not(Bool* x) {
  //Deal with unevaluated arguments.
  int tag = x & 0x7; //Extract tag of the object.
  if(tag == 0) //Check if object already evaluated.
  {
    x = x->evaluate();
  }

  //Perform the actual work.
  if (x & 0x7 == 1) { // Use the tag to infer 
                      // what constructor we are dealing with.
    return new Bool(false);
  } else {
    return new Bool(true);
  }
}
```

This is quite close to actual code generated by GHC. 

We can compare this to the Cmm code GHC produces by passing `-ddump-cmm` and compiling the example above.
Below is the code output for these interested. I did rename some of the labels and added comments to 
make it easier to follow.  
GHC's intermediate representation also uses gotos instead of nice `if`/`else` branches and the stack management is explicit but the actual logic is the same.


```C
     {offset
       c1bH: //Check if enough stack space is available.
           if ((Sp + -8) < SpLim) (likely: False) goto doGc; else goto enoughStack;
      
       doGc: //If we ran out of stack space run GC and rerun the current function.
           R2 = R2;
           R1 = foo_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;

       enoughStack:
           //Here we set up things required for "`evaluate()`", why here and not inside `evaluate`
           //is rather complicated. See also ticket #8905
           I64[Sp - 8] = alreadyEvaluated; //Push return address for evaluate onto stack.
           R1 = R2;
           Sp = Sp - 8;
           //Check if the tag is zero/the argument is evaluated
           if (R1 & 7 != 0) goto alreadyEvaluated; else goto evaluate;
       evaluate:
           //essentially x->evaluate(), returns the result in R1
           call (I64[R1])(R1) returns to alreadyEvaluated, args: 8, res: 8, upd: 8;
       alreadyEvaluated:
           // check if argument == True
           if (R1 & 7 != 1) goto returnFalse; else goto returnTrue;
       returnFalse:
           R1 = False_closure+1; //Add tag to the pointer!
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       returnTrue:
           R1 = True_closure+2; //Add tag to the pointer!
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
     }

```

## How good is it?

If you look closely you might notice that we never dereferenced the argument if it was already evaluated!
This is important because memory access can take (comparatively) very long and pointer tagging allows us to
get away without ever touching x in memory if it's already evaluated and we don't need to access it's fields.

To better understand just **how** much better this is consider the following snippet,
which lists all the instructions executed for our `not` function if we assume the argument was already evaluated and False:

```asm
foo_info:
_c1bH:
        leaq -8(%rbp),%rax
        cmpq %r15,%rax
        jb _c1bI
_c1bJ: 
        movq $block_c1bA_info,-8(%rbp)
        movq %r14,%rbx
        addq $-8,%rbp
        testb $7,%bl
        jne _c1bA
_c1bA:
        andl $7,%ebx
        cmpq $1,%rbx
        jne _c1bF
_c1bE:
        movl $True_closure+2,%ebx
        addq $8,%rbp
        jmp *(%rbp)
```

Let's hand wave a lot and be **very** pessimistic.   
We assume the CPU needs one cycle for each of these instructions.
Then we round up and say the whole thing will take 15 cycles to run.

Meanwhile if we need to fetch `x` from e.g. L3 cache (not even memory!) it can take up to **[42 cycles](https://www.7-cpu.com/cpu/Skylake.html)** until we get the result back.  
But at this point all we did was load `x` and we haven't even done the actual work!

The actual impact will vary a lot depending on the used soft/hardware.
But in practice back when pointer tagging was first introduced the average runtime decreased by 12-14% depending on the hardware used.
So it has quite an substantial impact on the performance of Haskell programs.

# Recap

Laziness can introduce various overheads when implemented naively.
However in practice GHC does a good job reducing this overhead via various tricks.

Pointer tagging is one of these tricks which improves performance in two main ways:

* Avoiding dereferencing pointers when determining if it's been already evaluated.
* For certain data types, encode (parts of) the value of an object in the pointer to the object.

Together these two aspects improve cache hit rates significantly, and with that
runtime performance.

This works very well, however sometimes leaves us with a small overhead in
cases where we know (parts of) an argument will be in WHNF already. Reducing this
overhead is part of my summer project with Well-Typed and will be the topic
of another post.
