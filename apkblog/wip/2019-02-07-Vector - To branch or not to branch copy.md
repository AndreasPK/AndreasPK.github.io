---
title: To branch or not to branch - case study on Data.Vector bounds checks.
tags: Haskell, GHC, vector, Assembly, optimization
---

This is a short writeup about that time when I thought about replacing the range checks in the vector package
with branchless code.

Disclaimer: I've only looked at a micro benchmark and for individual applications result could differ.
So if anyone feels like trying to set up a realistic benchmark suite to test the impact go for it.

This post will touch on:
* Branch prediction/Pipelining
* Assembly
* Unboxed types
* Modern CPU Architecture
* Tradeoffs of branchless code

Rudimentary knowledge of the first two should be enough to follow along.  
I try to keep things simple but [it's not always easy](https://en.wikipedia.org/wiki/Curse_of_knowledge).

# What is branchless code.

If you are interested in performance or compilers you will likely have heard of branchless code.

The idea is simple, whenever we jump (branch) on a condition in code the cpu will guess if the jump
is taken or not. Guessing wrong means a missprediction, pipeline stall and performance loss.

Sometimes however we can replace the code with different code which calculates the same result
without using any conditional jumps. We call that branchless code.

# Our example

Consider code like this:  
```Haskell
outOfBounds :: Int -> Int -> Bool
outOfBounds x bound = x < 0 || x > bound
```

We check if x < 0, if so we jump to the block returning False.  
Then we check if x > bound jumping again to False if true,
otherwise we continue and return true.

We can implement the same check as branchless code if we rely on two's compliment like this:  
```Haskell
outOfBounds :: Int -> Int -> Bool
outOfBounds x bound = asWord x <= asWord bound
```

* If x is between 0 and bound then we get true as expected.
* If x is greater than the bound the comparison is False.
* If x is negative then by treating it as unsigned it wraps around into
  the range between `maxBound :: Int` (exclusive) and `maxBound :: Word` (inclusive).  
  Which has to be bigger than bound which can't exceed `maxBound :: Int`.

Which brings us to the obvious question:

# Is the branchless variant actually better?

## Usecase considerations

We said above the main purpose of branchless code is avoiding misspredictions.  
However it's always safe to predict that the value will fall into the bounds. 
We don't really care about the case where it doesn't fall into the bounds either as that will lead to termination of the program.
Something for which performance hardly matters.

But given how simple the branchless code looks we will check the performance anyway.
So let's start by looking at the generated code:

## At the assembly level

The branchy version looks like this:

```asm
_c920:
	testq %rsi,%rsi # x < 0
	jl _c91Z        
_c91Y:
	cmpq %r14,%rsi  # x > bound
	jg _c91Z
_c927:
	movl $GHC.Types.True_closure+2,%ebx
	jmp *(%rbp)
_c91Z:
	movl $GHC.Types.False_closure+1,%ebx
	jmp *(%rbp)
```

The branchless version like this:
```
_c92F:
	cmpq %r14,%rsi
	setbe %al
	movzbl %al,%eax
	shlq $3,%rax
	movq GHC.Types.Bool_closure_tbl(%rax),%rbx
	jmp *(%rbp)
```

## Performance analysis

If we just count at the instructions required then both execute 6 instructions.

Benchmarks are mostly inconclusive, when mapping our range check over long lists
the branchy version wins but even so only by ~1%.

We can look at the size of the assembly, either using objdump or by pasting assembly into [this site](https://defuse.ca/online-x86-assembler.htm#disassembly).

The branchless code is a few byte smaller












Most users are probably aware of the sized Int/Word variants from `Data.Int` and `Data.Word`.  
However there are some pitfulls associated using them in places where we could use regular Int.

We will take a look at the [Int ones](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Int.html#t:Int8)
but this will apply for sized Word types as well.

# Why I consider the sized variants harmful

## Pitfall #1: They take up at least as much space as Int does

One might be tempted to change some code to Int8, expecting this to safe memory.
However if we look at the actual implementation it becomes clear Int8/Int take up
the same amount of memory.

```Haskell
------------------------------------------------------------------------
-- type Int8
------------------------------------------------------------------------

-- Int8 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data {-# CTYPE "HsInt8" #-} Int8 = I8# Int#
-- ^ 8-bit signed integer type
```

Int# stands for unboxed Integer values with 32 or 64bit depending on the machine.  
So each Int8 takes two words. One for the constructor and one for the actual value. Same as regular Int.

## Pitfall #2: They generate worse code

We know Int8 for example is backed by a full machine word. But we want to maintain overflows like we would expect in C.
So when we generate code we have to zero out unused parts of memory for each intermediate result.

This often boils down to inserting an extra instruction (or a special mov) for each intermediate value.
This is not horrible, but it does add up.

## Pitfall #3: Missing Rules

GHC has many rules to optimize common constructs, replacing them with more efficient implementations.
For example there is a more efficient implementation for `[0..n]` when we use Int.  
There are no equivalent rules for the sized variants so these can perform a lot worse.

There is a [ticket](https://ghc.haskell.org/trac/ghc/ticket/15185) about the issue as well.
## Drawback #4: Int64 in particular can be very slow on 32bit.

One would expect certain functions to be translated into only a handfull of assembly instructions.  
However on 32bit systems the 64 bit primitive operations are implemented as function calls, with according overhead.

Here is a [GHC Ticket](https://ghc.haskell.org/trac/ghc/ticket/5444) about the issue.

# When is it ok to use these then?

* They can be helpful to represent FFI APIs as they map nicely to `char`, `short`, ...
* If you need the overflow/value range behaviour of these types they are also a valid choice.
* If you work with unboxed data structures. Like these provided by Data.Vector.  
  They are backed by a bytearray so in theses cases the small variants actually take less space.

# How bad can it be?

As a showcase you can take the code below and run it.

I've defined IT once as `Int8` and once as `Int` and the runtime difference is about 30% on my machine.
I did however NOT check how much of that comes from missing rules and how much from the overhead of zeroing.

```Haskell

import GHC.Exts
import Data.Int
import System.Environment

type IT = Int

foo :: IT -> IT -> IT -> IT
foo x y z = x + y + z

times :: Int -> [IT] -> [IT]
times n xs = concat $ replicate n xs

main = do
  print $ sum $ map (\(x,y,z) -> foo x y z) [(x,y,z) | x <- 10 `times` [0..127]
                                            , y <- 10 `times` [0..127]
                                            , z <- 1  `times` [0..127]]
```