---
title: Opinion piece on GHC backends.
tags: Haskell, GHC, LLVM
lang: en-GB
---

You might have used or heard of the different GHC [backends](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/codegens.html)

There is a never ending discussion about using llvm vs the default backend.  
So here is a quick recap of what we are talking about and my opinions on the matter.

## What is a GHC backend

GHC can produce an executable in multiple ways.
In general the pipeline is equivalent up to GHC's intermediate Cmm
representation. From there we produce object code (or executables) via
one of three backends:

* The C backend: Produces C and then use a C compiler.
* The NCG (native code generator) backend: Produces assembly directly.
* The llvm backend: Produces llvm code which is then compiled using llvm

### The C backend

Is in general cumbersome and slow.  
It's used if you want to bootstrap GHC to a new platform and similar scenarios but not used commonly.

### The NCG backend

This is the default backend.

* It's a part of GHC itself
* It's written in Haskell

### The LLVM backend

This can be enabled via `-fllvm`.

* It depends on the llvm toolchain
* There is a thin conversion layer from Cmm to llvm ir written in Haskell
  that is part of GHC.
* Then we pass the result to llvm which itself is written in C++

## LLVM vs NCG

The main questions for GHC (and users) is really should we use the
NCG by default or use LLVM?

On the face of it the decision seems trivial. LLVM has more people working on it so that should be
free work we can use. So why is it not the default?

On the technical side llvm is quite impressive and a lot of both engineering and research
went into it.

However LLVM was not written for Haskell in particular, and lazy functional languages in
general. This means there is a certain friction between the code GHC wants to generate,
and the code LLVM expects to receive.

On the other side the NCG is a product of limited resources and it shows.
But it's 100% tuned for generating Haskell code, which allows it to do a good enough job
most of the time.

### Pick your poison: Nofib Numbers

For a start here are the current numbers:

| Relative difference | NCG  | -fllvm |
|-----------------:|------|-------:|
| Compile Time (%) | baseline    | +22.7% |
|     Run Time (%) | baseline    |  -4.5% |

These are taken by using the same ghc build based on commit `6e5dfcd2886d7523cfa059a64b343b22c5da4e97`
from early August.  
Then running nofib twice, once using `-fllvm` and once without.

They were run on linux using an i7-4790K.

`-fllvm` certainly compiles quite a bit slower. Meanwhile ncg is somewhat slower at runtime.

It's not a huge difference **on average** for runtime.
But there are absolutely a few individual benchmarks where llvm stomps ncg performance.

So for the most part this is a compile time/performance tradeoff. So pick your poison.

### Pro NCG? LLVM makes assumptions which cause friction for GHC

The two major hurdles here are [tables next to code](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects)
and the fact that GHC uses two stacks.

LLVM does not support tables next to code well despite being discussed as far back as [7 years ago](http://lists.llvm.org/pipermail/llvm-dev/2012-February/047555.html).  
The reason for this is the combination of GHC not being an important user of LLVM, and few GHC devs being interested in writing patches for LLVM.  
So instead GHC uses workarounds. For example we split methods into multiple methods (proc-point splitting) just so we can have tables next to code.  
This is bad for all kinds of reasons.

Somewhat related is the fact that GHC uses two stacks, one being heap allocated. Which is also something LLVM was not designed for
and a result llvm does "struggle" with the resulting code.

The last point of friction comes in the form of compile time. Some of this is lack of optimization for the llvm
backend on GHC's side. Some of it is that LLVM does at least duplicate some of the work GHC performs in the
shared Cmm pipeline. Some of it is just llvm doing so much more work.

That being said it's not clear to what degree this is unavoidable because of the nature of Haskell code.
And to what degree this could be solved with redesigns for parts of GHC.

But from my perspective even just figuring out if LLVM is a suitable long term solution requires a
large upfront investment of resources. Something hard to do for a Project like GHC which has limited corporate sponsorship.

### Pro LLVM? The NCG falls short in certain areas.

It's no secret that GHC's default backend falls short in certain areas:

* There is no SIMD support currently.  
* Register allocation would benefit from applying some modern research (or just engineering really).
* There is no peephole pass.
* Strength reduction for division sometimes falls short.
* And the list goes on.

While this sounds quite bad it's good enough for many applications. And even the features which are sorely
missing would not be hard to implement. There is simply little investment in these parts of GHC.

The whole NCG backend received less than 1k commits since **2006**. The result is that many of the pain
points could be reasonably solved even with a fairly limited investment.

In the NCG as a whole there is still a lot of low hanging fruit where someone could take a paper or even just a good idea,
combine it with a few weeks/months of time and make GHC produced code faster by a percent or two.

So I think most of these issues could be solved with less effort than required to make LLVM work well.
But overall still a reason for using llvm.

### Pro LLVM: The LLVM backend is here to stay

The NCG will never beat LLVM on things like register allocation or instruction selection (except maybe by accident).  
This is especially true when it comes to targeting a specific CPU.

GHC will likely never be best in class that those things, so users depending on them will either use LLVM or FFI.
This is ok, but it means the LLVM backend is unlikely to ever go away completely. So why don't just use it at all times?

### Pro NCG: Community issues

"We should use llvm!" is common notion even among ghc devs.

However in practice few people seem interested in actually working on it.
This seems odd at first but makes sense since it requires either knowledge
of both systems, or the willingness to dive into the workings of llvm.

Few people are willing to look into code generation issues to begin with,
and even fewer are willing to look at issues involving llvm.

This leads to issues not being fixed like [this](https://gitlab.haskell.org/ghc/ghc/issues/14251)
which has been broken for two major releases.
Or [-fllvm being broken](https://gitlab.haskell.org/ghc/ghc/issues/16354) in certain situations.

## My Opinion: Pro LLVM - Eventually

I think NCG being the default *right now* is a good thing.  
But no project lives forever, and neither will the NCG backend.

With ever more resources flowing into llvm, eventually taking advantage of this
will be too good of an offer to pass up.  
Even if it requires large compromises on GHCs side.

But I still expect the NCG to be the default for at *least* a few more years.

I've heard the sentiment that time spent on the NCG is wasted a few times by now.
But I strongly disagree. After all any improvement to the NCG will still save countless
cycles before a transition to llvm could happen.

And who knows, it might never happen at all.  
After all GHC is just another project, and could be superseded by something else before than.