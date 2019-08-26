---
title: Improving GHC's code generation - Summer of Code - Complete Report
---

Disclaimer: Initially this was a gist - So the actual date is estimated.

# Improvements to GHCs Code Generator

This is a writeup of the work done by me (Andreas Klebinger) during
my SoC 2018 Project.

## Improved code layout for GHC

The vast majority of my time was spent working on this part of the Project.
While the patch was not yet merged there is not much missing.

The state of the patch can be viewed on [GHCs Phab](https://phabricator.haskell.org/D4726).
Some discussion is also on the [Bug Tracker](https://ghc.haskell.org/trac/ghc/ticket/15124)

A explaination is also available on the [GHC wiki](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CodeLayout)

### Preliminary results:

Numbers are speedups, higher is better.

| Library       | Sandy Bridge (Linux) | Haswell (Linux) | Skylake (Win) |
| ------------- |------------:  | ----:             | -----: |
| aeson         | +2.6%   | +2.3%         |   +1.2%
| containers    | +1.4%   | +1.1%       |   +1.7%
| megaparsec    | +3.2%   | +13.6% 1 )  |   +8.0%
| perf-xml 2 )   | +0.2%   | NA        | +1.1%
| text          | +3.0%   | NA                |   NA
| Vector *2     | +2.5%   | +2.5%       |   +1.3%

* 1 ) Possibly exaggerated because of background load.
* 2 ) https://github.com/haskell-perf/xml

### Layout project review

#### Motivation

The idea was born out of neccesity. Some time ago I worked on adding static analysis to GHC to mark branches
as likely or unlikely to be taken. Ultimately however this showed almost no benefit on runtime since code
layout did not take advantage of this information. Even producing worse code at times.

#### Initial work

It became quickly clear that in order to improve on this GHC would need more information about control
flow available when doing code layout.

There were two ways to approach this: Attach metadata to assembly instructions or have this information
available out of band. I chose the later, building up a CFG from the CMM intermediate Language before
generating assembly from it. Then doing static analysis on that to assign edge weights which approximate control flow patterns and using these to find a good code layout.

I started with a greedy algorithm for placing blocks based on edge weights. This went surprisingly well and I did not run into major issues during the implementation. It was my favourite part of the project.

##### First Roadblock

Next I wrote the code required to construct the CFG. However soon when testing the code I hit linker errors.
After a good deal of debugging I realized that GHC adds (or removes!) basic blocks at non obvious places in the code.
This meant blocks where invisible in the CFG, hence not placed and leading to issues at link time.

Finding all the places that modify the CFG was the first unplanned part which took a larger amount of time.
But for the x64 backend the code now updates the CFG as we modify the code.

#### Benchmarks

Finally I was able to benchmark the actual performance. Which was **worse** initially.
However after experimentation it became clear that this was mostly a matter of adjusting
how we assign weights. With results improving ever since.

I initially only used nofib for benchmarks which has it's own drawbacks.

Over time I incorporated various library benchmark suits into my approach.
Which in hindsight was important as these showcased both benefits and drawbacks
which where not obvious looking only at nofib result.

However benchmarking in general took a lot of work and time.
Unexpected issues where:

* Benchmarks straight up being broken - some of the issues are listed below:
    * Nofib - Some benchmarks did not use the given compiler flags - Fixed during SoC
    * Nofib - Most of the benchmarks did not actually contribute to the reported runtime difference - Fixed during SoC
    * Vector - Various issues - Submitted a PR
    * Aeson - Broken dependency - Local workaround
    * text - benchmark not building on windows
* Issues with tooling. I've run into a few issues with cabal new-build in particular. These invalidated some results so a good bit of time was spent reporting these issues and finding workarounds.
* Computing time required - Getting a meaningful performance comparison took a lot of compute time. Especially since it was unclear which approach to assigning edge weights gives the best results. The longest ones are listed below.
    * nofib - >10 Hours.
    * containers - 4 Hours
    * text - 7 hours
* Benchmarks being broken when using a unreleased GHC version.

I ended up writing a lot of scripts (bash/R) in order to compare the results of benchmarks.
They are available [here](https://github.com/AndreasPK/bench_hc_libs) but are better used as a source of inspiration.
As they rely a lot on hardcoded paths/flags and the like.

## CMOV Support - Unfinished

Convert certain code patterns to branchless instructions.
There is a working prototype on [Phab](https://phabricator.haskell.org/D4832).

However there are still bugs with certain edge cases. Ultimately the work on this was
posponed in favour of the layout work which promised bigger benefits in more cases.

## Improvements to nofib

### Updated nofib default runtimes/settings

Driven by the need to get reliable benchmarks I reworked the default settings for the nofib
benchmark suite in order to harmonize runtimes between the different benchmarks.

This has not yet been merged but has been accepted to be merged. [Tracker](https://ghc.haskell.org/trac/ghc/ticket/15357),
[Patch](https://phabricator.haskell.org/D4989)

### QOL Patch - Ignore warnings about tabs vs spaces for nofib.

This has been merged upstream.

Nofib pretty consistently uses tabs especially in old benchmarks.
Given the nature of the code it doesn't make sense to emit warnings about this.
[Patch](https://phabricator.haskell.org/D4952)

### Bugfix - Don't search for perl binary in a hotcoded path.

Merged upstream. [Patch](https://phabricator.haskell.org/D4756)

### Bugfix - Some benchmarks ignored given compiler options if these were overriden by O2

Merged upstream. [Patch](https://phabricator.haskell.org/D4829)


## Other work

### Small documentation fixes about GHCs dump flags

Merged upstream. [Patch 1](https://phabricator.haskell.org/D4879), [Patch 2](https://phabricator.haskell.org/D4788)

### Allow users to hide most uniques when comparing Cmm code

Merged upstream. [Patch](https://phabricator.haskell.org/D4786)

This makes it easier to compare Cmm dumps in cases where the uniques are different but the code is the same.

### Small performance improvement for GHCs OrdList/Bag

Merged upstream: [Patch](https://phabricator.haskell.org/D4770)

### Use strict left folds in GHC.

Has been accepted but not yet merged. [Patch](https://phabricator.haskell.org/D4929)

### Alignment of symbols.

I wrote a patch to specify the alignment of generated functions at compile time to help rule out performance differences caused by alignment.

This work has been merged upstream.
[Tracker](https://ghc.haskell.org/trac/ghc/ticket/15148),
[Phab](https://phabricator.haskell.org/D4706)

### Eliminate conditional branches which jump to the same target on true and false.

This has been merged upstream. [Tracker](https://ghc.haskell.org/trac/ghc/ticket/15188),

### Invert float comparisons to eliminate explicit parity checks.

Improve generated code by taking advantage of the fact that certain
checks always fail on unordered floating point numbers.

The patch is complete but review and acceptance from maintainers is still required.

[Tracker](https://ghc.haskell.org/trac/ghc/ticket/15196), [Patch](https://phabricator.haskell.org/D4990)

### Contributions to the wider eco system

* [BugReport: Undocumented lib numa dependency by GHC](https://ghc.haskell.org/trac/ghc/ticket/15444)
* [Bugreport: cabal ignoring store-dir option](https://github.com/haskell/cabal/issues/5481)
* [Bugreport: cabal is broken with relative store-dir path](https://github.com/haskell/cabal/issues/5485)
* [Bugreport: cabal caches invalid store-dir path](https://github.com/haskell/cabal/issues/5504)
* [Pull request: update vector benchmarks](https://github.com/haskell/vector/pull/219)
* [Pull request: Update metadata for container benchmarks](https://github.com/haskell/containers/pull/557)
* [Bug report: Cabal has a race condition with custom store directory and new-build on windows](https://github.com/haskell/cabal/issues/5458)
* [Bug report: Undocumented/Wrong behaviour for cabal new-configure](https://github.com/haskell/cabal/issues/5457)
* [Bug report: Cabal fails with \<\<loop>> under certain conditions.](https://github.com/haskell/cabal/issues/5467)
* [Bug report: hashtables depends on outdated packages](https://github.com/gregorycollins/hashtables/issues/51)
* [Bug report: Benchmarks fails with out of memory exception](https://github.com/haskell/text/issues/225)
* Code review on other patches. [Here](https://phabricator.haskell.org/D4922) and [here.](https://phabricator.haskell.org/D4813)
