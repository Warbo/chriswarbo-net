---
title: Benchmarking
---

There is lots of folk-wisdom among programmers when it comes to speeding up
code. The advice I try to follow can be summed up as:

 - Favour *simplicity* over performance almost all of the time. Performance is
   rarely a problem, complexity is usually a problem. This isn't an excuse to be
   egregious: some simple approaches are better than others.
 - If performance is a problem, find the bottlenecks by *profiling*.
 - When a bottleneck is identified, write *benchmarks* to reliably measure its
   performance.
 - Try to improve the benchmark result by *incrementally* altering the code.
   Stop when there's a significant improvement, and see if the system now
   performs acceptably.

This closely mirrors testing practices, in particular regression tests. We might
also want benchmarks which run the whole system, to get a more realistic view of
performance, but we might want to keep it separate from our microbenchmarks so
that it doesn't slow down our iteration cycle. This is similar to integration
testing vs unit testing, and the same solutions can be used for both (e.g. build
servers watching for changes in our code repositories).

Whilst testing is very well supported by frameworks and tooling, benchmarking
seems to be in a worse state. The two main problems I see are:

 - Lack of *generic* benchmarking frameworks/tools.
 - Difficulty *isolating* benchmarks from interference (i.e. running on unloaded
   systems).

## Tooling ##

There are *many* tools for running benchmarks, measuring times, performing
simple statistics, etc. These are equivalent to test frameworks, like the
"xUnit" family, and likewise are usually tailored to a particular language, e.g.
[Criterion](http://www.serpentine.com/criterion/tutorial.html) for Haskell. In
principle they're all generic, since our benchmarks can invoke programs written
in other languages, but that's not the nicest idea (in the same way that we
don't tend to write tests which run other test suites).

When it comes to continuous integration (continuous benchmarking?) there seem to
be far fewer options. This sort of tooling is important for benchmarking, since
we can rarely judge benchmark results in isolation: we need to see graphs of how
they've changed over time. There are some "bare bones" generic tools, notably
[gipeda](https://github.com/nomeata/gipeda), but that still requires bespoke
scripting to run the benchmarks and store the results, and requires the user to
organise and track things like which machine was used, etc.

The best approach to running and tracking benchmarks that I've seen is
[Airspeed Velocity](http://asv.readthedocs.io/en/latest) (ASV). Out of the box
it is designed for benchmarking Python packages, but
[my Nix plugin](/projects/nixos/asv_benchmarking.html) lets
us use it for any language or system.

Each ASV benchmark is a Python function (or, if you must, a method in a class).
For non-Python projects we can use these in two ways: if we only need crude time
measurements of some script, we can use Nix to put that script in the
environment, and use ASV to time a function which invokes it. More powerfully,
we can write "tracking" benchmarks which just return numbers for ASV to track.
We can take these numbers from anywhere, e.g. from the output of some more
specialised tool like Criterion. In this case we're still stuck using Python,
but only as a thin layer more like a configuration language.

Using ASV to either perform benchmarks, or as a wrapper for some other
measurement tool, gives us some nice benefits:

 - A commandline API for selecting which benchmarks to run and on what commits
 - Isolated, sandboxed environments for each benchmarked commit
 - A permanent collection of results, stored as JSON
 - Separate tracking of results from different machines
 - Parameterised benchmarks, e.g. for testing small and large inputs
 - Parameterised environments, e.g. for testing alternative dependencies
 - HTML reports graphing results over time
 - Step detection for finding regressions

One difficulty is to ensure that we don't run multiple benchmark suites at once.
I use [Laminar](https://laminar.ohwg.net) for continuous integration, and use
Nix to define [my Laminar configuration
](/git/laminar-config). To prevent benchmarks running
concurrently, I've used
[`flock`](http://man7.org/linux/man-pages/man1/flock.1.html) in two ways:

 - Non-benchmark jobs (i.e. builds and tests) are guarded with a "read lock"
   (or "shared lock", via `flock -s`). Multiple such jobs can run concurrently.
 - Benchmark jobs are guarded with the same lock file, but as a "write lock"
   (AKA "non-shared"). Such jobs will not run concurrently with any other
   benchmark *or* any non-benchmark job.

It is important that we use `flock` on *all* jobs, not just benchmarks, to avoid
concurrency *during a benchmark*. I use git hooks to trigger each repo's
build/test Laminar job; but not for the benchmarks, since there's little point
benchmarking a project which doesn't build or whose test suite fails. Instead,
the benchmark jobs are triggered by the build/test jobs, iff the build/test
succeeds. We can do this thanks to the simplicity of Laminar, which is designed
to be easily scripted.
