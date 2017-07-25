---
title: Benchmark Anything with Nix and Airspeed Velocity
---

[Airspeed velocity](https://asv.readthedocs.io) (asv) is a benchmarking
framework originally created for Python packages. Using Nix, we can extend it to
support anything we like.

## Using ASV ##

To benchmark a project with asv, we write a set of benchmarks in Python. These
can just be standalone functions, or you can get fancy with classes, etc. if you
want. Special handling exists for benchmarking time taken (CPU or wall-clock) or
memory usage (output size or maximum resident), but benchmarks can also just
return some arbitrary number (say, lines of code, or number of FIXMEs, or
whatever).

Benchmark results are parameterised by two things: the version of the project
being benchmarked (typically this is a git commit), and the dependencies used.

The former allows the benchmarks to live in a separate repo to the project, it
ensures that we can benchmark historic versions (i.e. from before the benchmarks
were written), and guarantees that every version is being compared on the same
benchmark. None of these would be possible if the benchmarks were run directly
from the repo under test.

The latter allows fundamentally different configurations to be tracked
separately; for example, if we use a fast C library when available, and fall
back to a slow interpreted version if not, then we don't want to conflate the
numbers from these two scenarios.

Benchmark results are written to disk as JSON, where they can be version
controlled if desired, and static HTML reports can be generated.

So far so good, but officially asv only supports Python packages and
dependencies.

## Using Nix in ASV ##

Internally, asv runs benchmarks in an 'environment'; typically constructed using
`virtualenv` or `conda`. This is why asv doesn't support non-Python projects out
of the box.

Since I was after a robust benchmarking tool, but I don't use Python, I've
written [asv-nix](/git/asv-nix). This is a plugin for asv which uses Nix
instead of `virtualenv` or `conda`. Since Nix is a general purpose package
manager, we can set up environments for any sort of package rather than just
Python.

The required dependencies can be found in the `shell.nix` file of `asv-nix`.
With these available we can generate a default config file using:

    asv quickstart

To use `asv-nix` we set the following options in the resulting `asv.conf.json`:

    "plugins":          [ "asv_nix" ],
    "environment_type": "nix"

Note the underscore in the plugin name (Python complains about hyphens...)

Now we can use Nix expressions to define our dependencies, although since asv is
written with Python in mind we have to perform a little indirection compared to
the official documentation. The idea is as follows:

 - The `installer` property is (a string containing) a Nix expression which
   takes in some arguments and outputs a package (derivation). This package
   should provide a `python` executable, which will be used to run the
   benchmarks. The arguments include all dependencies (see below), and `root`
   which is the path to the checked-out project version (which we can `import`
   things from).
 - The `builders` property is an object. Each key names a dependency, and the
   values are (strings of) Nix expressions for building those dependencies. Each
   builder expression is given a `dir` argument, which is the path to the
   benchmarks being run (so we can `import` stuff). Note that this is not the
   checked-out project; it's wherever you're running the benchmarks from. There
   is also a `version` argument, which lets you pass in arbitrary values.
 - The `matrix` property specifies which versions of each dependency to use,
   just like in a normal asv project. Each key is the name of a dependency
   (matching those in `builders`) and each value is a list of "versions". Under
   `asv-nix`, each "version" is a (string containing a) Nix expression. These
   values will be passed as the `version` argument to the corresponding builder.

## Examples ##

### Configuration ###

If there's only one configuration for our project, a config like the following
will import `bench-env.nix` from the checked-out repo to provide the environment
(i.e. the package which includes `python`):

    "installer": "args: import ''${args.root}/bench-env.nix''",
    "builders":  {},
    "matrix":    {}

Of course, this requires the project to define a working environment in every
commit we want to benchmark. Instead, we may prefer to define the environment
alongside the benchmarks, and import it from there. That way, every commit will
get an environment built in the same way.

To do this, we need to make two changes to `installer`:

 - Take in the benchmarks path (the `dir` argument in `builders`) as an
   argument. We do this by making a dependency whose builder simply passes along
   this directory as is.
 - Pass the checked-out repo (the `root` argument) as an argument to the
   environment builder, so the result can include that version of the project in
   the environment. For simplicity we can just pass in the whole of `args`:

```
    "installer": "args: import ''${args.dep}/bench-env.nix'' args",
    "builders" : { "dep": "given: given.dir" },
    "matrix"   : { "dep": [ "null" ]         }
```

Notice that the dependency `dep` now appears in the `args` of `installer`. The
builder for `dep` just plucks `dir` out of its arguments. Since we don't need to
distinguish between different "versions" of `dep`, we just define a single
version with the uninformative value `null`. This will appear as `given.version`
in the builder, but we ignore it.

Note that I've used a variety of names here, to ease comprehension. Since each
expression has its own scope, we can actually re-use names if we like:

    "installer": "args: import ''${args.dir}/bench-env.nix'' args",
    "builders" : { "dir": "args: args.dir" },
    "matrix"   : { "dir": [ "null" ]       }

We can also write arbitrary Nix code in place of these definitions, but it's
generally cleaner to keep these in a separate file and use `import`.

### Nix expressions ###

Here's an example `bench-env.nix` that we might put in our project repo for the
first version:

    with import <nixpkgs> {};
    runCommand "benchmark-env"
      {
        # Use default Python package from nixpkgs
        inherit python;

        # Lets us override a command's environment
        buildInputs = [ makeWrapper ];

        # The actual project we want to include, assuming we have a default.nix
        ourProject  = import ./.;

        # We can include arbitrary data via environment variables
        someVal = import ./data.nix;
      }
      ''
        mkdir -p "$out/bin"

        # Add ourProject to PATH when python is run, and set SOME_VAR to someVal
        makeWrapper "$python/bin/python" "$out/bin/python" \
          --prefix PATH : "$ourProject/bin" \
          --set SOME_VAR "$someVal"
      ''

With this environment, our benchmarks can access `someVal` via
`os.getenv("SOME_VAR")`, and any binaries defined by our project will be in
`PATH`.

If use the second config, then `bench-env.nix` will need to take an argument and
use it to look up `ourProject`:

    with import <nixpkgs> {};
    args: runCommand "benchmark-env"
      {
        # Use default Python package from nixpkgs
        inherit python;

        # Lets us override a command's environment
        buildInputs = [ makeWrapper ];

        # The actual project we want to include, assuming we have a default.nix
        ourProject  = import args.root;

        # We can include arbitrary data via environment variables
        someVal = import "${args.root}/data.nix";
      }
      ''
        mkdir -p "$out/bin"

        # Add ourProject to PATH when python is run, and set SOME_VAR to someVal
        makeWrapper "$python/bin/python" "$out/bin/python" \
          --prefix PATH : "$ourProject/bin" \
          --set SOME_VAR "$someVal"
      ''

### Benchmarks ###

With all of this in place, we can use asv as per the official documentation. For
example, here's a benchmark which parses `someVal` as an integer and returns it
(this could be some arbitrary metric, gathered however we like):

```python
import os
def track_stuff()
  return int(os.getenv('SOME_VAR'))
track_stuff.unit = "lightyears"
```

The `track_` prefix tells asv that this is a generic, numeric benchmark. `unit`
is an arbitrary label for the results.

Here's a benchmark which times the `ourBinary` program:

```python
import subprocess
import timeit

def time_prog():
  subprocess.call(['ourBinary', 'some', 'tasty', 'arguments'])
time_prog.timer = timeit.default_timer
```

Note that we use `timeit.default_timer` to measure wall-clock time; otherwise,
asv would measure the CPU time of the Python process, which won't include the
time spent waiting for `ourBinary`.

As with all asv benchmarks, we have the full power of Python at our disposal. By
including Nix, it's also easy to integrate data from non-Python sources, across
project versions, as long as we can make a number available to Python:

```python
import json
import subprocess

def setup_cache():
  with open('result.json', 'w') as f:
    f.write(subprocess.check_output(['runTests']))

def track_passes():
  with open('result.json', 'r') as f:
    return json.loads(f.read()).passed
track_passes.unit = "tests"

def track_failures():
  with open('result.json', 'w') as f:
    return json.loads(f.read()).failed
track_passes.unit = "tests"
```

## More Information ##

The `asv-nix` repository provides more information and examples, in its `README`
and test suite. For general usage information, see asv's own documentation.
