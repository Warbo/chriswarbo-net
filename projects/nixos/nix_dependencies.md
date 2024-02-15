---
title: Dependency solving in Nix
---

I've [written a lot about the Nix build system](index.html) over the past
decade, but one complaint I keep coming across is that Nix requires dependency
constraints to be "solved manually".

This is a sort of category-error: sure, it's true that Nix doesn't "solve
dependency constraints"; in the same way that, say, `/usr/bin/env` doesn't
"solve dependency constraints". That's not what it's for! Yet that doesn't stop
us using *actual* dependency solvers, if we want to.

In this post I want to show:

 - *Why* Nix doesn't perform constraint-solving
 - Why you shouldn't *want* it to perform constraint-solving
 - Why this is a red herring, since it's easy to have Nix call out to *any*
   dependency solver (or indeed any program at all)

**NOTE:** If you just want to wrap the build process of some existing project in
Nix then this post is not for you (just parse a lock-file like everyone else)!
On the other hand, if you want to create your own package manager (e.g. for a
new programming language), then you are *absolutely* my target audience, and I
hope you'll think about the issues I bring up (rather than copying the mistakes
of Maven/NPM/Cabal/etc. *again*)!

## Nix Basics ##

Nix calls itself a "purely functional package manager", but that terminology can
lead to an incorrect, and overly-complicated, mental model of what Nix is, e.g.
it doesn't do any sort of name resolution, it has no concept of "versions",
"constraints" or indeed "packages" (so it's no wonder that Nix won't solve
package version constraints)!

I prefer to describe Nix as a tool to **GET SPECIFIED OUTPUTS**, where:

 - "Output" just means "file or directory"; e.g. a JSON file, or a directory
   containing a Firefox executable, or an OCI container image, etc.
 - We can "specify" an output either with a hash of its contents, or as the
   result of running some arbitrary executable. The latter type of specification
   is called a "derivation".
 - Nix will "get" an output by looking in various caches. If an output specified
   with a derivation is not found, Nix will run the executable (and cache its
   result).
    - If an output specified by hash is not found, Nix gives up: there's no way
      to re-create a file/folder from just its hash!

This is the *underlying model* of Nix, which I describe in more detail in
[Nix from the bottom up](bottom_up.html). Nix is much more of a *build system*
(like Make) rather than a *package manager* (like APT).

This model is too cumbersome to interact with directly; instead, we use various
shell commands and higher-level languages (similar to how we use the `git` CLI,
rather than writing trees and blobs by hand). The most common/popular interfaces
for Nix are the original Nix CLI (`nix-build`, `nix-store`, etc.) and the "new"
`nix` CLI (technically still 'experimental', but pretty stable). For both of
those CLIs, we specify what we want using the
[Nix Expression Language (or "Nixlang")](glossary.html). Many of the code
examples in this post are written in Nixlang.

## Building Up By Example ##

First we'll quickly run through some basic Nix examples. Keep in mind that these
are lower level and more verbose than the sort of real Nixlang code people write
day to day. They're also performing overly-simplistic tasks, so we can focus on
what Nix is doing; rather than e.g. the finer details of POSIX shell signal
trapping, or whatever.

### Plain Text ###

Let's start with a simple example of using Nixlang to specify an output. In this
case the output will be a file containing the text `hello world`. We can do this
using the function `builtins.toFile`{.nix}, which takes a filename and the
file's content (function calls in Nixlang look like `f x`{.nix}):

```nix
builtins.toFile "example.txt" "hello world"
```

Entering this expression in a `nix repl` session gives the path of the output;
and we can check that it really contains the specified text:

```sh
$ nix repl
Welcome to Nix 2.9.1. Type :? for help.

nix-repl> builtins.toFile "example.txt" "hello world"
"/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt"
nix-repl> :quit
$ cat "/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt"
hello world
```

This example is rather silly, since it would be easier to just write that text
file ourselves; but it shows off a little of how Nix works. In this case, a few
things happened:

 - A hash of the name and content was calculated
 - A file was created in the Nix "store" (the `/nix/store` folder), with a
   filename containing both the hash and the name
 - The content was written to that file
 - The file's path was returned as a string

The Nix store acts like a local cache: it contains outputs (files and
directories), which can be looked-up using a hash (the filename). This simple
trick (putting hashes in filenames) makes caching trivial, reproducible and
verifiable; *unlike* systems which use timestamps (e.g. Make) or manually-chosen
"version numbers" (e.g. Maven).

### Splicing Outputs ###

Nixlang becomes useful when we start *calculating* the contents of our outputs,
rather than using fixed strings. One way to do this is "string interpolation",
which is a feature common to many programming languages. In Nixlang, we use the
syntax `${}` inside a string to embed or "splice" an arbitrary Nixlang
expression into the string. We can use this to write the *path* of one output
into the *contents* of another output. Let's use this to write a shell script,
which prints the contents of the output above:

```nix
nix-repl> builtins.toFile "sayHello" ''
  set -eux
  echo 'BEGIN'
  cat ${builtins.toFile "example.txt" "hello world"}
  echo 'END'
''
"/nix/store/5ckbk1rfqaa2zcpk2lnllmgh38drd7m4-sayHello"

nix-repl> :quit
```

```sh
$ bash "/nix/store/5ckbk1rfqaa2zcpk2lnllmgh38drd7m4-sayHello"
+ echo BEGIN
BEGIN
+ cat /nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt
hello world+ echo END
END
```

Notice that the output `/nix/store/5ckbk1rfqaa2zcpk2lnllmgh38drd7m4-sayHello`
contains the path `/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt`. So
`/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt` is a "dependency" of
`/nix/store/5ckbk1rfqaa2zcpk2lnllmgh38drd7m4-sayHello`. This seems trivial, but
it is remarkably powerful. In particular:

 - Dependencies are identified by file path, and looked-up/resolved using the
   filesystem. This is decentralised; unlike e.g. PyPI, NPM, Maven, Debian, etc.
   which must consult separate "repositories".
 - There are no "versions" or "constraints" to solve: only one output has that
   path (assuming no hash collisions). This reduces supply-chain attacks,
   maintenance burden, coordination issues, etc.

Remember that we can splice *arbitrary Nix expressions*, so this approach to
dependencies isn't limited to known, "static" outputs. Indeed, most Nixlang code
consists of *functions*, which take various dependencies as arguments and return
an output with their paths spliced in as appropriate. Using functions makes it
trivial for dependencies to be replaced/overridden/updated/etc. (just call it
with different arguments!). See
[my Nixlang glossary](glossary.html) for (opinionated) advice on making your
Nixlang definitions flexible, whilst keeping them simple.

### Reading Files ###

Nixlang can read the contents of a file, using the `builtins.readFile`{.nix}
function. This also works when the file is an output! For example:

```nix
nix-repl> builtins.readFile (builtins.toFile "x" "hello")
"hello"
```

We can use this to alter our previous script to read its message at "eval time",
rather than when the script is executed (hence we also change `cat` to `echo`):

```nix
nix-repl> with {
  # Define a local variable, to make things a bit more readable
  messageFile = builtins.toFile "example.txt" "hello world";
}; builtins.toFile "sayHello" ''
  set -eux
  echo 'BEGIN'
  echo ${builtins.readFile messageFile}
  echo 'END'
''
"/nix/store/f8ay7ki4ivkih5zyhp8pj158lslc4mx2-sayHello"

nix-repl> :quit
```

```sh
$ bash "/nix/store/f8ay7ki4ivkih5zyhp8pj158lslc4mx2-sayHello"
+ echo BEGIN
BEGIN
+ echo hello world
hello world
+ echo END
END
```

Again, this is rather silly (we could just write the string directly), but the
*principle* is very powerful: we can *calculate* an output, *read in that data*,
and use that to calculate *another* output!

Note that this also works for the `import`{.nix} function, which reads a file at
a given path (or, if it's a directory, reads a `default.nix` file in that
directory), and *evaluates the contents as a Nixlang expression*. For example:

```nix
nix-repl> import (builtins.toFile "foo.nix" "1 + 2")
3
```

### Our First Derivation ###

So far we've seen outputs specified by hash, either with contents written
verbatim, or calculated as a Nixlang string. Now we'll specify an output using
a *derivation* instead. Nixlang represents a derivation as an attrset
("attribute set"; think JSON object), containing certain attributes (name/value
pairs). We can write the attrset for a derivation directly, but
[it's pretty tedious](bottom_up.html#derivations-in-the-nix-expression-language)
so we can instead call the `builtins.derivation` function to handle the
boilerplate for us:

```nix
nix-repl> builtins.derivation {
  name = "example";
  builder = "/bin/sh";
  system = builtins.currentSystem;
  args = [ "-c" "echo RUNNING >&2 && echo $message > $out" ];
  message = "hello";
}
«derivation /nix/store/1jbxryvmz276lik7hdf3yjs0kx7kqp7x-example.drv»
```

Derivations specify which command to run (`/bin/sh`, in this case), which
arguments to give it (here the strings `-c` and
`echo RUNNING >&2 && echo $message > $out`) and which
environment variables to set (here we set one called `message`; by default the
`builtins.derivation` function will also set another environment variable called
`out`, whose value will be the output path of that derivation). We get the
outputs specified in a derivation by "building" it (again, checking caches
first). The `nix repl` interface can do this, via its `:b` command, but I'll use
the `nix-build` command instead, since it's a bit more verbose:

```sh
$ nix-build /nix/store/1jbxryvmz276lik7hdf3yjs0kx7kqp7x-example.drv
this derivation will be built:
  /nix/store/1jbxryvmz276lik7hdf3yjs0kx7kqp7x-example.drv
building '/nix/store/1jbxryvmz276lik7hdf3yjs0kx7kqp7x-example.drv'...
RUNNING
/nix/store/xcb4crw14wl8mn31lqidrkagpn5qpwdc-example
$ cat /nix/store/xcb4crw14wl8mn31lqidrkagpn5qpwdc-example
hello
```

`nix-build` prints the output paths to its stdout (in this case just the last
line; the rest are from stderr). Dumping the contents of that path shows us that
it contains our desired `message`!

For a more thorough explanation of what's going on underneath, see
[Nix from the bottom up](bottom_up.html). In practice, nobody specifies the
contents of `.drv` files in such a low-level way like this! Instead we use
helper functions, like those provided by Nixpkgs (a git repository containing
many Nixlang definitions).

### Using Nixpkgs ###

[Nixpkgs](https://github.com/nixos/nixpkgs) is a git repo, so we can use
Nixlang's `builtins.fetchGit`{.nix} function to (surprise surprise) fetch a
git repository. Specifically, it outputs a directory containing a specified
commit, without a `.git` folder. However, in the specific case of Nixpkgs this
can be a bad idea, since it has *so many* commits that fetching the metadata can
be very slow. Instead, GitHub.com provides tarballs containing just the desired
commit; which we can fetch using (surprise surprise)
`builtins.fetchTarball`{.nix}. Since HTTP lacks the cryptographic validation
provided by git, we also provide a SHA256 hash of the expected content:

```nix
nix-repl> builtins.fetchTarball {
  name = "nixpkgs2311";
  url = "https://github.com/nixos/nixpkgs/archive/057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
}
"/nix/store/b492a6w85dja0nrzvcp8g48drypzzqp8-nixpkgs2311"
```

This returns the output path of that tarball's contents (extracted into the Nix
store). To access the definitions from that path we can use `import`:

``` nix
nix-repl> import (builtins.fetchTarball {
  name = "nixpkgs2311";
  url = "https://github.com/nixos/nixpkgs/archive/057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
})
«lambda @ /nix/store/b492a6w85dja0nrzvcp8g48drypzzqp8-nixpkgs2311/pkgs/top-level/impure.nix:14:1»
```

The result is a *function* (AKA a `lambda`), to let us specify details like the
desired system architecture (e.g. if we're on a Mac but want to specify Linux
packages), which licenses to allow, etc. We can call this function with an empty
attribute set `{}` to use its default values, however some of those defaults are
"impure", since they depend on environment variables, the contents of our home
directory, etc. which can vary over time and between different machines. We want
reproducible results so we'll provide our own values for the `config` and
`overlays` attributes (whose defaults depend on `$HOME`).

**WARNING:** This function returns an attribute set with tens of thousands of
values, so it's unwise to try printing the whole thing! Instead, we'll just pick
out, say, the Bash derivation:

```nix
nix-repl> (import (builtins.fetchTarball {
  name = "nixpkgs2311";
  url = "https://github.com/nixos/nixpkgs/archive/057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
}) { config = {}; overlays = []; }).bash
«derivation /nix/store/4zvy3cs2j91bvm7vr4q13dnq90n2la85-bash-5.2-p15.drv»
```

### `runCommand`{.nix} ###

`runCommand`{.nix} is one of the most useful functions provided by Nixpkgs. It's
actually quite high-level, implemented in terms of other helper functions like
`stdenv.mkDerivation`{.nix}, but we'll skip those details here (feel free to
play with it in `nix repl`, and open its definition in your editor!)

`runCommand`{.nix} takes three arguments:

 - A name, which will appear in the output's path.
 - A set of environment variables. The output path will be appended as an `out`
   environment variable.
 - A string of Bash code to execute.

The result is a derivation which uses a Bash executable to run the given code.
In fact, it will use that `bash`{.nix} attribute we printed; which is more
reliable and reproducible than our clumsy `/bin/sh` derivations from previous
sections! This also allows us to override with a different shell, by putting
things in that `overlays` argument; but that's a topic for another day! Of
course, many people don't like Bash or shell scripting; that's fine, and Nix
doesn't prioritise any particular language (it just executes a binary, which can
be any programming language interpreter you wish). However, we'll be sticking to
Bash, since the Nixpkgs project provides many helpers that make it convenient.

Unlike our previous examples, which gave our code directly to the shell
interpreter, `runCommand`{.nix} actually executes a setup script to make our
life easier, then `eval`s our code. For example, the setup script will look for
an env var called `buildInputs` and, if present, append its contents to the
`PATH` env var (separated by `:` and suffixed with `/bin`); which makes it easy
to run other programs from our Bash code.

Here's a version of our "hello world" example using `runCommand`{.nix}, which
will *run* our Bash code rather than merely defining it. We'll write the message
to the derivation's output (whose path is available in the `$out` env var):

```nix
with rec {
  nixpkgs-src = builtins.fetchTarball {
    name = "nixpkgs2311";
    url = "https://github.com/nixos/nixpkgs/archive/057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
    sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  };

  nixpkgs = import nixpkgs-src { config = {}; overlays = []; };
};
nixpkgs.runCommand "example.txt"
  {
    buildInputs = [ nixpkgs.coreutils ];
    messageFile = builtins.toFile "example.txt" "hello world";
  }
  ''
    echo 'BEGIN'
    cat "$messageFile" > "$out"
    echo 'END'
  ''
```

If we save this to a file like `hello.nix`, we can get its outputs using
`nix-build`:

```sh
$ nix-build hello.nix
building '/nix/store/pgmibqwbivj3aj8dakpzac7hbk1gdbp0-example.txt.drv'...
BEGIN
END
/nix/store/hhki7lqk48xdcw7ynq7sya5yv11a0gd3-example.txt
$ cat /nix/store/hhki7lqk48xdcw7ynq7sya5yv11a0gd3-example.txt
hello world
```

Again, this is a very convoluted way to just output a static piece of text; but
this example can now be adapted to run *any program* on *any input*!

## Solving dependencies in Nix: Haskell example ##

**Note:** The code examples in this section aren't standalone; if you want to
run them, copy them one after another into a single `default.nix` file.

We now have all the pieces needed to resolve dependencies and solve version
constraints for projects that use legacy tooling. In particular we can fetch
metadata for resolving and identifying artifacts (using `fetchGit`,
`fetchTarball`, etc.); we can use derivations (e.g. via `runCommand`) to invoke
the legacy tools to solve the dependency constraints; and we can read those
solutions (via `builtins.readFile`{.nix} or `import`{.nix}) to define a
project's build.

This example is based on
[the `callCabal2nixWithPlan` function](https://github.com/Warbo/nix-helpers/blob/master/helpers/callCabal2nixWithPlan/default.nix)
from [my nix-helpers repo](/git/nix-helpers), although I've removed a bunch of
abstraction to simplify the presentation (for advice on writing *maintainable*
Nix code you can read
[my (opinionated) Nixlang glossary](glossary.html))!

These definitions have evolved over the years, but trace their origins back to
[around 2015](https://github.com/Warbo/nix-config/blob/9883680d593bef355d3499c5c67fb9f9369e88b2/imports/nixFromCabal.nix)
when my job required writing, integrating and maintaining a bunch of custom
Haskell projects. For those unfamiliar with Haskell, there is an online
repository of Haskell "packages" called Hackage, each identified by two
author-provided strings called the "name" and "version". Hackage packages
provide a "`.cabal` file" with instructions for
[the Cabal build tool](https://www.haskell.org/cabal), including the name and
version of this package; and the names and versions of packages it depends on.
Version strings tend to be written in a pseudo-numerical style, which allows a
package to depend on a "range" of acceptable versions, e.g.
`pandoc-types >=1.20 && < 2`.

Every package name+version combination on Hackage provides a tarball of source
code, and Hackage appends all of the `.cabal` files to an ever-growing "index"
tarball. There are other nuances, including details of
[package metadata](https://www.extrema.is/blog/2022/03/02/hackage-metadata) and
the use of [TUF signing
keys](https://www.well-typed.com/blog/2015/04/improving-hackage-security), but
the overall approach is similar to many other legacy tools (Maven, NPM, Gradle,
PyPI, etc.) so I'm guessing most readers have encountered something like this
before.

For this example we'll need a Haskell project to build; I'm going to use my
[Panpipe](/git/panpipe) project, since it's pretty simple but has non-trivial
dependencies. We'll ignore the fact that its repo already includes perfectly
good Nix build instructions!

Let's start by defining the source code we want to build:

```nix
with rec {
  panpipeSrc = builtins.fetchGit {
    url = "http://chriswarbo.net/git/panpipe.git";
    ref = "master";
    rev = "19e37791ff36a37117b5715cfd5f1b11471bdfbf";
  };

  panpipeCabal = "${panpipeSrc}/panpipe.cabal";
```

`panpipeCabal`{.nix} is a direct reference to the Cabal build instructions in
that repo, containing the dependency names+versions.

We can break down the problem of *reproducibly* building this source code by
working backwards, identifying the sub-problems we need to solve to reach each
step:

 - We ultimately want to compile the project's code. Nixpkgs provides helper
   functions like `callCabal2nix` which will define derivations to do this,
   as long as we have all of the dependencies and they're compiled.
 - Compiling each dependency can also be done with `callCabal2nix`; but of
   course that requires *their* dependencies, and so on transitively. The main
   difficulty is knowing which Hackage packages (and versions) to compile.
 - Cabal can solve the inter-dependencies between Hackage packages, to produce
   an "install plan" of mutually-compatible packages, where no package depends
   on something outside the set, and no two packages have the same name. To
   produce a plan, Cabal needs a project's `.cabal` file (like `panpipeCabal`
   above) and a Hackage index tarball.
 - Getting a Hackage index is the trickiest part of this. Cabal can fetch a
   tarball from `hackage.haskell.org`, but that's always changing so isn't
   reproducible. Thankfully it turns out that all of the same information is
   also available in a git repository called
   [all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes), so
   we can check out a specific commit of that instead.

In the following sub-sections we'll work our way up this list. The result will
be a fully-reproducible build of Panpipe, using Cabal to choose the dependencies
rather than doing so "manually".

<details class="odd">
<summary>**In defence of Cabal...**</summary>

The workflow I'm describing here was originally based around Cabal "sandboxes",
pre-dates Hackage's TUF security infrastructure, and used the old `00-index`
rather than the new `01-index`. I've updated it to work with the 2024 tools,
but in the mean time the Haskell packaging situation has been improving,
especially since Cabal introduced "Nix-style" builds. Cabal can also now
selectively-ignore parts of an index which are timestamped after a given moment,
to (hopefully, but not verifiably) reproduce the same behaviour even with an
updated index. These go some way to making builds more predictable and reliable,
but it still requires some trust that the algorithm is working as intended.

Since Hackage now only appends to its index, we could try to make a reproducible
index by only downloading its initial segment (ignoring any updates that appear
later in the file). Indeed I've done that in the past, but it's more ergonomic
to ask for a git commit (easily found, even historically), rather than a
file offset. Annoyingly this still required some post-processing, since when I
last tried it Hackage's HTTP server didn't support byte ranges in requests. In
any case I'm also wary of derivations which rely on the behaviour of some
particular HTTP server; especially when we can use something distributed and
verifiable like git!

</details>

### Getting a Hackage index, reproducibly ###

In order for Cabal to solve Panpipe's dependency constraints, it needs to know
what packages are available on Hackage (at least, at a particular moment in
time). That's what the Hackage "index" tarball is for; but the one offered by
hackage.haskell.org keeps changing, which makes builds unreproducible.
Thankfully there is an alternative source for the same information called
[all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes), which
is maintained as a git repository. This is preferable, since we can use commit
IDs to fetch any historical version and we can verify that the contents are what
we expect. GitHub can provide a `.tar.gz` of any commit, which avoids us having
to download all of the repo metadata (which is *a lot*, for this repo!). What's
more, Nixpkgs already provides such a tarball of `all-cabal-hashes` we can use!

```nix
  nixpkgs =
    with rec {
      rev = "057f9aecfb71c4437d2b27d3323df7f93c010b7e";
      sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
      src = builtins.fetchTarball {
        inherit sha256;
        name = "nixpkgs2311";
        url = "https://github.com/nixos/nixpkgs/archive/" + rev + ".tar.gz";
      };
    };
    import src { config = {}; overlays = []; };

  # Bring some useful definitions from nixpkgs into scope
  inherit (nixpkgs) all-cabal-hashes haskellPackages lib runCommand writeScript;
```

Unfortunately this `all-cabal-hashes` tarball is in *slightly* the wrong format
for use as our Hackage index. We'll need to fix the following issues:

 - Cabal will fail if an index tarball contains anything other than files, but
   `all-cabal-hashes` also contains directories.
 - All of the paths in `all-cabal-hashes` are prefixed with an extra component,
   so we need to rename entries like
   `all-cabal-hashes-xxxxxxxxx/3dmodels/0.3.0/3dmodels.cabal` to just
   `3dmodels/0.3.0/3dmodels.cabal`.
 - `all-cabal-hashes` contains metadata for each package name+version in a JSON
   file like `3dmodels/0.3.0/3dmodels.json`, but Cabal expects a file called
   `3dmodels/0.3.0/package.json` with the metadata in a different structure.

Each of these changes is pretty straightforward to make, but I want to avoid
naïvely extracting, transforming then re-archiving the files, since that would
use an unnecessary amount of disk space and cause a lot of filesystem churn.
I've instead written a small script called `mkHackageIndex`{.nix} which reads an
`all-cabal-hashes` tarball from stdin and streams a Hackage index tarball to
stdout. The details aren't important, so I've hidden them in an expandable
section below.

We can use `runCommand`{.nix} to run `mkHackageIndex`{.nix} on
`all-cabal-hashes`{.nix}, to make an `index`{.nix} tarball that Cabal will
accept:

```nix
  index =
    with {
      # We should include the git revision of all-cabal-hashes in the filename
      # of this index tarball, so it's easier to tell them apart as we update
      # our dependencies in the future.
      rev = lib.removePrefix "all-cabal-hashes-" all-cabal-hashes.name;
    };
    runCommand "01-index-${rev}" { }
      ''< ${all-cabal-hashes} gunzip | ${mkHackageIndex} | gzip > "$out"'';
```

<details class="odd">
<summary>**Implementing `mkHackageIndex`{.nix}...**</summary>

The `mkHackageIndex`{.nix} script uses `writeScript`, which is similar to
`builtins.toFile` except it results in a derivation. That's useful, since
derivations can have dependencies (their "inputs"), and hence we can splice the
paths of other derivation outputs into the file contents:

```nix
  mkHackageIndex = writeScript "mkHackageIndex" ''
```

This script is written in Haskell, so its `#!` needs the path to a `runhaskell`
executable from the [GHC project](https://www.haskell.org/ghc). Nixpkgs provides
a `ghc` derivation, however we also want our script to use some packages from
Hackage (e.g. to handle the TAr and JSON formats), so we use the helper function
`ghcWithPackages` to bundle GHC with some attributes from the `haskellPackages`
attrset (this is much simpler and more direct than using Cabal!):

``` nix
#!${
  haskellPackages.ghcWithPackages (pkgs: [
    pkgs.aeson pkgs.MissingH pkgs.tar
  ])
}/bin/runhaskell
```

The rest of the string is Haskell code. I've tried to keep it compact, and left
a few comments in-line for those curious to know what it's doing:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Archive.Tar.Entry    as Tar
import           Control.Exception          (throw)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encoding        as A
import qualified Data.Aeson.Types           as A
import           Data.Aeson                 ((.=), (.:))
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List                  (isSuffixOf)
import           Data.String                (fromString)
import           Data.String.Utils          (join, split)

-- Pipe stdio through Tar.read/Tar.write, running fixEntry on each entry
main    = LB.interact pipeTar
pipeTar = Tar.write . Tar.foldEntries fixEntry [] throw . Tar.read

-- Use these to abort the process if any error occurs
err     = either error id
tarPath = err . Tar.toTarPath False

fixEntry x xs = case (Tar.entryContent x, path) of
    -- Keep NormalFiles, but change JSON structure and path
    (Tar.NormalFile f _, _ : n : v : _) | ".json" `isSuffixOf` last path ->
      fixFile n v f : xs

    -- Keep other files as-is, but drop the leading dir from their path
    (Tar.NormalFile _ _, _) ->
      x { Tar.entryTarPath = tarPath (join "/" (tail path)) } : xs

    -- Anything other than NormalFile gets dropped (directories, etc.)
    _ -> xs
  where path = split "/" (Tar.fromTarPath (Tar.entryTarPath x))

-- Parse JSON for source metadata, then use that to write a package.json
fixFile name version bytes = Tar.fileEntry path (A.encode pkg)
  where
    -- Replace all-cabal-hashes-xxx/p/v/p.json with p/v/package.json
    path = tarPath (name ++ "/" ++ version ++ "/package.json")
    location = concat [ "<repo>/package/", name, "-", version, ".tar.gz"]
    existing = err (A.eitherDecode bytes)
    -- Parse required metadata from existing .json entry
    (size :: Int, md5 :: String, sha :: String) =
      err . ($ existing) . A.parseEither $ \obj -> do
        hashes <- obj   .: "package-hashes"
        (,,) <$>  obj   .: "package-size"
             <*> hashes .: "MD5"
             <*> hashes .: "SHA256"
    -- Construct a new .json entry from the existing metadata
    pkg = A.object
      [ "signatures" .= ([] :: [Int])
      , "signed"     .= A.object
        [ "_type"   .= ("Targets" :: String)
        , "expires" .= A.Null
        , "version" .= (0         :: Int   )
        , "targets" .= A.object
          [ fromString location .= A.object
            [ "length" .= size
            , "hashes" .= A.object ["md5" .= md5, "sha256" .= sha]]]]]
'';
```

</details>

**Lesson:** It's better to store metadata in a versioned way, like using git,
rather than an ever-changing "latest" tarball. It's easy to generate tarballs
from git repos, but going the other way is hard.

**Lesson:** When providing the same information in different media, like a
tarball and a git repository, care should be taken to prevent minor choices
(e.g. filenames or JSON structure) from causing unnecessary incompatibilities.

### Generating an install plan for Panpipe ###

We'll use this `index` tarball to generate an "install plan", using our old
friend the `runCommand` function. We need Cabal and GHC executables in our
`PATH` env var, so we add the `ghc` and `cabal-install` attributes of Nixpkgs
to the `buildInputs` env var (those attributes are derivations, so their output
paths will be spliced into `buildInputs`, separated by spaces, and the setup
script used by `runCommand` will prepend them to `PATH` appropriately).

In order for Cabal to use our Hackage index, we need to run the `cabal update`
command (we can't just copy our index into Cabal's cache directory, since it
also relies on some extra files that store offsets into the tarball; those use a
bespoke binary format, which is easiest to just let Cabal generate by itself).
The `cabal update` command will try to access the configured repositories; and
whilst Cabal *does* support using local directories as repositories, they need
to contain source tarballs for all of the packages we want to use (so Cabal can
calculate their SHA256 hash). Since we don't yet know what packages are needed
(that's the point of generating the plan!), and we don't want to download every
version of every package from Hackage just-in-case, we'll instead trick Cabal
into thinking it's using a "secure" online repository. That way, it will use the
pre-calculated SHA256 hashes from the `package.json` entries of our `index`
tarball.

**Note:** A nicer solution would be improving Cabal to allow local repositories
containing only an index. That's definitely a good idea, but here I want to show
that we can work around such deficiencies regardless; since many other legacy
tools have similar shortcomings, and we don't want to stall our progress due to
imperfect tooling!

The approach I've used to trick Cabal is to put a custom script in our `$PATH`
called `curl`: Cabal will mistakenly assume it is
[the cURL tool](https://curl.se) and use it to perform downloads; that allows
our custom script to output the data we want Cabal to use.

This also requires Cabal to have a config file pointing at some secure
repository. The URL doesn't matter (since we're not accessing the network), but
we need to set a `key-threshold` of `0` to allow unsigned metadata (this is
easier than generating our own signing keys, etc.).

```nix
 panpipePlan = runCommand "panpipe-plan.json" {
    inherit panpipeCabal;
    buildInputs = [ nixpkgs.cabal-install nixpkgs.ghc ];
    CABAL_CONFIG = builtins.toFile "dummy-cabal.config" ''
      repository repo
        url: http://example.org/
        secure: True
        root-keys: []
        key-threshold: 0
    '';
  } ''
```

```sh
    export HOME="$PWD"

    mkdir bin
    cp ${fakeCurl} bin/curl
    PATH="$PWD/bin:$PATH"

    mkdir -p "$HOME/.cache/cabal"
    cabal update
```

After `cabal update` has cached the index, we make a copy of Panpipe's `.cabal`
file and run `cabal build` to generate an install plan (using the `--dry-run`
option to avoid actually compiling the project):

```sh
    cp "$panpipeCabal" panpipe.cabal
    cabal build --enable-test --dry-run
    mv dist-newstyle/cache/plan.json "$out"
  '';
```

We can use Nix to read this install plan, parse it as JSON and use the contents
in the rest of our definitions:

```nix
  panpipePlanJSON = builtins.fromJSON (builtins.readFile panpipePlan);
```

<details class="odd">
<summary>**Implementing `fakeCurl`{.nix}...**</summary>

The `fakeCurl` script we use to trick Cabal into using our index is relatively
straightforward. Feel free to skip this section, or keep going for the
nitty-gritty.

```nix
  fakeCurl = writeScript "fakeCurl" ''
```

We use `writeScript`, like with `mkHackageIndex`, but this time we're writing
shell code rather than Haskell. We extend `$PATH` with the `jq` tool (from
Nixpkgs), which is useful for manipulating JSON data. We also make a copy of the
first argument `$1`, which will be the URL Cabal is trying to download.

```sh
    #!/bin/sh
    set -e
    set -o pipefail
    export PATH="${nixpkgs.jq}/bin:$PATH"

    # Remember which file is being requested
    F=$(basename "$1")
```

The Hackage Security initiative requires repositories to provide a bunch of
extra metadata, in the form of JSON files. These functions will output dummy
contents for these files (to stdout):

```sh
   # Helper functions, to output the required file contents

    function sha { sha256sum - | cut -d' ' -f1; }
    function md5 {    md5sum - | cut -d' ' -f1; }

    function root {
      echo '${
```

```nix
        builtins.toJSON {
          signatures = [ ];
          signed = {
            _type = "Root";
            expires = "9999-01-01T00:00:00Z";
            keys = { };
            version = 5;
            roles = nixpkgs.lib.genAttrs [
              "mirrors"
              "root"
              "snapshot"
              "targets"
              "timestamp"
            ] (_: {
              keyids = [ ];
              threshold = 0;
            });
          };
        }
```

```sh
      }'
    }

    function mirrors {
      echo '${
```

```nix
        builtins.toJSON {
          signatures = [ ];
          signed = {
            _type = "Mirrorlist";
            expires = "9999-01-01T00:00:00Z";
            mirrors = [ ];
            version = 1;
          };
        }
```

```sh
      }'
    }

    function snapshot {
      # This can take a few seconds, to hash the index twice
      echo '${
```

```nix
        builtins.toJSON {
          signatures = [ ];
          signed = {
            _type = "Snapshot";
            expires = "9999-01-01T00:00:00Z";
            version = 1;
            meta = {
              "<repo>/01-index.tar.gz".hashes = { };
              "<repo>/mirrors.json".hashes = { };
              "<repo>/root.json".hashes = { };
            };
          };
        }
```

```sh
      }' | jq --argjson rlen "$(root | wc -c         )" \
              --argjson mlen "$(mirrors | wc -c      )" \
              --argjson zlen "$(stat -c '%s' ${index})" \
              --arg rmd5 "$(root    | md5 )" \
              --arg mmd5 "$(mirrors | md5 )" \
              --arg zmd5 "$(md5 < ${index})" \
              --arg rsha "$(root    | sha )" \
              --arg msha "$(mirrors | sha )" \
              --arg zsha "$(sha < ${index})" \
              '(.signed.meta["<repo>/root.json"      ] |= {
                 "length": $rlen,
                 "hashes": { "md5": $rmd5, "sha256": $rsha }
               }) |
               (.signed.meta["<repo>/mirrors.json"   ] |= {
                 "length": $mlen,
                 "hashes": { "md5": $mmd5, "sha256": $msha }
               }) |
               (.signed.meta["<repo>/01-index.tar.gz"] |= {
                 "length": $zlen,
                 "hashes": { "md5": $zmd5, "sha256": $zsha }
               })'
    }

    function timestamp {
      # Calculate snapshot.json once, to avoid re-hashing the index
      S=$(snapshot)
      echo '${
```

```nix
        builtins.toJSON {
          signatures = [ ];
          signed = {
            _type = "Timestamp";
            expires = "9999-01-01T00:00:00Z";
            meta."<repo>/snapshot.json".hashes = { };
            version = 1;
          };
        }
```

```sh
      }' | jq --argjson len "$(echo "$S" | wc -c)" \
              --arg     md5 "$(echo "$S" | md5  )" \
              --arg     sha "$(echo "$S" | sha  )" \
              '(.signed.meta["<repo>/snapshot.json"] |= {
                 "length": $len,
                 "hashes": { "md5": $md5, "sha256": $sha }
               })'
    }
```

Next we loop over the given arguments: the argument appearing after `--output`
is the path where Cabal expects the "downloaded" data to appear; the argument
appearing after `--dump-header` is another path Cabal expects to find, but its
contents don't matter so we just `touch` it and move on.

```sh
    # Grab the output file paths requested by Cabal, and write empty headers
    while [[ "$#" -gt 0 ]]
    do
      echo "$@" 1>&2
      case "$1" in
        --output)
          OUTPUT="$2"
          shift 2
          ;;
        --dump-header)
          touch "$2"
          shift 2
          ;;
        *)
          shift
          ;;
      esac
    done
```

We branch on the first argument (the URL, which we copied to `$F`), and write
the appropriate content (either generated JSON, or the `index` tarball itself)
to the specified `$OUTPUT` path:

```sh
    # Write the desired content to the output path Cabal is expecting
    case "$F" in
      *.json)
        case "$F" in
          *root.json) root;;
          *mirrors.json) mirrors;;
          *snapshot.json) snapshot;;
          *timestamp.json) timestamp;;
        esac > "$OUTPUT"
        ;;
      *index.tar.gz) cp ${index} "$OUTPUT";;
      *)
        echo "UNKNOWN FILE REQUESTED '$F'" 1>&2
        exit 1
        ;;
    esac
```

Finally we send a HTTP success code to stdout:

```sh
    # Finish with a "success" HTTP code
    echo 200
  '';
```

</details>

**Lesson:** Anything designed for remote access should also accept a local
alternative, in a like-for-like fashion.

**Lesson:** Provide features separately, rather than bundling them. Cabal
succeeds here by allowing dependencies to be solved without then compiling the
project. However, it fails in the case of reading SHA256 hashes from the index,
which only works for (secure) remote repositories.

**Lesson:** Be aware of which input data is actually required for each task
supported by your tool. For example, solving dependencies doesn't require source
tarballs, only their SHA256 hashes: Cabal will take these from an index when
making an install plan with a (secure) remote repository; but requires a local
repository to contain source tarballs of all relevant packages (which we don't
know, since we've not yet got an install plan).

**Lesson:** Choose file formats which are easy for other tools to generate and
consume. For example, Cabal stores tarball offsets in files using a bespoke
binary format, making it difficult to generate with other tools. Its `.cabal`
format is bespoke, although library support has lead to the development of some
helper tools. The use of JSON for additional metadata makes it easy to produce,
transform and consume.

### Defining a consistent set of inter-dependent Haskell packages ###

Nixpkgs provides a reasonably-useful subset of Hackage packages, along with some
helper functions for using them, in its `haskellPackages` attribute. We want to
override the contents of this set, to have precisely those versions specified in
the install plan we just defined. We do this via the `extend` function, which
we'll actually use twice:

```nix
  haskellPackagesForPanpipe =
    (haskellPackages.extend fixes).extend chosenVersions;
```

The `fixes` override is the most straightforward. It defines an attribute called
`testu01`, which is required to avoid an error message; and it overrides the
`mkDerivation` function to always skip test suites. The latter is important,
since Cabal allows test suites to have their own list of dependencies, so
attempting to build a package *and* its test suites in one derivation can result
in circular dependencies. For example if a Hackage package `foo` depends on
another package `bar`, and the test suite of `bar` depends on `foo`, then we
either need to compile `bar` without its tests (then compile `foo` after), or
compile everything in one big derivation (allowing us to compile `bar`, then
`foo`, then the `bar` tests; which Cabal would normally attempt). Compiling
things separately is preferable, so they can be cached and re-used; hence we
just skip all the tests.

**Note:** We could define separate derivations to compile and run the test
suites, but that would make this post *even longer*!

```nix
  fixes = self: super: {
    # A simple way to disable all test suites: we replace the 'mkDerivation'
    # function (which defines the derivations that build Haskell packages), so
    # we can intercept its arguments to always append `doCheck = false;`. This
    # avoids circular dependencies, where foo depends on bar, but bar's test
    # suite depends on foo.
    mkDerivation = args: super.mkDerivation (args // { doCheck = false; });

    # The splitmix package lists 'testu01' as a required "system dependency"
    # for one of its test suites. A "system dependency" is what Cabal calls
    # anything that's not a Haskell package, since (unlike Nix) it's incapable
    # of managing them. That means it doesn't appear in the install plan, so
    # it's not in the 'chosenVersions' set, and Nix will abort due to this
    # dependency being undefined. Since it's only needed by a test suite, and
    # we're skipping all test suites, it can safely be defined as 'null'!
    testu01 = null;
  };
```

Note that the names `self` and `super` are just function arguments: they're not
keywords, like in other languages. This is a common pattern in Nixpkgs, for
writing definitions that are easily overridable: the `extend` function will call
`fixes` with the original `haskellPackages` set as the second argument
(`super`); the first argument (`self`) will be the result of `fixes` appended to
`haskellPackages` (which is also the overall result of `extend`). This
circularity is fine, thanks to Nixlang being lazy; and makes it easy to override
dependencies deep within a nested structure.

Now we reach the `chosenVersions`, which are calculated from our install plan:

```nix
  chosenVersions = nixpkgs.haskell.lib.packageSourceOverrides
    (builtins.listToAttrs (namesToVersions
      (builtins.filter (pkg: pkg.type != "pre-existing")
        panpipePlanJSON.install-plan)));

  namesToVersions = builtins.map (pkg: {
    name = pkg.pkg-name;
    value = pkg.pkg-version;
  });
```

The function `nixpkgs.haskell.lib.packageSourceOverrides`{.nix} is exactly what
we need to turn an install plan into a set of Nix derivations. It creates an
override function, of the same form as `fixes` (i.e.
`self: super: { ... }`{.nix}), based on a given attribute set mapping names to
versions (e.g. `{ foo = "1.2"; bar = "999"; }`). A corresponding `.cabal` file
for those name+version combinations will be looked up in the `all-cabal-hashes`
revision in Nixpkgs, and [the `cabal2nix`
tool](https://github.com/NixOS/cabal2nix/tree/master/cabal2nix) will be run on
them.

The argument we're passing to `packageSourceOverrides` looks a bit complicated,
but it's just pulling names and versions out of the install plan JSON (via the
helper function `namesToVersions`) and combining them into the required
name->version mapping. One thing to note is that some Haskell packages are
bundled with the GHC compiler, and hence cannot be set to any other version;
these appear in the install plan with `"type": "pre-existing"`{.json}, which is
why those are being filtered out.

### Compiling the Panpipe project ###

Nixpkgs provides helper functions for wrapping various bespoke/legacy build
systems into normal Nix derivations. Since Panpipe uses Cabal, we'll use the
`callCabal2nix`{.nix} function: this is provided in the `haskellPackages` set,
but taking it from our overridden `haskellPackagesForPanpipe` set ensures the
correct package versions will be used as dependencies. `callCabal2nix` takes
following arguments:

 - A string for the name. This can be anything, since it's only used for the
   path in the Nix store (not Hackage name resolution). We'll use the string
   `"panpipe"`.
 - A directory of source code; we can use the `panpipeSrc`{.nix} we defined
   at the beginning.
 - An attribute set containing dependency overrides. We won't need to override
   anything, since we're already using the correct versions from
   `haskellPackagesForPanpipe`. Hence this will be an empty set `{}`.

The resulting definition looks like this:

```nix
  panpipe = haskellPackagesForPanpipe.callCabal2nix "panpipe" panpipeSrc {};
```

This is the last of our definitions, so the final piece of our `.nix` file is to
close off the attribute set we've been writing, and specify what value we want
to return. In this case it's the `panpipe` attribute we just wrote; and, as a
bonus, we'll append a bunch of our local definitions to it, so they can be
accessed from `nix repl`, or via `import`, for those curious to explore them:

```nix
};
panpipe // {
  inherit nixpkgs index panpipePlan;
  haskellPackages = haskellPackagesForPanpipe;
}
```

We can use `nix-build` to evaluate and compile this `panpipe` derivation; and if
we call our file `default.nix` then we don't even need to specify the filename:

```sh
$ nix-build
```

**WARNING:** This will take a while! The expandable section below shows some of
the more interesting parts of the output; along with a commentary on what's
happening.

<details class="odd">
<summary>**Annotated highlights of the `nix-build` output...**</summary>

Nix typically works by first evaluating Nixlang code to find the specified
derivations, then building/fetching outputs which we don't yet have. What we're
doing here is more like evaluate, build, evaluate some more, and so on. That's
why it might take some time to give some meaningful output. You may see some
lines about building the `mkHackage` script, the `index` tarball, etc.
(depending on what's already cached in your Nix store). Eventually you should
see many lines like the following:

``` nix
building '/nix/store/qb2589r0xbja8ay2c76fpgl14nnx0zx0-all-cabal-hashes-component-dlist-1.0.drv'...
all-cabal-hashes-f4111a737432472002a38495b14b930255705d6a/dlist/1.0/dlist.cabal
all-cabal-hashes-f4111a737432472002a38495b14b930255705d6a/dlist/1.0/dlist.json
building '/nix/store/40bqkw06srvmjmq0xn6pnbh874vcdj4a-cabal2nix-dlist-1.0.drv'...
```

These are due to our use of `packageSourceOverrides`: that first derivation is
extracting `.cabal` and `.json` files from the `all-cabal-hashes` tarball (in
this case for the `dlist` package version `1.0`), then the second derivation is
running the `cabal2nix` tool on that `.cabal` file to get a Nixlang definition
for that Haskell package; each definition being a function from dependencies to
a derivation (the JSON file provides a SHA256 hash, which will be used to verify
the source tarball used by that derivation). These package names and versions
have come from the install plan that was generated for Panpipe; with the
immediate dependencies of Panpipe appearing earliest, and transitive
dependencies appearing later as each definition is generated, then passed its
dependencies as arguments, which causes their definitions to be generated, and
so on.

We'll eventually have all of the required definitions, and it's time for Nix to
realise the Panpipe derivation and its dependencies. This point has been reached
when we see lines like this:

```sh
these 39 derivations will be built:
  /nix/store/6wk8hrf29iybd1nbj06k7ry3w2xnk0xa-hashable-1.4.3.0.drv
  /nix/store/5hsi842z6757swcqcm6kvhcfbp3g62fg-data-fix-0.3.2.drv
...
  /nix/store/06bh1x76mzvfy947siq2h0kkcn1qfmfs-pandoc-types-1.23.1.drv
  /nix/store/0g3is6whplwkb3m2q7lk8rvjhrq0a843-panpipe-0.4.1.0.drv
these 69 paths will be fetched (1.84 MiB download, 7.64 MiB unpacked):
  /nix/store/ymjz6canbxvrgbv3msicx4z9slzgpkik-OneTuple-0.4.1.1-r1.cabal
  /nix/store/p6f5wrczg9fyh9182cz62bp1ib1myrlq-OneTuple-0.4.1.1.tar.gz
  /nix/store/d668w8yvplc8gqn925kn4x9kxbd6d9bg-QuickCheck-2.14.3.tar.gz
...
```

These are the derivations for the individual Haskell packages, along with their
source tarballs (that `-r1.cabal` appears to be a "revised" Cabal file, which
has been edited on Hackage and stored alongside the original tarball). Notice
the very last thing to be built is Panpipe. These derivations will be compiling
Haskell code, which can take a very long time, so I'd recommend doing something
else in the mean time!

</details>

Eventually the build will finish, with the output path of our Panpipe build
printed to stdout:

```sh
/nix/store/1wm6ybh8ikdqrxbmmxr53ppz09gk2wf6-panpipe-0.4.1.0
```

If we look inside, we should find a `panpipe` executable, which we can pipe
Pandoc JSON data into:

```sh
$ printf '```{pipe="/bin/sh"}\necho sseccus | rev\n```' |
pandoc --to markdown --filter /nix/store/1wm6ybh8ikdqrxbmmxr53ppz09gk2wf6-panpipe-0.4.1.0/bin/panpipe

    success
```

If we run `nix-build` again it will just spit out the same path, since nothing
needs to be built:

```sh
$ nix-build
/nix/store/1wm6ybh8ikdqrxbmmxr53ppz09gk2wf6-panpipe-0.4.1.0
```

## Unreliable/Non-deterministic Dependencies ##

I chose a Haskell example since the `all-cabal-hashes` repository makes it
*possible* (albeit rather tedious) to resolve "Hackage packages" to specific
artifacts in a reproducible, verifiable way; i.e. by fetching all the metadata
from a known git commit. Unfortunately, many legacy tools aren't so usable;
instead relying on the contents of arbitrary HTTP responses. Fixing such
non-determinism is out of scope for this article, but one approach I've had
reasonable success with is simply *checking* the calculated dependencies, by
writing them in some standard form (say, JSON) and comparing it's SHA256 hash
against what we expect. In principle this is similar to using a lock file,
although maintaining a single hash is less effort than every individual
dependency; and it's also completely agnostic to the tool or sorts of
dependencies being checked. Whilst this will spot security and integrity
problems on the third-party infrastructure (e.g. it has saved me from Maven
repositories serving corrupted `pom.xml` files) it provides no resilience to
work around such problems, since the required data is not content-addressed and
therefore can't be fetched from trustless mirrors/caches/P2P-networks.

As an example, I've used this approach successfully at a company with many
internal, inter-dependent Scala projects. Still, some tools are better than
others: these were originally using SBT, which required lots of manual copying
of `.jar` files, checking-out of old git commits in order to overwrite the
contents of hidden cache folders in `$HOME`, and was generally a mess. We
introduced Nix to avoid these problems, but it was very difficult to make SBT
behave consistently, both over time (so building old commits would result in the
same output; not some "latest update") and even between runs (so results could
be validated against an expected hash). We gradually transitioned to Maven,
which is [much saner](https://news.ycombinator.com/item?id=37249255), and made
it easier to programatically control through Nix. Who knows, if enough people
ask for it, I may contact them to request open-sourcing their Nix code...

## Takeaways ##

I wanted to show how existing projects, defined using legacy build systems, can
still benefit from Nix, e.g. for reliability, reproducibility, verifiable supply
chains, transparent caching, decentralised distribution, etc. Going further than
just building, such projects can also utilise tools like `nix bundle` to produce
container images, RPM and Deb packages, self-contained executables, etc.
Managing a private binary cache for Nix is also easier than the various "package
repositories" companies often resort to; since they are completely agnostic
about their contents (whether it's Maven POMs, Cabal files, text files, etc.);
their contents are identified by hash (so no need for `SNAPSHOT-5` and other
such nonsense); etc.

There's still some work to do on making such workflows ergonomic, which is
mostly a deficiency of the legacy tooling and its assumptions about centralised,
always-online, trusted infrastructure. Removing such crutches would be a benefit
even for those not looking to wrap them in Nix! Still, I would only recommend
going down a path like this if your legacy tooling has got you into dependency
hell (like my previous employer was with private SBT packages). Otherwise, I'd
still recommend Nix; but via the usual "lock file" approach.

For those building and maintaining such "package managers", I hope the examples
above have highlighted some functionality that is useful to provide, and some
pitfalls to avoid. For example, git repositories are a more secure and
reproducible way to distribute metadata than some centralised HTTP server; and
benefit from being decentralised, usable offline and locally, and are easily
forked for those with more peculiar requirements.

Lessons specific to Cabal include the utility of local index files (which we had
to shoe-horn in via a fake `curl`); the annoyance of bespoke, hard-to-produce
data formats (like cache files which can only be made by running `cabal update`,
due to deviations from the usual `binary` package); and missed opportunities for
alignment (like having the layout of `all-cabal-hashes` and `01-index` match).

There are also a few areas where Cabal shines compared to other tools: it
creates "install plan" files of readable JSON, whilst for many other tools we're
left guessing what they chose by inspecting what appeared in their cache.
Cabal's strict separation between "update" and "build" also ensures that the
latter can be performed offline; something that's notoriously difficult with
e.g. most Java tooling (which usually feature an `--offline` option, but it's
usually ignored by the assortment of plugins required to build a typical
project)!

I think the increasing adoption of Nix in recent years will incentivise tools to
make these improvements; either to appease those attempting to run those tools
via Nix, like in this post; or by using Nix as an inspiration, like with Cabal's
"Nix-style builds". The situation's already much better than it was: most legacy
tools at least feature some form of "lock file" these days; so it hopefully
won't be long until we can verifiably reproduce their outputs, too!
