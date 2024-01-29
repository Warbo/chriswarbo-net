---
title: Dependency Solving in Nix
---

I've [written a lot about the Nix build system](/projects/nixos) over the past
decade, but one complaint I keep coming across is that Nix requires dependency
constraints to be "solved manually".

This is a sort of category-error: sure, it's true that Nix doesn't "solve
dependency constraints"; in the same way that, say, `/usr/bin/env` doesn't
"solve dependency constraints". That's not what it's for! Yet that doesn't stop
us using *actual* dependency solvers, if we want to.

In this post I want to show *why* Nix doesn't perform constraint-solving; why
you shouldn't *want* it to perform constraint-solving; and why this is all a red
herring, since it's easy to have Nix call out to *any* dendency solver (or
indeed any program at all)!

## Nix Basics ##

This section is a quick overview of Nix. A more thorough glossary is given in an
appendix.

Nix calls itself a "purely functional package manager", but that terminology can
lead to an incorrect, and overly-complicated, mental model of what Nix is. I
prefer to describe Nix as a tool to "get specified outputs", where:

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

This is the *underlying model* of Nix. Notice how it doesn't contain any notion
of "package"; hence why I find the phrase "package manager" unhelpful. Also note
that there are no concepts like "version", "constraint", etc.

This model is too cumbersome to interact with directly; instead, we use various
shell commands and higher-level languages (similar to how we use the `git` CLI,
rather than writing trees and blobs by hand). The most common/popular interfaces
for Nix are the original Nix CLI (`nix-build`, `nix-store`, etc.) and the "new"
`nix` CLI (technically still 'experimental', but pretty stable).

### Nixlang ###


## Building Up By Example ##

### Plain Text ###

Let's start with a simple example of using Nixlang to specify an output. In this
case the output will be a file containing the text `hello world`. We can do this
using the function `builtins.toFile`, which takes a filename and the file's
content (function calls in Nixlang look like `f x`):

```nix
builtins.toFile "example.txt" "hello world"
```

Entering this expression in a `nix repl` session gives the path of the output;
and we can check that it really contains the specified text:

```
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
  #!/usr/bin/env bash
  set -eux
  echo 'BEGIN'
  cat ${builtins.toFile "example.txt" "hello world"}
  echo 'END'
''
"/nix/store/i6h22whv4hsnpn7hzwaqxg9jid1pg3hd-sayHello"

nix-repl> :quit
$ bash "/nix/store/i6h22whv4hsnpn7hzwaqxg9jid1pg3hd-sayHello"
+ echo BEGIN
BEGIN
+ cat /nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt
hello world+ echo END
END
```

Notice that the output `/nix/store/i6h22whv4hsnpn7hzwaqxg9jid1pg3hd-sayHello`
contains the path `/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt`. So
`/nix/store/1ygkbyc9ywdkab2ab4f3jx9n92x0ncr7-example.txt` is a "dependency" of
`/nix/store/i6h22whv4hsnpn7hzwaqxg9jid1pg3hd-sayHello`. This seems trivial, but
it is remarkably powerful. In particular:

 - Dependencies are identified by file path, and looked-up/resolved using the
   filesystem. This is decentralised; unlike e.g. PyPI, NPM, Maven, Debian, etc.
   which must consult separate "repositories".
 - There are no "versions" or "constraints" to solve: only one output has that
   path (assuming no hash collisions). This reduces supply-chain attacks,
   maintenance burden, coordination issues, etc.

### Reading Files ###

Nixlang can read the contents of a file, using `builtins.readFile`. This also
works when the file is an output! For example:

```nix
nix-repl> builtins.readFile (builtins.toFile "x" "hello")
"hello"
```

We can use this to alter our previous script to read its message at "build time"
rather than when the script is executed (hence we also change `cat` to `echo`):

``` nix
nix-repl> with {
  # Define a local variable, to make things a bit more readable
  messageFile = builtins.toFile "example.txt" "hello world";
}; builtins.toFile "sayHello" ''
  #!/usr/bin/env bash
  set -eux
  echo 'BEGIN'
  echo ${builtins.readFile messageFile}
  echo 'END'
''
"/nix/store/0ldbwz0s9l1wbhm89g67955x4824lzlj-sayHello"

nix-repl> :quit
$ bash "/nix/store/0ldbwz0s9l1wbhm89g67955x4824lzlj-sayHello"
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

Note that this also works for the `import` function, which reads a file at a
given path (or, if it's a directory, reads a `default.nix` file in that
directory), and *evaluates the contents as a Nixlang expression*. For example:

``` nix
nix-repl> import (builtins.toFile "foo.nix" "1 + 2")
3
```

### Our First Derivation ###

So far we've seen outputs specified by hash, either with contents written
verbatim, or calculated as a Nixlang string. Now we'll specify an output using
a *derivation* instead. Nixlang represents a derivation as an attrset,
containing certain attributes. For example:

``` nix
nix-repl> with {
  out = ''"/nix/store/jp9gw2x59lv6bc51kccpm5zqf9bjwxcn-myDerivation"'';
};
{
  name = "example";
  type = "derivation";
  drvPath = builtins.toFile "myDerivation.drv"
    ''Derive([("out",${out},"","")],[],[],"${builtins.currentSystem}",'' +
    ''"/bin/sh",["-c","echo hello>$out"],[("out",${out}),("outputs","out")])'';
}
«derivation /nix/store/g5wxiik4zdlj308s60yydgz4nxbyc3yr-myDerivation.drv»
```

Derivations specify which command to run (`/bin/sh`, in this case), which
arguments to give it (here `-c` and `echo hello>$out`) and which environment
variables to set (`out` and `outputs`, in this case). We get the outputs
specified in a derivation by "building" it (again, checking caches first). The
`nix repl` interface can do this, via the `:b` command, but I prefer to use the
`nix-build` tool:

```
$ nix-build /nix/store/g5wxiik4zdlj308s60yydgz4nxbyc3yr-myDerivation.drv
this derivation will be built:
  /nix/store/g5wxiik4zdlj308s60yydgz4nxbyc3yr-myDerivation.drv
building '/nix/store/g5wxiik4zdlj308s60yydgz4nxbyc3yr-myDerivation.drv'...
/nix/store/jp9gw2x59lv6bc51kccpm5zqf9bjwxcn-myDerivation
$ cat /nix/store/jp9gw2x59lv6bc51kccpm5zqf9bjwxcn-myDerivation
hello
```

In practice, nobody specifies the contents of `.drv` files manually like this!
Instead, we use helper functions, like those provided by Nixpkgs (a git
repository containing many Nixlang definitions).

### Using Nixpkgs ###

Since Nixpkgs is a git repo, we can use Nixlang's `builtins.fetchGit` function
to output a directory containing some particular commit. However, Nixpkgs has so
many commits that fetching the metadata can make this quite slow. Instead, it's
faster to use GitHub's HTTP API, via `builtins.fetchTarball`; although that
lack's the cryptographic validation provided by git, so we ask Nix to check the
output against a known hash (obtained via trust-on-first-use):

```nix
nix~repl> builtins.fetchTarball { url = "FIXME"; }
```

To access the definitions from that `outPath` directoy, we can use `import`:

``` nix
nix~repl> import (builtins.fetchTarball { url = "FIXME"; })
```

Notice that the result is a *function* (AKA a `lambda`). This lets us pass in
arguments which override defaults like the system architecture, which licenses
to allow, etc. We'll override the `config` and `overlays` to be empty to avoid
the default, impure definitions (which look for files in `$HOME/.config`; eww!)

WARNING: Nixpkgs has tens of thousands of definitions, so it's unwise to try
printing the whole thing!

### `runCommand` ###

`runCommand` is one of the most useful functions provided by Nixpkgs. It takes
three arguments:

 - A name, which will appear in the output's path. An appropriate hash will be
   calculated and prepended automatically.
 - A set of environment variables. The output path will be appended as an `out`
   environment variable.
 - A string of Bash code to execute.

The result is a derivation which uses a Bash executable to run the given code.
(There is some indirection via a helper script, which makes it easier to control
via Nixlang rather than needing Bash boilerplate).

Here's our "hello world" example, refactored to use `runCommand`:

```nix
with {
  nixpkgs = with {
    src = builtins.fetchTarball {
      url = "FIXME";
    };
    import src { config = {}; overlays = []; };
  };
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

## Solving Constraints ##

pallpWe now have all pieces needed to perform constraint-solving:

 - We can specify an output containing our constraints (e.g. using `toFile`,
   `readFile`, `fetchGit`, `runCommand`, etc.)
 - We can specify an output containing a solution, by giving `runCommand` a
   script that executes some constraint-solver on our constraints.
 - We can use `readFile` or `import` to read in that solution, and splice it
   into our other derivations (e.g. more calls to `runCommand`).

### Haskell Example ###

Here's a realistic example: using Haskell's Cabal packaging tool to resolve
constraints, taking dependencies from the Hackage repository:

```nix
TODO
```

## Unreliable/Non-deterministic Dependencies ##

I chose a Haskell example since Hackage has cryptographically-verifiable indexes
(i.e. we can fetch a known git commit). Unfortunately, many legacy dependency
managers aren't so usable; instead relying on the contents of arbitrary
HTTP responses. Fixing such non-determinism is out of scope
for this article, but we can at least *check* that results match an expected hash.

### Maven Example ###

Apache Maven makes arbitrary requests to its "repositories" whilst solving dependencies. This can be a local folder, but in that case must be pre-populated with the required dependencies; creating a chicken-and-egg problem.

One way to avoid such infinite-recursion is to use "lock files", but those are pretty clunky. In particular, lock files split a project's build into two steps, requiring extra manual effort to manage. However, we've already seen how to implement such multi-step builds: we can write a Nix derivation that specifies a lock file, then import its contents to define our main project derivation.

I've used this approach successfully at a company with many internal, inter-dependent Maven projects. Maven doesn't have native support for lock files (and having it fetch a plugin is another chicken-and-egg problem!), so we used `mvn2nix`. As a result, each build of a project would perform the following steps:

 - Calculate a derivation that runs `mvn2nix` on the project's POM, to produce a "lock file" of dependencies (including their URL and hash)
 - Read that lock file, to calculate derivations which:
    - Fetch each dependency
    - Define a Maven repo directory containing all of those dependencies
    - Use that repo directory to build the project in "offline" mode

Since Nix caches outputs, it will only build lock files for POMs it hasn't seen before; it will only download dependencies it hasn't already fetched; and it will only rebuild the project if its source code or POM have changed. Otherwise, cached outputs will be used.

## Tips & Tricks ##

I've been using these techniques successfully for years. Still, there are some approaches that will work better than others.

### Invalidate Fixed-Output Derivations By Putting Hashes In Names ###

A "fixed-output derivation" uses impure commands, which should nevertheless produce the same output every time. Downloading a static file over HTTP is a common example; another example is generating a "lock file" (e.g. using `mvn2nix`). The output of these derivations is checked against an expected hash, to make sure it worked as intended (e.g. no supply-chain attacks).

One problem with using fixed-output derivations is that cached outputs will be used *even if the dependencies change*. For example, if we change our POM, but forget to change the expected hash, Nix will happily use its cached output from the old POM (since that has the requested hash!).

I hate having to remember things, or carry out steps which could be automated. Thankfully, there's an easy way to avoid this: the function which calculates your "lock file" derivation should *also* take a hash of the input (e.g. the POM), and append that to the derivation's *name*. This way, when the POM gets changed, we'll be asking Nix for a different filename, which won't exist in its cache. If the expected hash is no longer valid (e.g. due to adding new dependencies in the POM), then Niz will tell us.

### Normalise Inputs To Avoid Unnecessary Rebuiding ###

It's very common to update a project's version number, which has no effect on its dependencies. To avoid rebuilding lock files unnecessarily, we can pre-process the config file to normalise such unnecessary details: for example, replace the project's version number with "unversioned" or 0.0.0. That way, its hash will be unaffected by such irrelevant updates.
