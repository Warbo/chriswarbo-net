---
title: What I Would Like
---

I'm a big fan of Nix and the "ecosystem" which is growing up around it:

 - The language at the heart of everything, called the "nix expression language"
   in the Nix manual.
 - The commandline tools for evaluating (`nix-instantiate`), building
   (`nix-build`), creating environments (`nix-shell`), caching
   (`nix-store --add`) and so on. They can be a little clunky, but there is work
   underway to clean them up a little.
 - `nix-repl`, a commandline REPL for the Nix language.
 - The nixpkgs library, which contains a small "standard library" for writing
   Nix expressions, as well as a whole bunch of packages and infrastructure for
   defining, combining and overriding them.
 - The NixOS operating system, which is built out of nixpkgs.
 - Hydra, a build server which can check git repos, build Nix expressions and
   serve the resulting build products.

There is more stuff out there (e.g. NixOps, Disnix, etc.) and these components
can be combined in lots of ways. One thing that I'd really like to see is some
combination of `nix-repl`, `nix-shell` and maybe an interactive CLI or editor
plugin which allows step-wise interactive debugging of scripts.

Here a somewhat contrived example. Let's say that `foo.nix` contains the
following:

```
{ pkgs ? import <nixpkgs> {} }:

with pkgs;
stdenv.mkDerivation {
  name = "foo";
  buildInputs = [ bar ];
}
```

This can be built with `nix-build foo.nix`, but what if there's a failure? We
can use `nix-shell foo.nix` to drop into a shell which mimics the build
environment of the `foo` package; in particular, the dependency `bar` will be
available. From here we can try to track down the problem, by running the
various "phases" (`buildPhase`, `checkPhase`, etc.) which are defined by
`stdenv.mkDerivation`; tedious, but doable.

If there's a problem building `bar`, we can't point `nix-shell` to some
`bar.nix` file instead, since we don't know where such a file might be; or
whether any such file exists (`bar` might have been calculated in some
arbitrarily complicated way).

However, we know where `bar` is "in Nix": it must be taken from inside
`import <nixpkgs> {}`, since otherwise the definition of `foo` would abort with
an "undefined variable" error. Hence we can run a command like
`nix-shell -E '(import <nixpkgs> {}).bar'` to get into the build environment of
`bar`.

What if we *don't* know where `bar` comes from? For example, `foo` might use a
modified version like this:

```
{ pkgs ? import <nixpkgs> {} }:

with pkgs;
stdenv.mkDerivation {
  name = "foo";
  buildInputs = [ (bar.override { a = b; }) ];
}
```

Now we're not depending directly on `bar`, we're first overriding it in some
way. We can still debug `foo` just as before, and the shell environment will get
this overridden version of `bar`, but what if `bar` is the one with the problem?

We can't pluck `bar` out of `import <nixpkgs> {}` like before, since that's not
the right version to use. Instead we must recalculate the correct version; in
this case that means taking `bar` out of `import <nixpkgs> {}` and calling its
`override` function; but we're also using some value `b` in our overrides, which
must have come from `import <nixpkgs> {}` too (via the same reasoning as for
`bar`). Having reverse-engineered our dependency, we can now supply `nix-shell`
with an equivalent calculation, like this:

    nix-shell -E '(with import <nixpkgs> {}; bar.override { a = b; })'

This sort of expression-gymnastics seems like exactly the sort of thing a pure
functional programming language like Nix should be good at. Our lives are
certainly made easier by Nix's purity, but such expressions can still be
arbitrarily complicated; for example, many of my Nix expressions will fetch a
Git repository, run a tool like `cabal2nix` on the contents to *generate Nix
expressions at runtime*, which are then imported and messed around with in all
sorts of other ways.

Trying to debug such things can get very tedious very quickly; often relying on
printf debugging, and long iteration times (since these print statements can
invalidate caches and cause a whole bunch of things to be recompiled, retested,
etc.).

What I would like to see is a way of querying the build environment for some
term buried inside a Nix expression; some way of asking for *this*
`bar.override { a = b; }`, given the surrounding scope (i.e. using the same
values of `bar`, `b`, etc. that are used during a build), without having to
decouple things or reverse-engineer the right inputs or any other such
inconvenience.

One approach would be to "tag" such derivations with a value, e.g. with
`dropToShell = true`; `nix-shell` could then proceed as normal (e.g. using `-E`)
but drop into the appropriate shell if it spots one of those annotations.

Another approach might be an option which drops the user into a shell if a
builder gives an error (where Nix would normally bail out saying "could not
build"). More hackily, we could drop to an interactive shell if some
randomly-generated token appears on the builder's stderr; inserting
`echo "$TOKEN" 1>&2` into the builder would work for many derivations, although
it's a bit bash-centric for others.

I've thought about an alternative, e.g. like an XPath expression into an
expression, but it seems like too much work for the existing Nix implementation.
In particular, we would have to treat function calls as first-class entities, so
that we can inspect the expression being called and the argument; this seems
like a job for syntax trees, but we'd *also* need to allow particular function
calls to be performed explicitly, so that we can *generate* the expressions that
we want to debug. Hence we also need access to a full interpreter.

This seems like exactly the sort of thing Lisp/Scheme macros are good for, so is
probably a point in favour of Guix.

The end goal would be to edit a Nix expression, e.g. using Emacs, and be able to
say "give me a shell at this point". This might, for example, annotate the code
appropriately (inserting a `dropToShell` or call to `exit 1`), send a
predetermined expression to `nix-shell` or some equivalent tool (a good default
would be 'import ./.'), and remove the annotation. A less hacky way would be to
provide a filename and cursor position to some program, which will "mark" that
expression in some way (not necessarily within the Nix language), and get the
same result.

While we're at it, having a "`nix-repl` at point" would be nice too :)
