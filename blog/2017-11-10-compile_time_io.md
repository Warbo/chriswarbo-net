---
title: Compile-time I/O
---

I think [Gabriel Gonzalez' ideas about removing runtime I/O](
http://www.haskellforall.com/2017/10/why-do-our-programs-need-to-read-input.html) actually
map very closely to the [Nix](http://nixos.org/nix) language. Nix is a pure
functional programming language, with no I/O, where a program is an
"expression", which we can "evaluate". Some expressions evaluate to, for
example, an integer. Others evaluate to a boolean. The most interesting are
those which evaluate to a "derivation".

A derivation is basically a datastructure containing an "environment" (a map
from strings to strings), a "builder" (a string containing a filesystem path),
"arguments" (a list of strings) and a set of "dependencies" (other
derivations). As far as the Nix language is concerned, these derivations are
just values like any other.

By itself this is pretty useless, but there is a tool (`nix-build`) which will
take the result of evaluating a Nix program and, if it's a derivation, will
"build" that derivation. To build a derivation, we first build all of its
dependencies (recursively), then we run the "builder" as a program, with the
"arguments" passed as commandline arguments, and the "environment" as its env
vars. We also add an "out" variable to the environment, with a path as its
value, and the result of the build is whatever the builder wrote to that path.

So why is this relevant? Firstly, the Nix language is useful for *describing*
what to do without *actually* doing anything. This is actually the same idea as
Haskell's I/O system: Haskell is a pure language, with no I/O, which calculates
a single value called `main`. That value has type `IO ()` which basically means
"a program which can perform arbitrary effects". A 'separate tool' (the Haskell
runtime system) takes the resulting value of `main` and runs it as a
program. Hence Haskell is like a pure meta-language, used to construct impure
programs.

Elliott actually [makes this analogy](http://conal.net/blog/posts/the-c-language-is-purely-functional)
by comparing `IO ()` values to C programs, and the Haskell language to the C
preprocessor!

So what practical effect does this way of thinking have, if any? I can't speak
for Dhall (Gabriel's language), but in the case of Nix there is a clear
distinction between "eval time" and "build time". I think this is the key to
figuring out how Gabriel can (provocatively) claim to 'perform all I/O at
compile time': the I/O happens during *evaluation*, which is basically like an
interpreter (if you're wondering why a compiler would implement an interpreter,
consider that "inlining a function" is basically an elaborate way to "call a
function" at compile time; and "constant folding" is an elaborate way to
"perform calculations" at compile time; the logical conclusion to doing this is
a "supercompiler", which can run arbitrary code at compile time).

So really, Gabriel is saying we should embrace metaprogramming, to push as many
failure cases as possible into a run-at-compile-time language, so that the
resulting program (if compilation succeeds) is guaranteed to avoid those
problems. This is similar in spirit to static typing (catching errors before
running the program), although we're actually shifting the emphasis: "the
program" is actually rather trivial, since most of our code is part of the
compile-time language (basically, really extensive macros).

I have sympathy for this; although there's a need to more precisely distinguish
between the "resulting program" and "the output of the resulting program". If we
perform a bunch of elaborate compilation which results in the number `42`, then
that is "a program which performs no I/O", but it's also not a particularly
interesting case. If the result of our compilation is a function, which
e.g. counts the number of words in a given string, then that itself might
perform no I/O, but it's not actually useful until it's invoked by something
which does: either a 'separate tool' (the equivalent of nix-build or Haskell's
RTS) or a subsequent compilation which "imports" that function.
