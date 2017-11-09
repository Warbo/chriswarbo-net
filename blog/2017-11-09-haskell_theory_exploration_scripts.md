---
title: Haskell Theory Exploration scripts
---

I thought I'd make a public announcement about some scripts I've built
up over the last few years for "exploring" Haskell code. They can be
found at:

  [http://chriswarbo.net/git/haskell-te](/git/haskell-te)

With mirrors at [/ipns/QmWv958VzBJQchGjYKiSaxLC9ugrjvXkqMpVrmjp9AonXq](
https://ipfs.io/ipns/QmWv958VzBJQchGjYKiSaxLC9ugrjvXkqMpVrmjp9AonXq) and
[https://github.com/Warbo/haskell-te](https://github.com/Warbo/haskell-te)

These use the excellent QuickSpec library from:

  [https://hackage.haskell.org/package/quickspec](
  https://hackage.haskell.org/package/quickspec)

This empirically (through brute force and QuickCheck testing)
conjectures equations about given Haskell function definitions.

I think it's a very under-utilised library, perhaps due to a couple of
reasons:

 - It requires hand-holding from the user, e.g. installing packages,
   importing modules, naming functions, monomorphising types, picking an
   arity, etc.
 - It can be very resource-intensive!

I know there's some work to address the efficiency concerns in newer
version, but that's not made its way to Hackage, Nixpkgs, etc. yet:

  [https://github.com/nick8325/quickspec](https://github.com/nick8325/quickspec)

My scripts try to address the usability issues. More details are in the
README, but basically we can run:

    quickspec myHaskellDir | renderEqs

This will (hopefully) output conjectures found by QuickSpec about the
functions defined in myHaskellDir. This automatically finds all function
definitions, looks up their types, arities, etc. and invokes QuickSpec
in a subprocess (with optional timeouts and memory limits).

If we leave off the `renderEqs`, we get an easily-parsed JSON format,
which we can feed into other tools.

Note that I make *heavy* use of the Nix language, and it's quite
hit-or-miss whether a package will work. My hope is that more widespread
knowledge of these tools will alleviate some of the existing pain
points.

In particular, many packages define `Arbitrary` instances for their
types, but they do so in their test suites rather than their libraries,
so nobody else can make use of them (I assume this is to keep down
library dependencies?)
