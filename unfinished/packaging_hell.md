Haskell's packaging workflow is overly complicated. GHC tries to be clever about
where it looks for dependencies, how it orders their compilation, etc.. This is
usually not enough, so we have `ghc-pkg` which tries to make dependency lookups
smarter. This is usually not enough *either*, so we have `cabal` to make
dependency lookup smarter. Some find this to not be enough either, and use
`stack` to try and make dependency lookup even smarter. This is *still* not
enough for some, who use Nix, Docker, Puppet, Chef, Ansible, etc. to make their
dependency lookup *even smarter*.

This is a mess; not because there's a lot of tools, but because many of these
tools were not designed to be (easily) extensible: they use non-standard config
formats (like '.cabal' files), they make arbitrary assumptions (e.g. filesystem
layout, network access, etc.), and don't allow programmatic control over their
behaviour.

My thoughts:

 - Make programs simple and predictable. In particular, if we find that some
   "smart" functionality, like `ghc` looking up dependencies, is no longer good
   enough, and we need a separate tool (`ghc-pkg`, `cabal`, `stack`, etc.), then
   we should drop that not-smart-enough functionality from the underlying tool
   as well. In the case of `ghc`, we could e.g. take a single directory
   containing all of the relevant modules (compiled or source), and not bother
   looking anywhere else. Those directories can then be provided by an external
   tool, e.g. by symlinking to some global cache of extracted tarballs.
 - In a related way: always assume that your current program will be abstracted
   over or scripted by some future program. Cabal, etc. do this to some extent
   by providing the `Cabal` library, but

The default/de-facto Haskell setup of GHC + Hackage/Stackage + Cabal/Stack is
enormously complicated, and only seems to be getting worse as more options and
tweaks are added.

I think the main problem is that there are far too many tools involved, which
each expect their own databases, search directories, etc. `ghc` itself will
search for modules in particular locations, e.g. the current working directory
and paths given in the arguments. GHC also calls out to `ghc-pkg`, which adds a
layer of indirection, letting us set up a database of packages for GHC to
use. `cabal` and `stack` provide another layer of indirection on top of that,
with their own config files which detach some of the functionality from the
