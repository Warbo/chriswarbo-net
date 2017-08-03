---
title: nix-eval for Haskell
packages: [ 'nix-shell' ]
---

The [`nix-eval` package](https://hackage.haskell.org/package/nix-eval) scratches
a particular itch for me. It allows Haskell code to be evaluated at run-time, in
a sub-process, using [Nix](http://nixos.org/nix) to get any dependencies it
needs.

NOTE: This page is [active code](/projects/activecode/), so check out the "view
source" link at the bottom of the page if you want to follow along with the
examples!

```{pipe="cat > runWithPkgs.sh && chmod +x runWithPkgs.sh"}
#!/bin/sh

# Separate all arguments with spaces
PKGS=""
for PKG in "$@"
do
  PKGS="$PKGS $PKG"
done

CMD=$(nix-shell -p which \
  -p "with import <nixpkgs> {}; haskellPackages.ghcWithPackages (h: [$PKGS])" \
  --run 'which runhaskell')
echo "Running '$CMD'" 1>&2
$CMD -XOverloadedStrings
```

As tradition dictates, we'll start with `"hello world"`, which is trivial to do
using Haskell's built-in `String` type:

```{.haskell pipe="tee hws.hs"}
main = putStr "hello world"
```

```{.haskell pipe="sh"}
./runWithPkgs.sh < hws.hs
```

However, if we try to write `"hello world"` using the popular `Text` library, it
breaks:

```{.haskell pipe="tee hwt.hs"}
main = putStr (Data.Text.unpack (Data.Text.pack "hello world"))
```

```{pipe="sh"}
./runWithPkgs.sh < hwt.hs 2>&1
exit 0 # Ignore the error
```

In order to use functions like `Data.Text.unpack`, we first need to import the
`Data.Text` module:

```{.haskell pipe="tee import.hs"}
import Data.Text
main = putStr (Data.Text.unpack (Data.Text.pack "hello world"))
```

```{pipe="sh"}
./runWithPkgs.sh < import.hs 2>&1
exit 0 # Ignore the error
```

Another error! In order to import modules like `Data.Text`, we first need to
register the `text` package. This can't actually be done from within Haskell; we
need to perform this step before invoking the Haskell compiler/interpreter.

Most of the time that's fine, and we have tools like Cabal and Nix to help us:

```{.haskell pipe="sh"}
cat import.hs
```

```{.haskell pipe="sh"}
./runWithPkgs.sh "h.text" < import.hs
```

However, this falls apart when we want to evaluate Haskell code at *run-time*,
ie. using an `eval` function.

Run-time code evaluation is well-known in scripting languages, and although it's
a little tricky, it can be done in Haskell too. However, most implementations of
`eval` rely on the packages and modules which are available to the "host"
program, rather than what is needed by the given expression.

That's where `nix-eval` comes in. It provides a simple `Expr` data type which
contains a `String` of code to evaluate, along with a list of packages and
modules it needs. During evaluation, these dependencies are fetched using Nix,
and the code is sent to a new instance of GHC, which has those packages
available.

For example, our simple `"hello world"` would become:

```{.haskell pipe="tee nixs.hs"}
import Language.Eval
main = do result <- eval (asString "hello world")
          case result of
               Nothing -> error "Didn't work :("
               Just x  -> putStr x
```

```{.haskell pipe="sh"}
./runWithPkgs.sh "h.nix-eval" < nixs.hs
```

As you can see, it's a bit more verbose. In particular, the output type of
`eval` is `IO (Maybe String)`, since evaluation is not a pure function (`IO`)
and it may fail (`Maybe`). The `String` is the standard output of the GHC
process which Nix invokes, which we're free to parse in any way we like.

Next we can try the `text` example, but this time we won't import or register
anything besides `nix-eval`, like above:

```{.haskell pipe="tee nixt.hs"}
import Language.Eval

dt = withPkgs ["text"] . qualified "Data.Text"

main = do result <- eval (dt "unpack" $$ (dt "pack" $$ asString "hello world"))
          case result of
               Nothing -> error "Didn't work :("
               Just x  -> putStr x
```

```{.haskell pipe="sh"}
./runWithPkgs.sh "h.nix-eval" < nixt.hs
```

`nix-eval` has a few simple combinators, including `$$` which applies one `Expr`
to another (like Haskell's `$`); `withPkgs` and `withMods` which append to an
`Expr`'s context; and `qualified` which appends to the context *and* prefixes
the expression.

I wrote this to help simplify my [`mlspec`](/git/mlspec/)
project, which currently generates entire Haskell projects (Cabal files and all)
in order to avoid hand-coding theories for
[QuickSpec](https://hackage.haskell.org/package/quickspec). All of that
shenanigans should be replacable by a simple call out to `eval`, and even better
I should be able to simply discard failures, rather than have GHC flat out
refuse to touch anything else in the project.

There are still tricky issues like running `nix-shell`s inside `nix-shell`s. For
example, this page's source is a little complicated since the Haskell code is
running `nix-shell` (via `nix-eval`); yet that Haskell code *itself* is running
in `nix-shell`, in order to satisfy the `nix-eval` dependency; and all of this
is inside another `nix-shell` which is rendering my site.

That's required a couple of tricks, eg. using
[`nix-shell` shebangs](/projects/nixos/nix_shell_shebangs.html) rather than direct
invocations from the shell; and using `nix-shell` to *build* the required GHC +
packages, but actually *invoking* it from outside the shell.

Hopefully the edge-cases will become less painful as Nix evolves :)
