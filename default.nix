# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};

let env = haskellPackages.ghcWithPackages (p: with p; [
            hakyll QuickCheck smallcheck
          ]);
in stdenv.mkDerivation {
  name   = "chriswarbo.net";
  src    = ./.;

  buildInputs = [
    env
    php
    git
    cacert
    gnuplot
    pngcrush
    imagemagick
    coq
    panpipe
    panhandle
    utillinux
    time
    python
  ];

  shellHook   = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
