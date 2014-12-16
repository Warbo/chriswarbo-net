# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "chriswarbo.net";

  src = ./.;

  buildInputs = [
    php
    haskellPackages.ghc
    haskellPackages.hakyll
    gnuplot
    pngcrush
  ];
}
