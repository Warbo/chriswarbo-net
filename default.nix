# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};

stdenv.mkDerivation {
  name   = "chriswarbo.net";
  src    = ./.;

  buildInputs = [
    graphviz
    xidel
    php
    gnuplot
    panpipe
    panhandle
    python
  ];
}
