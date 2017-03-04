# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};
with {
  commands = callPackage ./commands.nix {};
};
stdenv.mkDerivation {
  name   = "chriswarbo.net";
  src    = ./.;

  buildInputs = [
    commands
    graphviz
    xidel
    php
    gnuplot
    gnumake
    panpipe
    panhandle
    python
    pythonPackages.pyyaml
    pandoc
  ];
}
