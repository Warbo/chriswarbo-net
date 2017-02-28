# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};
with {
  stripper = stdenv.mkDerivation {
    name         = "stripper";
    buildInputs  = [ makeWrapper ];
    script       = ./static/stripEmptyPreCode;
    env          = buildEnv {
      name  = "stripper-env";
      paths = [ pythonPackages.python pythonPackages.beautifulsoup4 ];
    };
    buildCommand = ''
      mkdir -p "$out/bin"
      makeWrapper "$script" "$out/bin/stripEmptyPreCode" \
        --prefix PATH : "$env/bin"
    '';
  };
};
stdenv.mkDerivation {
  name   = "chriswarbo.net";
  src    = ./.;

  buildInputs = [
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
    stripper
  ];
}
