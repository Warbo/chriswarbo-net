# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};

let env = haskellPackages.ghcWithPackages (p: with p; [
            hakyll QuickCheck smallcheck
          ]);
in stdenv.mkDerivation {
  name   = "chriswarbo.net";
  src    = ./.;

  LANG           = "en_US.UTF-8";
  LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux "${glibcLocales}/lib/locale/locale-archive";
  CURL_CA_BUNDLE = "/etc/ssl/certs/ca-bundle.crt";

  buildInputs = [
    env
    php
    git
    cacert
    #haskellPackages.ghc
    #haskellPackages.hakyll
    #haskellPackages.QuickCheck
    #haskellPackages.smallcheck
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
