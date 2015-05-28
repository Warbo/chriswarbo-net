# Used by `nix-shell` to gather the dependencies for building this site
with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "chriswarbo.net";

  src = ./.;

  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux "${glibcLocales}/lib/locale/locale-archive";
  CURL_CA_BUNDLE = "/etc/ssl/certs/ca-bundle.crt";

  buildInputs = [
    php
    git
    cacert
    haskellPackages.ghc
    haskellPackages.hakyll
    haskellPackages.QuickCheck
    haskellPackages.smallcheck
    gnuplot
    pngcrush
    imagemagick
    coq
    panpipe
    panhandle
    utillinux
    time
    python
    git2html
  ];
}
