with import ./static/nix;
with configuredPkgs;
with rec {
  pages = callPackage ./pages.nix { inherit pages repoSource; };
};
pages
