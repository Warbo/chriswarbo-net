with import ./static/nix/defs.nix;
with configuredPkgs;
with rec {
  pages = callPackage ./pages.nix { inherit pages repoSource; };
};
pages
