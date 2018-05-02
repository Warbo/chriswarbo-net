{ stable ? true }:
with import ./static/nix/defs.nix { inherit stable; };
with configuredPkgs;
with rec {
  pages = callPackage ./pages.nix { inherit pages repoSource; };
};
pages
