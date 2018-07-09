with import ./static/nix;
with rec {
  pages = callPackage ./pages.nix { inherit pages pandocPkgs repoSource; };
};
pages
