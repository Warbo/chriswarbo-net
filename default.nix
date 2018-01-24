with import ./static/nix/defs.nix {};
with configuredPkgs;
with rec {
  pages = callPackage ./pages.nix ({
      inherit latestConfig pages repoSource;
    } // (if python ? withPackages
             then builtins.trace "No longer need python.withPackages check" {}
             else { inherit (nixpkgs1709) python; }));
};
pages
