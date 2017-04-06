with import ./defs.nix;
with configuredPkgs;
callPackage ./pages.nix { inherit latestConfig repoRefs repoSource; }
