{ warbo-packages ? import ./warbo-packages.nix { }
, nix-helpers ? warbo-packages.nix-helpers, nixpkgs ? nix-helpers.nixpkgs }:
with rec {
  defs = builtins.mapAttrs
    (_: f: nixpkgs.newScope (nix-helpers // warbo-packages // defs) f { })
    (nix-helpers.nixFilesIn ./.) // {
      inherit warbo-packages nix-helpers nixpkgs;
    };
};
defs
