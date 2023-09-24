{ warbo-packages ? import ../../../warbo-packages { }
, nix-helpers ? warbo-packages.nix-helpers, nixpkgs ? nix-helpers.nixpkgs }:
with builtins;
with rec {

  extras = nix-helpers // warbo-packages;
};
nixpkgs.newScope (extras // { inherit extras; }) ./overlay.nix { }
