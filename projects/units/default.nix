{ defs ? import ../../static/nix { }, nix-helpers ? defs.nix-helpers
, nixpkgs-lib ? nix-helpers.nixpkgs-lib, nixpkgs ? nix-helpers.nixpkgs }:
with rec {
  inherit (defs) render;
  inherit (nixpkgs-lib) mapAttrs mapAttrs';

  renderPage = name: vars:
    render {
      inherit vars;
      name = "${name}.html";
      file = ./. + "/${name}.md";
      SOURCE_PATH = "projects/units/${name}.md";
      TO_ROOT = "./../..";
    };

  pages = mapAttrs (_: _: { }) (nix-helpers.suffixedFilesIn ".md" ./.);

  allPages = mapAttrs renderPage pages;
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
