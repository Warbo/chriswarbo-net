{ defs ? import ../../static/nix { }, nix-helpers ? defs.nix-helpers
, nixpkgs-lib ? nix-helpers.nixpkgs-lib, nixpkgs ? nix-helpers.nixpkgs }:
with rec {
  inherit (defs) render;
  inherit (nixpkgs-lib) mapAttrs mapAttrs';

  renderPage = name: extraVars:
    render {
      name = "${name}.html";
      file = ./. + "/${name}.md";
      SOURCE_PATH = "projects/units/${name}.md";
      TO_ROOT = "./../..";
      vars = extraVars;
    };

  pages = mapAttrs (_: _: { }) (nix-helpers.suffixedFilesIn ".md" ./.);

  allPages = mapAttrs renderPage (with allPages; pages);
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
