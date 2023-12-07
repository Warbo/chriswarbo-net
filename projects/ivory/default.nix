{ defs ? import ../../static/nix { }, nix-helpers ? defs.nix-helpers
, nixpkgs-lib ? nix-helpers.nixpkgs-lib }:
with rec {
  inherit (defs) render;
  inherit (nixpkgs-lib) mapAttrs mapAttrs';

  renderPage = name: extraVars:
    render {
      name = "${name}.html";
      file = ./. + "/${name}.md";
      SOURCE_PATH = "projects/ivory/${name}.md";
      TO_ROOT = "./../..";
      vars = { setup = ./setup.sh; } // extraVars;
    };

  # TODO: Use filesIn
  allPages = mapAttrs renderPage (with allPages; {
    index = { };
    sums_and_products = { };
    negatives_and_inverses = { };
    numbers_in_scheme = { };
    radicals = { };
    geometric_units = { };
    geometric_algebra = { };
  };
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
