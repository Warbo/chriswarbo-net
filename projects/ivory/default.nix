{ defs ? import ../../static/nix { }, nix-helpers ? defs.nix-helpers
, nixpkgs-lib ? nix-helpers.nixpkgs-lib }:
with rec {
  inherit (defs) render;
  inherit (nixpkgs-lib) mapAttrs';

  renderPage = n: vals: {
    name = "${n}.html";
    value = render (rec {
      name = "${n}.md";
      file = ./. + "/${name}";
      SOURCE_PATH = "projects/ivory/${name}";
      TO_ROOT = "./../..";
    } // vals);
  };

  allPages = mapAttrs' renderPage {
    sums_and_products = { };
    negatives_and_inverses = { };
    radicals = { };
    geometric_units = { };
    geometric_algebra = { };
  };
};
allPages
