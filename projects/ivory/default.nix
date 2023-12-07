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
    complex_and_hypercomplex_numbers = { };
    expressions = { };
    geometric_algebra = { };
    geometric_units = { };
    indeterminates = { };
    index = { };
    negatives_and_inverses = { };
    numbers_in_scheme = { };
    numerical_towers = { };
    polynomials = { };
    radicals = { };
    #scheme_geometric_algebra = { };
    sums_and_products = { };
    zero_one_many = { inherit numbers_in_scheme; };
  });
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
