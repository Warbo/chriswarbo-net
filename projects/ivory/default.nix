{
  defs ? import ../../static/nix { },
  nix-helpers ? defs.nix-helpers,
  nixpkgs-lib ? nix-helpers.nixpkgs-lib,
}:
with rec {
  inherit (defs) render;
  inherit (nixpkgs-lib) mapAttrs mapAttrs';

  renderPage =
    name: extraVars:
    render {
      name = "${name}.html";
      file = ./. + "/${name}.md";
      SOURCE_PATH = "projects/ivory/${name}.md";
      TO_ROOT = "./../..";
      vars = {
        setup = ./setup.sh;
      } // extraVars;
    };

  pages = mapAttrs (_: _: { }) (nix-helpers.suffixedFilesIn ".md" ./.);

  allPages = mapAttrs renderPage (
    with allPages;
    pages
    // {
      geometric_algebra = {
        inherit
          geometric_units
          numbers_in_scheme
          sums_and_products
          zero_one_many
          ;
      };
      sums_and_products = {
        inherit numbers_in_scheme;
      };
      zero_one_many = {
        inherit numbers_in_scheme;
      };
    }
  );
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
