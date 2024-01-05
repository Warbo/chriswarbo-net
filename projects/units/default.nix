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
      inputs = builtins.attrValues scripts;
    };

  scripts = mapAttrs (name: file:
    nix-helpers.mkBin {
      inherit name file;
      paths = [ nixpkgs.jq nixpkgs.libxslt ];
      vars = {
        CTOP = "${web-xslt}/ctop/ctop.xsl";
        BARS = scripts/bars.xsl;
        OURS = scripts/ours.xsl;
      };
    }) (nix-helpers.suffixedFilesIn ".sh" ./scripts);

  web-xslt = fetchGit {
    url = "https://github.com/davidcarlisle/web-xslt.git";
    ref = "main";
    rev = "e2d0023bd9b0e5f0538e2ec3df34a5ec5872f549";
  };

  pages = mapAttrs (_: _: { }) (nix-helpers.suffixedFilesIn ".md" ./.);

  allPages = mapAttrs renderPage (with allPages; pages);
};
mapAttrs' (name: value: {
  inherit value;
  name = "${name}.html";
}) allPages
