{ dirsToAttrs, isPath, lib, render }:

with { inherit (builtins) toJSON; };
with lib;
with rec {
  mdToHtml = n: if hasSuffix ".md" n then "${removeSuffix ".md" n}.html" else n;

  renderGo = prefix: n: v: rec {
    name = mdToHtml n;
    value = if isDerivation v || isPath v then
      render {
        inherit name;
        unfinished = take 1 prefix == [ "unfinished" ];
        file = v;
        SOURCE_PATH = concatStringsSep "/" (prefix ++ [ n ]);
        TO_ROOT = concatStringsSep "/" ([ "." ] ++ map (_: "..") prefix);
      }
    else if isAttrs v then
      go (prefix ++ [ n ]) v
    else
      abort "Can't render ${toJSON { inherit n v; }}";
  };

  go = prefix: mapAttrs' (renderGo prefix);
};

dir:
go [ dir ] (dirsToAttrs (../.. + "/${dir}"))
