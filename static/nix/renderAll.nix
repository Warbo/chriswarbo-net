{ dirsToAttrs, isPath, lib, render }:

with rec {
  inherit (builtins) isAttrs toJSON;
  inherit (lib)
    concatStringsSep hasSuffix isDerivation mapAttrs' removeSuffix take;

  mdToHtml = n: if hasSuffix ".md" n then "${removeSuffix ".md" n}.html" else n;

  renderGo = prefix: n: v: rec {
    name = mdToHtml n;
    # Standalone files should be passed to render
    value = if isDerivation v || isPath v then
      render {
        inherit name;
        # We'll skip tests for anything in the "unfinished" directory
        unfinished = take 1 prefix == [ "unfinished" ];
        file = v;
        SOURCE_PATH = concatStringsSep "/" (prefix ++ [ n ]);
        TO_ROOT = concatStringsSep "/" ([ "." ] ++ map (_: "..") prefix);
      }
    else if isAttrs v then # Directories will become attrsets
      (if v ? "default.nix" then # defer to a default.nix file if present
        import v."default.nix" { }
      else # Otherwise recurse
        go (prefix ++ [ n ]) v)
    else
      abort "Can't render ${toJSON { inherit n v; }}";
  };

  go = prefix: mapAttrs' (renderGo prefix);
};

dir:
go [ dir ] (dirsToAttrs (../.. + "/${dir}"))
