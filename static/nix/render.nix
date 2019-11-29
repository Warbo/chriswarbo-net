{ callPackage, commands, dirContaining, fail, lib, relTo, runCommand, self,
  writeScript }:

with builtins;
with lib;
with {
  metadata = callPackage ./metadata.nix {};
  nonempty = { file, name ? "nonempty" }: runCommand name
    {
      inherit file;
      buildInputs = [ fail ];
    }
    ''
      SIZE=$(stat -L --printf="%s" "$file")
      [[ "$SIZE" -gt 5 ]] || fail "File '$file' has size '$SIZE'"
      ln -s "$file" "$out"
    '';
};
rec {
  render = { file, inputs ? [], name, SOURCE_PATH, TO_ROOT ? "", vars ? {} }:
    with rec {
      md        = metadata file;
      extraPkgs = map (n: getAttr n (self // commands))
                      (md.packages or []);
      dir       = dirContaining ../.. (md.dependencies or []);
      rendered  = runCommand name
        (vars // {
          inherit dir file SOURCE_PATH TO_ROOT;
          __noChroot    = true;
          buildInputs   = inputs ++ extraPkgs ++ [
                            commands.relativise
                            commands.render_page
                            fail
                          ];
          postprocessor = md.postprocessor or "";
        })
        ''
          export DEST="$PWD/out.html"

          cd "$dir"
          SOURCE="$file" render_page
          grep '^.' < "$DEST" > /dev/null || fail "Error: No output when rendering"

          relativise < "$DEST" > "$out"
        '';

      output = if hasSuffix ".html" file
                  then with { data = writeScript name (readFile file); };
                       if TO_ROOT == ""
                          then data
                          else relTo TO_ROOT data
                  else rendered;
    };
    nonempty { inherit name; file = output; };

  renderAll = dir:
    with rec {
      mdToHtml = n: if hasSuffix ".md" n
        then "${removeSuffix ".md" n}.html"
        else n;

      renderGo = prefix: n: v: rec {
        name  = mdToHtml n;
        value = if isDerivation v || self.isPath v
          then self.render {
          inherit name;
          file        = v;
          SOURCE_PATH = concatStringsSep "/" (prefix ++ [n]);
          TO_ROOT     = concatStringsSep "/" (["."] ++ map (_: "..")
            prefix);
        }
        else if isAttrs v
          then go (prefix ++ [n]) v
          else abort "Can't render ${toJSON { inherit n v; }}";
      };

      go = prefix: mapAttrs' (renderGo prefix);
    };
    go [ dir ] (self.dirsToAttrs (../.. + "/${dir}"));
}
