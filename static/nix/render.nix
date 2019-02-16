{ commands, dirContaining, lib, metadata, relTo, runCommand, self,
  writeScript }:

{ file, inputs ? [], name, vars ? {}, relBase ? null, SOURCE_PATH }:

with builtins;
with lib;
with rec {
  md        = metadata file;
  extraPkgs = map (n: getAttr n (self // commands))
                  (md.packages or []);
  dir       = dirContaining ../.. (md.dependencies or []);
  rel       = if relBase == null
                 then (x: x)
                 else relTo relBase;
  dupe      = rel (writeScript name (readFile file));
  rendered  = runCommand name
    (vars // {
      inherit dir file SOURCE_PATH;
      buildInputs   = inputs ++ extraPkgs ++ [
                        commands.relativise
                        commands.render_page
                      ];
      TO_ROOT       = if relBase == null then "" else relBase;
      postprocessor = md.postprocessor or "";
    })
    ''
      export DEST="$PWD/out.html"

      cd "$dir"
      SOURCE="$file" render_page
      if grep '^.' < "$DEST" > /dev/null
      then
        if [[ -n "$TO_ROOT" ]]
        then
          relativise < "$DEST" > "$out"
        else
          mv "$DEST" "$out"
        fi
      else
        echo "No output when rendering, aborting" 1>&2
        exit 1
      fi
    '';
};
if hasSuffix ".html" file
   then dupe
   else rendered
