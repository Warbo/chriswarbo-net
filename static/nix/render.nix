{ callPackage, commands, dirContaining, fail, lib, relTo, runCommand, self,
  writeScript }:

with { metadata = callPackage ./metadata.nix {}; };

{ file, inputs ? [], name, vars ? {}, relBase ? "", SOURCE_PATH }:

with builtins;
with lib;
with rec {
  md        = metadata file;
  extraPkgs = map (n: getAttr n (self // commands))
                  (md.packages or []);
  dir       = dirContaining ../.. (md.dependencies or []);
  rendered  = runCommand name
    (vars // {
      inherit dir file SOURCE_PATH;
      buildInputs   = inputs ++ extraPkgs ++ [
                        commands.relativise
                        commands.render_page
                        fail
                      ];
      TO_ROOT       = relBase;
      postprocessor = md.postprocessor or "";
    })
    ''
      export DEST="$PWD/out.html"

      cd "$dir"
      SOURCE="$file" render_page
      grep '^.' < "$DEST" > /dev/null || fail "Error: No output when rendering"

      relativise < "$DEST" > "$out"
    '';
};
if hasSuffix ".html" file
   then with { data = writeScript name (readFile file); };
        if relBase == ""
           then data
           else relTo relBase data
   else rendered
