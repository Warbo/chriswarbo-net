{ callPackage, commands, dirContaining, fail, lib, relTo, runCommand, withArgs,
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
{ file, inputs ? [], name, SOURCE_PATH, TO_ROOT ? "", vars ? {} }:
  with rec {
    md        = metadata file;
    extraPkgs = map (n: if hasAttr n commands
                           then getAttr n commands
                           else callPackage (withArgs [ n ] (getAttr n)) {})
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
  nonempty { inherit name; file = output; }
