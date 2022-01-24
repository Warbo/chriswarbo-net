{ callPackage, commands, dirContaining, fail, lib, relTo, runCommand, withArgs,
  writeScript }:

with builtins;
with lib;
with rec {
  metadata  = callPackage ./metadata.nix  {};
  pageTests = callPackage ./pageTests.nix {};
  test      = { file, name }: runCommand name
    { inherit file; }
    ''
      ${concatStringsSep "\n"
        (attrValues (mapAttrs
          (testName: t:
            if elem testName [ "override" "overrideDerivation" ]
               then ""
               else ''${t} "$file'')
          pageTests))}
      ln -s "$file" "$out"
    '';
};
{ file, inputs ? [], name, SOURCE_PATH, TO_ROOT ? "", vars ? {} }:
  with rec {
    md        = metadata.of file;
    extraPkgs = map (n: if hasAttr n commands
                           then getAttr n commands
                           else callPackage (withArgs [ n ] (getAttr n)) {})
                    (md.packages or []);
    dir       = dirContaining ../.. (md.dependencies or []);
    rendered  = test {
      inherit name;
      file = runCommand "untested-${name}"
      (vars // {
        inherit dir file SOURCE_PATH TO_ROOT;
        __noChroot  = true;
        buildInputs = inputs ++ extraPkgs ++ [
          commands.relativise
          commands.render_page
          fail
        ];
        postprocessor = md.postprocessor or "";
      } //
      optionalAttrs (hasAttr "parseArgs" md) {
        parseArgF = writeScript "${name}-parseArgs.sh" ''
          parseArgs=()
          ${concatMapStringsSep "\n"
              (arg: "parseArgs+=(${escapeShellArg arg})")
              md.parseArgs
          }
         '';
      } //
      optionalAttrs (hasAttr "renderArgs" md) {
        renderArgF = writeScript "${name}-renderArgs.sh" ''
          renderArgs=()
          ${concatMapStringsSep "\n"
            (arg: "renderArgs+=(${escapeShellArg arg})")
            md.renderArgs
          }
        '';
      })
      ''
        export DEST="$PWD/out.html"

        cd "$dir"
        SOURCE="$file" render_page
        grep '^.' < "$DEST" > /dev/null || fail "Error: No output when rendering"

        relativise < "$DEST" > "$out"
      '';
    };
  };
  if hasSuffix ".html" file
     then with { data = writeScript name (readFile file); };
          if TO_ROOT == ""
             then data
             else relTo TO_ROOT data
     else rendered
