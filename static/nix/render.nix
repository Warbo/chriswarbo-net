{ commands, emptyDirectory, fail, lib, metadata, newScope, nix-helpers
, nixpkgs-lib, pageTests, relTo, runCommand, warbo-packages, withArgs
, writeScript }:

with rec {
  inherit (builtins) attrValues elem getAttr hasAttr hashFile map readFile;
  inherit (lib)
    concatMapStringsSep concatStringsSep escapeShell hasSuffix mapAttrs
    optionalAttrs;

  test = { file, name }:
    runCommand name { inherit file; } (concatStringsSep "\n" (attrValues
      (mapAttrs (testName: t:
        if elem testName [ "override" "overrideDerivation" ] then
          ""
        else
          ''${t} "$file"'') pageTests) ++ [ ''ln -s "$file" "$out"'' ]));
};
{ file, inputs ? [ ], name, SOURCE_PATH, TO_ROOT ? "", unfinished ? false
, vars ? { }, }:
with rec {
  md = metadata.of file;
  extraPkgs = map (n:
    if hasAttr n commands then
      getAttr n commands
    else
      newScope (nix-helpers // warbo-packages) (withArgs [ n ] (getAttr n)) { })
    (md.packages or [ ]);

  dir = if md ? dependencies then
    nixpkgs-lib.fileset.toSource {
      root = ../..;
      fileset =
        nixpkgs-lib.fileset.unions (map (p: ../.. + "/${p}") md.dependencies);
    }
  else
    emptyDirectory;

  # If the page specifies a 'sha256' in its metadata, its renderer will be a
  # fixed-output derivation (hence allowing network access)
  hash = if md ? sha256 then {
    outputHashMode = "flat";
    outputHashAlgo = "sha256";
    outputHash = md.sha256;
  } else
    { };

  # If we're using a fixed-output derivation, its hash will be looked up in the
  # cache, even if we've changed the page. We would prefer such pages to be
  # rebuilt when changed, just in case their hash is out of date. To make this
  # happen, we append a hash of the page's content to its derivation name.
  prefix = if md ? sha256 then hashFile "sha256" file + "-" else "";

  untested = runCommand "untested-${prefix + name}" (vars // hash // {
    inherit dir file SOURCE_PATH TO_ROOT;
    buildInputs = inputs ++ extraPkgs
      ++ [ commands.relativise commands.render_page fail ];
    postprocessor = md.postprocessor or "";
  } // optionalAttrs (hasAttr "parseArgs" md) {
    parseArgF = writeScript "${name}-parseArgs.sh" ''
      parseArgs=()
      ${concatMapStringsSep "\n" (arg: "parseArgs+=(${escapeShellArg arg})")
      md.parseArgs}
    '';
  } // optionalAttrs (hasAttr "renderArgs" md) {
    renderArgF = writeScript "${name}-renderArgs.sh" ''
      renderArgs=()
      ${concatMapStringsSep "\n" (arg: "renderArgs+=(${escapeShellArg arg})")
      md.renderArgs}
    '';
  }) ''
    export DEST="$PWD/out.html"

    cd "$dir"
    SOURCE="$file" render_page
    grep '^.' < "$DEST" > /dev/null || fail "Error: No output when rendering"

    relativise < "$DEST" > "$out"
  '';

  rendered = if unfinished then
    untested
  else
    test {
      inherit name;
      file = untested;
    };
};
if hasSuffix ".html" file then
  with { data = writeScript name (readFile file); };
  if TO_ROOT == "" then data else relTo TO_ROOT data
else
  rendered
