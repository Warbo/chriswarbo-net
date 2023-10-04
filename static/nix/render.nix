{ commands, emptyDirectory, fail, lib, metadata, newScope, nix-helpers
, nixpkgs-lib, pageTests, relTo, runCommand, warbo-packages, withArgs
, writeScript }:

with rec {
  inherit (builtins) attrValues elem getAttr hasAttr hashFile map readFile;
  inherit (lib)
    concatMapStringsSep concatStringsSep escapeShellArg hasSuffix mapAttrs
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
  # happen, we append a hash of the page's content to its derivation name, so
  # when the content changes, so does the name (and hence the store path).
  prefix = with {
    # When we hash the file, we need to strip out the `sha256:` line, so that
    # when we plug in the correct hash, that won't cause the file's hash to
    # change!
    # We also add the paths to our processing commands, so any changes to those
    # will cause rebuilds too.
    # See https://stackoverflow.com/a/23696995/884682 for the awk command
    stripHash = runCommand "strip-hash-${name}" { inherit file; } ''
      {
        echo ${commands.render_page}
        awk '!/^sha256:/ || f++' < "$file"
      } > "$out"
    '';
  };
    if md ? sha256 then "${hashFile "sha256" "${stripHash}"}-" else "";

  # A "raw" page has been rendered, but not postprocessed or checked. We do that
  # in separate derivations, to prevent tweaks from triggering the whole site to
  # re-render (note that, due to Panpipe, each page can take arbitrarily long!)
  raw = runCommand "raw-${prefix + name}" (vars // hash // {
    inherit dir file SOURCE_PATH;
    buildInputs = inputs ++ extraPkgs ++ [ commands.render_page fail ];

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
    mv "$DEST" "$out"
  '';

  untested = runCommand "untested-${name}" {
    inherit raw TO_ROOT;
    buildInputs = [
      commands.cleanup
      commands.relativise
    ]
    # If postprocessor is given, it will usually refer to a commands element
      ++ (if md ? postprocessor && hasAttr md.postprocessor commands then
        [ (getAttr md.postprocessor commands) ]
      else
        [ ]);
    postprocessor = md.postprocessor or "cat";
  } ''
    # Perform some post-processing steps: we always run cleanup and relativise,
    # and pages are free to set another postprocessor in their YAML (defaults
    # to 'cat', which passes its stdio along unchanged)
    < "$raw" cleanup | "$postprocessor" | relativise > "$out"
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
