{ jq, lib, parallel, runCommand, wrap, writeScript, yq }:

with rec {
  inherit (builtins) attrNames currentTime filter getAttr map readDir toJSON toString;
  inherit (lib) concatMapStringsSep concatStringsSep escapeShellArg fold hasPrefix hasSuffix;

  markdownPaths =
    with rec {
      dirEntries = dir:
        with { entries = readDir dir; };
        map (entry: {
              path = dir + "/${entry}";
              type = getAttr entry entries;
            })
            # Avoid hidden files like .asv
            (filter (n: !(hasPrefix "." n))
                    (attrNames entries));

      go = { path, type }: rest:
        if type == "directory"
           then fold go rest (dirEntries path)
           else rest ++ (if hasSuffix ".md" (toString path)
                            then [ (toString path) ]
                            else [                 ]);
    };
    go { path = ../..; type = "directory"; } [];

  metadataMapFile = runCommand "metadata-map"
    {
      default = writeScript "metadata.nix" ''
        with builtins;
        fromJSON (readFile ./metadata.json)
      '';
      buildInputs = [ yq ];
      extract     = wrap {
        name   = "extract-yaml";
        #paths  = [ ];
        script = ''
          #!/usr/bin/env bash
          set -e
          PAT='^----*'
          YAML=$(grep -B 99999 -m 2 "$PAT" < "$2" |
               grep -v "$PAT") || true
          if [[ -z "$YAML" ]]
          then
            echo "$1: {}"
          else
            echo "$1:"
            echo "$YAML" | sed -e 's/^/  /g'
          fi
        '';
      };
    }
    ''
      mkdir "$out"
      cp "$default" "$out/default.nix"

      printf 'Looking for YAML metadata' 1>&2
      {
        ${concatMapStringsSep "\n"
          (p: ''
            "$extract" ${escapeShellArg p} ${escapeShellArg "${/. + p}"}
          '')
          markdownPaths}
      } | yq '.' > "$out/metadata.json"
    '';

  metadataMap = import metadataMapFile;
};
{
  inherit markdownPaths metadataMapFile metadataMap;
  of = path: metadataMap."${toString path}" or {};
}
