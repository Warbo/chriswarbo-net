{ haskell, haskellPackages, jq, lib, parallel, runCommand, wrap, writeScript }:

with rec {
  inherit (builtins) attrNames currentTime filter getAttr map readDir toString;
  inherit (lib) concatStringsSep fold hasPrefix hasSuffix;

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
      __noChroot  = true;  # Allow access to filesystem
      cacheBust   = toString currentTime;
      paths       = writeScript "markdown-paths"
        (concatStringsSep "\n" markdownPaths);
      default     = writeScript "metadata.nix" ''
        with builtins;
        fromJSON (readFile ./metadata.json)
      '';
      buildInputs = [ jq parallel ];
      convert     = wrap {
        name   = "yaml-to-json";
        paths  = [
          (haskell.lib.disableCabalFlag haskellPackages.yaml "no-exe")
        ];
        script = ''
          #!/usr/bin/env bash
          set -e
          PAT='^----*'
          YAML=$(grep -B 99999 -m 2 "$PAT" < "$1" |
               grep -v "$PAT") || true
          if [[ -z "$YAML" ]]
          then
            YAML='{}'
          fi
          INDENTED=$(echo "$YAML" | sed -e 's/^/  /')

          printf '"%s":\n%s\n' "$1" "$INDENTED" | yaml2json -
        '';
      };
    }
    ''
      mkdir "$out"
      cp "$default" "$out/default.nix"

      printf 'Looking for YAML metadata' 1>&2
      parallel --will-cite "$convert" < "$paths" |
        jq -s 'add' > "$out/metadata.json"
    '';

  metadataMap = import metadataMapFile;
};
{
  inherit markdownPaths metadataMapFile metadataMap;
  of = path: metadataMap."${toString path}" or {};
}
