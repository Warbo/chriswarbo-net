{ haskellPackages, lib, runCommand, writeScript }:

with builtins;
with lib;
with rec {
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

  metadataMapYaml = runCommand "metadata-map.yaml"
    {
      __noChroot  = true;  # Allow access to filesystem
      cacheBust   = toString currentTime;
      paths       = writeScript "markdown-paths"
        (concatStringsSep "\n" markdownPaths);
    }
    ''
      printf 'Looking for YAML metadata' 1>&2
      PAT='^----*'
      while read -r P
      do
        printf '.' 1>&2
        YAML=$(grep -B 99999 -m 2 "$PAT" < "$P" |
               grep -v "$PAT") || true
        if [[ -z "$YAML" ]]
        then
          YAML='{}'
        fi
        INDENTED=$(echo "$YAML" | sed -e 's/^/  /')

        printf '"%s":\n%s\n' "$P" "$INDENTED" >> "$out"
      done < "$paths"
    '';

  metadataMapFile = runCommand "metadata-map"
    {
      inherit metadataMapYaml;
      __noChroot  = true;  # Allow access to filesystem
      buildInputs = [ haskellPackages.yaml ];
      default     = writeScript "metadata.nix" ''
        with builtins;
        fromJSON (readFile ./metadata.json)
      '';
    }
    ''
      mkdir "$out"
      cp "$default" "$out/default.nix"

      echo "Converting to JSON" 1>&2
      yaml2json - < "$metadataMapYaml" > "$out/metadata.json"
    '';

  metadataMap = import metadataMapFile;
};
{
  inherit markdownPaths metadataMapFile metadataMap;
  of = path: metadataMap."${toString path}" or {};
}
