{ callPackage, jq, lib, makeWrapper, pandoc, panhandle, panpipe, pkgs,
  pythonPackages, runCommand, stdenv, writeScript }:

with builtins;
with lib;
with rec { pages = rec {
  attrsToDirs = attrs:
    with rec {
      names     = attrNames attrs;
      dataOf    = name: attrsToDirs attrs."${name}";
      nameToCmd = name: ''
        cp -r "${dataOf name}" "$out/${name}"
      '';
    };
    if typeOf attrs == "path" || attrs ? builder
       then attrs
       else stdenv.mkDerivation {
              name = "collated-data";
              buildCommand = ''
                mkdir -p "$out"
                ${concatStringsSep "\n" (map nameToCmd names)}
              '';
            };

  dirsToAttrs = dir: mapAttrs (n: v: if v == "regular"
                                        then dir + "/${n}"
                                        else dirsToAttrs (dir + "/${n}"))
                              (readDir dir);

  cleanup = runCommand "cleanup"
    {
      cleanup = ./static/cleanup;
      buildInputs = [ makeWrapper ];
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$cleanup" "$out/bin/cleanup"
    '';

  commands = callPackage ./commands.nix {};

  empty  = attrsToDirs {};

  metadata =
    with rec {
      yaml2json = writeScript "yaml2json.py" ''
        import sys, yaml, json
        json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)
      '';

      read = file: runCommand "metadata-${baseNameOf file}.json"
               {
                 inherit file yaml2json;
                 buildInputs = [ pythonPackages.python pythonPackages.pyyaml ];
               }
               ''
                 # Look for a YAML section between "---" lines

                 # Get line numbers matching "---"
                 LINES=$(grep -n "^---[-]*$" < "$file") || {
                   echo "{}" > "$out"
                   exit 0
                 }

                 NUMS=$(echo "$LINES" | cut -d ':' -f 1 | head -n2)
                 START=$(echo "$NUMS" | head -n1)
                   END=$(echo "$NUMS" | tail -n1)

                 if [[ -n "$START" ]] && [[ -n "$END" ]]
                 then
                   head -n$(( END - 1         )) < "$file" |
                   tail -n$(( END - 1 - START ))           |
                   python "$yaml2json" > "$out"
                 else
                   echo "{}" > "$out"
                 fi
               '';
    }; file: fromJSON (readFile "${read file}");

  # Create a directory containing 'files'; the directory structure will be
  # relative to 'base', for example:
  #
  #   dirContaining /foo/bar [ /foo/bar/baz /foo/bar/quux/foobar ]
  #
  # Will produce a directory containing 'baz' and 'quux/foobar'.
  dirContaining = base: files:
    mergeDirs (map (f: runCommand "dir"
                         {
                           base = toString base;
                           file = toString base + "/${f}";
                         }
                         ''
                           REL=$(echo "$file" | sed -e "s@$base/@@g")
                           DIR=$(dirname "$REL")
                           mkdir -p "$out/$DIR"
                           cp "$file" "$out/$REL"
                         '') files);

  # Copy the contents of a bunch of directories into one
  mergeDirs = dirs:
    if dirs == []
       then empty
       else runCommand "merged" { a = head dirs; b = mergeDirs (tail dirs); } ''
         shopt nullglob
         mkdir -p "$out"
         for D in "$a" "$b"
         do
           for F in "$D"/*
           do
             cp -r "$F" "$out/"
             chmod +w -R "$out"
           done
         done
       '';

  render = { cwd ? empty, file, inputs ? [], name ? "page.html", vars ? {} }:
    with rec {
      md        = metadata file;
      extraPkgs = map (n: (pkgs // commands // pages)."${n}")
                      (md.packages or []);
      dir       = mergeDirs [ cwd (dirContaining ./. (md.dependencies or [])) ];
    };
    if hasSuffix ".html" file
       then writeScript name (readFile file)
       else runCommand name (vars // {
              buildInputs = [ commands.render_page ] ++ inputs ++ extraPkgs;
              inherit dir file;
            })
            ''
              cd "$dir"
              SOURCE="$file" DEST="$out" render_page < "$file" > "$out"
            '';

  rel = dir: runCommand "relative" { buildInputs = [ commands.relativise ]; } ''
    cp -r "${dir}" "$out"
    chmod +w -R "$out"
    cd "$out"

    while read -r F
    do
      DIR=$(dirname "$F" | sed -e 's@/[^/][^/]*@/..@g')
      TO_ROOT="$DIR" relativise "$F"
    done < <(find . -name "*.html")
  '';

  index = render {
    cwd  = attrsToDirs { rendered = { inherit blog; }; };
    file = ./index.md;
    name = "index.html";
  };

  mdToHtml = name: (removeSuffix ".html" (removeSuffix ".md" name)) + ".html";

  blogPosts = with rec {
    # Read filenames from ./blog and append to the path './blog', so that each
    # is a standalone path. This way each post only depends on its own source,
    # and won't get rebuilt if e.g. a new post is added to ./blog.
    postNames = attrNames (readDir ./blog);
    posts = listToAttrs (map (p: { name  = mdToHtml p;
                                   value = ./blog + "/${p}"; }) postNames);
  }; mapAttrs (n: v: render { file = v; name = "blog-${n}"; }) posts;

  blog = attrsToDirs blogPosts;

  renderAll = mapAttrs (n: v: if isAttrs v
                                 then renderAll v
                                 else render { file = v;
                                               name = mdToHtml n; });

  projects = attrsToDirs (renderAll (dirsToAttrs ./projects));

  site = rel (attrsToDirs {
    inherit blog projects;
    "index.html" = index;
  });
}; }; pages
