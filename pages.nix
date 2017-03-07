{ callPackage, jq, latestGit, lib, makeWrapper, pandoc, panhandle, panpipe,
  pkgs, pythonPackages, runCommand, stdenv, wget, writeScript }:

with builtins;
with lib;
with rec { pages = rec {
  inDir = d: content: runCommand "in-dir-${d}" { inherit d content; } ''
    mkdir -p "$out"
    cp -r "$content" "$out/$d"
  '';

  attrsToDirs = attrs:
    mergeDirs (map (name: let val = attrs."${name}";
                           in inDir name (if isAttrs val
                                             then if val ? builder
                                                     then val
                                                     else attrsToDirs val
                                             else val))
                   (attrNames attrs));

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

  empty = runCommand "empty" {} ''mkdir -p "$out"'';

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
                           cp -r "$file" "$out/$REL"
                         '')
                   files);

  mergeTwo = a: b: runCommand "merged" { inherit a b; } ''
    shopt -s nullglob
    mkdir -p "$out"
    for F in "$a"/*
    do
      cp -r "$F" "$out"/
    done
    chmod +w -R "$out"
    for F in "$b"/*
    do
      cp -r "$F" "$out"/
    done
  '';

  # Copy the contents of a bunch of directories into one
  mergeDirs = dirs: runCommand "merged-dirs" { dirs = map toString dirs; } ''
    shopt -s nullglob
    mkdir -p "$out"

    for D in $dirs
    do
      for F in "$D"/*
      do
        cp -r "$F" "$out"/
      done
      chmod +w -R "$out"
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

  relTo = base: file: runCommand
    "relative-${baseNameOf (unsafeDiscardStringContext file)}"
    {
      inherit base file;
      buildInputs = [ commands.relativise ];
    }
    ''
      cp -r "$file" "$out"
      chmod +w -R "$out"

      DIR=$(echo "$base" | sed -e 's@/[^/][^/]*@/..@g')
      TO_ROOT="$DIR" relativise "$out"
    '';

  mkRel =
    with rec {
      go = base: name: val: if hasSuffix ".html" name
                               then relTo base val
                               else if isAttrs val
                                       then mapAttrs (go "${base}/${name}") val
                                       else val;
    };
    mapAttrs (go ".");

  mdToHtml = name: (removeSuffix ".html" (removeSuffix ".md" name)) + ".html";
  mdToHtmlRec = x: if isAttrs x
                      then mapAttrs' (n: v: { name  = if hasSuffix ".md" n
                                                         then mdToHtml n
                                                         else n;
                                              value = mdToHtmlRec v; })
                                     x
                      else x;

  blogPosts = with rec {
    # Read filenames from ./blog and append to the path './blog', so that each
    # is a standalone path. This way each post only depends on its own source,
    # and won't get rebuilt if e.g. a new post is added to ./blog.
    postNames = attrNames (readDir ./blog);
    posts = listToAttrs (map (p: { name  = mdToHtml p;
                                   value = ./blog + "/${p}"; }) postNames);
  }; mapAttrs (n: v: render { file = v; name = "blog-${n}"; }) posts;

  blog = attrsToDirs blogPosts;

  renderAll = x: mdToHtmlRec
                   (mapAttrs (n: v: if isAttrs v
                                       then renderAll v
                                       else render { file = v;
                                                     name = mdToHtml n; })
                             x);

  repoUrls = import (runCommand "repos.nix" { buildInputs = [ wget ]; } ''
    echo "Looking up repos from chriswarbo.net/git" 1>&2

    echo "[" >> "$out"

    wget -O- 'http://chriswarbo.net/git'       |
    grep -o '<a .*</a>'                        |
    grep -o 'href=".*\.git/"'                  |
    grep -o '".*"'                             |
    grep -o '[^"/]*'                           |
    sed  -e 's@^@http://chriswarbo.net/git/@g' >> "$out"

    echo "]" >> "$out"
  '');

  repoPageOf = url: runCommand "${removeSuffix ".git" (baseNameOf url)}.html"
    {
      inherit url;
      buildInputs = [ commands.git2md commands.render_page ];
      repo        = latestGit { inherit url; };
    }
    ''
      NAME=$(basename "$url" .git)

      DATE=""

      CONTENT=$(LATEST="$DATE" git2md "$NAME")
      CODE="$?"

      if [[ "$CODE" -eq 100 ]]
      then
          echo "Skipping regeneration of '$NAME'" 1>&2
      else
          echo "Got new content for '$NAME', generating" 1>&2
          echo "$CONTENT" | SOURCE= DEST= render_page > "$out"
      fi
    '';

  repoPages = listToAttrs (map (url: { name  = removeSuffix ".git"
                                                 (baseNameOf url) + ".html";
                                       value = repoPageOf url; })
                               repoUrls);

  repos = repoPages // {
    "index.html" = render {
      file = ./repos.md;
      name = "index.html";
      cwd  = attrsToDirs { repos = repoPages; };
    };
  };

  projects = attrsToDirs (renderAll (dirsToAttrs ./projects //
                                      { inherit repos; }));

  unfinished = attrsToDirs (renderAll (dirsToAttrs ./unfinished));

  topLevel = {
    "index.html"      = render {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./index.md;
    };
    "blog.html"       = render {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./blog.md;
    };
    "contact.html"    = render {
      file = ./contact.md;
    };
    "projects.html"   = render {
      cwd  = attrsToDirs { rendered = { inherit projects; }; };
      file = ./projects.md;
    };
    "unfinished.html" = render {
      cwd  = attrsToDirs { rendered = { inherit unfinished; }; };
      file = ./unfinished.md;
    };
  };

  resources = { css = ./css; js = ./js; };

  allPages = attrsToDirs (topLevel // resources // {
               inherit blog projects unfinished;
             });

  site = mkRel allPages;
}; }; pages
