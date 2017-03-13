{ callPackage, hfeed2atom, jq, latestConfig, latestGit, lib, makeWrapper, nix,
  pandoc, panhandle, panpipe, pkgs, pythonPackages, runCommand, stdenv,
  tidy-html5, wget, writeScript }:

with builtins;
with lib;
with rec { pages = rec {

  # Builds a directory whose entries/content correspond to the names/values of
  # the given attrset. When a value is an attrset, the corresponding entry is a
  # directory, whose contents is generated with attrsToDirs on that value.
  attrsToDirs =
    with {
      inDir = d: content: runCommand "in-dir-${d}" { inherit d content; } ''
        mkdir -p "$out"
        cp -r "$content" "$out/$d"
      '';
    };
    attrs: mergeDirs (map (name: let val = attrs."${name}";
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

  reverse = lst: if lst == []
                    then []
                    else reverse (tail lst) ++ [(head lst)];

  withNix = attrs:
    attrs // {
      buildInputs = (attrs.buildInputs or []) ++ [ nix ];
      NIX_PATH    = getEnv "NIX_PATH";
      NIX_REMOTE  = getEnv "NIX_REMOTE";
    };

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

  relTo = TO_ROOT: file: runCommand
    "relative-${baseNameOf (unsafeDiscardStringContext file)}"
    {
      inherit file TO_ROOT;
      buildInputs = [ commands.relativise ];
    }
    ''
      echo "Relativising $file to $TO_ROOT" 1>&2
      relativise < "$file" > "$out"
    '';

  mkRel =
    with rec {
      go = base: name: val: if hasSuffix ".html" name
                               then relTo base val
                               else if isAttrs val
                                       then mapAttrs (go "${base}/..") val
                                       else val;
    };
    mapAttrs (go ".");

  # Turn ".md" names in to ".html"
  mdToHtml = name: (removeSuffix ".html" (removeSuffix ".md" name)) + ".html";
  mdToHtmlRec = x: if isAttrs x
                      then mapAttrs' (n: v: { name  = if hasSuffix ".md" n
                                                         then mdToHtml n
                                                         else n;
                                              value = mdToHtmlRec v; })
                                     x
                      else x;

  blog = with rec {
    # Read filenames from ./blog and append to the path './blog', so that each
    # is a standalone path. This way each post only depends on its own source,
    # and won't get rebuilt if e.g. a new post is added to ./blog.
    postNames = attrNames (readDir ./blog);
    posts     = listToAttrs (map (p: { name  = mdToHtml p;
                                       value = ./blog + "/${p}"; }) postNames);
  }; mapAttrs (n: v: render { file = v; name = "blog-${n}"; }) posts;

  renderAll = x: mdToHtmlRec
                   (mapAttrs (n: v: if isAttrs v
                                       then renderAll v
                                       else render { file = v;
                                                     name = mdToHtml n; })
                             x);

  # Look up available git repos and generate a page for each, including the
  # contents of any READMEs we find.
  repos =
    with rec {
      repoUrls = import (runCommand "repos.nix" { buildInputs = [ wget ]; } ''
        echo "Looking up repos from chriswarbo.net/git" 1>&2

        echo "[" >> "$out"

        wget -O- 'http://chriswarbo.net/git'         |
          grep -o '<a .*</a>'                        |
          grep -o 'href=".*\.git/"'                  |
          grep -o '".*"'                             |
          grep -o '[^"/]*'                           |
          sed  -e 's@^@http://chriswarbo.net/git/@g' >> "$out"

        echo "]" >> "$out"
      '');

      cleanUp = writeScript "cleanUp" ''
        #!/usr/bin/env bash
        # READMEs might contain nonsense that's out of our control; send it
        # through tidy to fix it up and prevent downstream tools bailing out.
        TIDY_INPUT=$(cat)

        # Ignore errors, as they're probably not ours
        if echo "$TIDY_INPUT" | tidy -q
        then
          true
        else
          CODE="$?"

          # Ignore warnings but abort on errors
          if [[ "$CODE" -eq 1 ]]
          then
            true
          else
            echo -e "Content:\n$TIDY_INPUT" 1>&2
            exit 1
          fi
        fi
      '';

      repoPageNix = writeScript "repo-page.nix" ''
        with import <nixpkgs> { config = import ${latestConfig}; };
        with builtins;
        with lib;

        { url }: runCommand
          ((removeSuffix ".git" (baseNameOf url)) + ".html")
          {
            inherit url;
            buildInputs = [ tidy-html5 ];
            repo        = latestGit { inherit url; };
          }
          (readFile ${writeScript "git2md" ''
            set -o pipefail

            echo "Rendering page for $url" 1>&2
            NAME=$(basename "$url" .git)
            ${commands.git2md}/bin/git2md "$NAME"                   |
              sed -e 's/</\&lt;/g'                                  |
              sed -e 's/>/\&gt;/g'                                  |
              SOURCE= DEST= ${commands.render_page}/bin/render_page |
              "${cleanUp}"                                          > "$out"
          ''})
      '';

      repoPageOf = url:
        with rec {
          name  = removeSuffix ".git" (baseNameOf url);

          # The maximum time that a repo page will be cached for, in seconds
          maxCache = 60 * 60 * 24 * 30;  # 30 days

          # Derive a number between 0 and 99 from the digits of this URL's hash
          hashN     = concatStrings (take 2 (reverse (filter digit hashChars)));
          hashChars = stringToCharacters ("0" + hashString "sha256" url);
          digit     = c: elem c (stringToCharacters "0123456789");

          # Scale hashN from [0,99] to [0,maxCache]
          extra = toInt hashN * (maxCache / 100);

          # Avoid stampedes by staggering timestamps using 'extra'
          time  = concatStrings [
            "cached-"
            (toString maxCache)
            "-"
            (toString ((currentTime + extra) / maxCache))
          ];

          # To force a page's generation, set UPDATE_REPOS to a JSON array
          # containing the repo's name, e.g. UPDATE_REPOS='["warbo-utilities"]'
          regen = if getEnv "UPDATE_REPOS" == ""
                     then false
                     else elem name (fromJSON (getEnv "UPDATE_REPOS"));

          mkDrv = ''DRV=$(nix-instantiate --argstr url "$url" ${repoPageNix})'';

          # If regen is true, we clear out any cached versions of this page
          clear = runCommand "clear" (withNix { inherit currentTime url; }) ''
            if ${if regen then "true" else "false"}
            then
              echo "Deleting cache ${time} of build of ${name}" 1>&2

              # Get the derivation for this page
              ${mkDrv}

              nix-store --delete "$DRV"
              printf '"%s"' $(date '+%s') > "$out"
            else
              echo '"cached"' > "$out"
            fi
          '';
        };

        runCommand "repo-page-${name}.html"
          (withNix {
            inherit url;
            cache = "cached-${time}";
            clear = import clear;
          })
          ''
            echo "No ${time} build found for ${name}, generating" 1>&2

            # Get the derivation for this page
            ${mkDrv}

            # Build the derivation (or use cached version)
            F=$(nix-store --realise "$DRV") || exit 1

            # Use as our output
            cp "$F" "$out"
          '';

      repoPages = listToAttrs (map (url: { name  = removeSuffix ".git"
                                                     (baseNameOf url) + ".html";
                                           value = repoPageOf url; })
                                   (take 1 repoUrls));
    };
    repoPages // {
      "index.html" = render {
        file = ./repos.md;
        name = "index.html";
        cwd  = attrsToDirs { repos = repoPages; };
      };
    };

  projects = renderAll (dirsToAttrs ./projects // { inherit repos; });

  unfinished = renderAll (dirsToAttrs ./unfinished);

  topLevel = mapAttrs' (name: val: {
                         inherit name;
                         value = render (val // { inherit name; });
                       }) {
    "index.html"      = {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./index.md;
    };
    "blog.html"       = {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./blog.md;
    };
    "contact.html"    = {
      file = ./contact.md;
    };
    "projects.html"   = {
      cwd  = attrsToDirs { rendered = { inherit projects; }; };
      file = ./projects.md;
    };
    "unfinished.html" = {
      cwd  = attrsToDirs { rendered = { inherit unfinished; }; };
      file = ./unfinished.md;
    };
  };

  resources = with rec {
    atom = runCommand "blog.atom"
      {
        blog        = topLevel."blog.html";
        buildInputs = [ hfeed2atom ];
      }
      ''
        "${./static/mkAtom}" < "$blog" > "$out"
      '';

    rss  = runCommand "blog.rss"
      {
        inherit atom;
        buildInputs = [ commands.mkRss ];
      }
      ''
        mkRss < "$atom" > "$out"
      '';
  };
  {
    "blog.atom" = atom;
    "blog.rss"  = rss;
    css         = ./css;
    js          = ./js;
  };

  addRedirects = site: dirsToAttrs (runCommand "with-redirects"
    {
      buildInputs = [ commands.mkEssayLinks ];
      site        = attrsToDirs site;
      index2      = render {
        cwd  = attrsToDirs { rendered = { inherit blog; }; };
        file = ./redirect.md;
        name = "index.php";
      };
    }
    ''
      cp -r "$site" "$out"
      chmod +w -R "$out"
      cp "$index2" "$out/index.php"
      cd "$out"
      mkEssayLinks
    '');

  allPages = mkRel (addRedirects (topLevel // resources // {
               inherit blog projects unfinished;
             }));

  strip = filterAttrs (n: v: !(elem n [ "override" "overrideDerivation" ]));

  tests = strip (callPackage ./tests.nix { inherit pages; });

  untested = attrsToDirs allPages;

  site = runCommand "site"
           {
             inherit untested;
             tests = attrsToDirs tests;
           }
           ''cp -r "$untested" "$out"'';
}; }; pages
