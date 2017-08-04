{ attrsToDirs, callPackage, dirsToAttrs, git, git2html, hfeed2atom, ipfs,
  isPath, jq, latestConfig, lib, pages, pkgs, pythonPackages, repoRefs,
  repoSource, reverse, runCommand, sanitiseName, stdenv, wget, wrap,
  writeScript, xidel }:

with builtins;
with lib;
rec {
  bins = bin: attrsToDirs { inherit bin; };

  cleanup = bins { cleanup = ./static/cleanup; };

  commands = callPackage ./commands.nix {};

  empty = runCommand "empty" {} ''mkdir -p "$out"'';

  metadata =
    with rec {
      yaml2json = writeScript "yaml2json.py" ''
        import sys, yaml, json
        json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)
      '';

      # Choose a nice name, and avoid 'cannot refer to store path' errors
      prettyName = file:
        with rec {
          base       = baseNameOf file;
          bits       = splitString "redirect-to-" base;
          isRedirect = length bits != 1;
          name       = unsafeDiscardStringContext (head (reverse bits));
        };
        if isRedirect
           then name
           else base;

      read = file: runCommand "metadata-${prettyName file}.json"
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

  # Create a directory containing (links to) 'files'; the directory structure
  # will be relative to 'base', for example:
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
                           ln -s "$file" "$out/$REL"
                         '')
                   files);

  # Link the contents of a bunch of directories into one
  mergeDirs = dirs: runCommand "merged-dirs" { dirs = map toString dirs; } ''
    shopt -s nullglob
    mkdir -p "$out"

    for D in $dirs
    do
      for F in "$D"/*
      do
        cp -as "$F" "$out"/
      done
      chmod +w -R "$out"
    done
  '';

  render = { cwd ? empty, file, inputs ? [], name ? "page.html", vars ? {},
             SOURCE_PATH ? "" }:
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
              inherit dir file SOURCE_PATH;
            })
            ''
              export DEST="$PWD/out.html"

              cd "$dir"
              SOURCE="$file" render_page < "$file"
              if grep '^.' < "$DEST" > /dev/null
              then
                mv "$DEST" "$out"
              else
                echo "No output when rendering, aborting" 1>&2
                exit 1
              fi
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

  # Turn ".html" into ".md"
  htmlToMd = name: (removeSuffix ".html" (removeSuffix ".html" name)) + ".md";

  blog = with rec {
    # Read filenames from ./blog and append to the path './blog', so that each
    # is a standalone path. This way each post only depends on its own source,
    # and won't get rebuilt if e.g. a new post is added to ./blog.
    postNames = attrNames (readDir ./blog);
    posts     = listToAttrs (map (p: { name  = mdToHtml p;
                                       value = ./blog + "/${p}"; }) postNames);
  };
  mapAttrs (n: v: render {
             file        = v;
             name        = "blog-${n}";
             SOURCE_PATH = "blog/${htmlToMd (baseNameOf n)}";
           })
           posts;

  renderAll = prefix: x: mdToHtmlRec (mapAttrs
    (n: v: if isDerivation v || isPath v
              then render {
                     file        = v;
                     name        = mdToHtml n;
                     SOURCE_PATH = "${prefix}/${n}";
                   }
              else if isAttrs v
                      then renderAll "${prefix}/${n}" v
                      else abort "Can't render ${toJSON { inherit n v; }}")
    x);

  projects     = renderAll "projects" (dirsToAttrs ./projects) //
                 { repos = projectRepos; };

  unfinished   = renderAll "unfinished" (dirsToAttrs ./unfinished);

  topLevel     = mapAttrs' (name: val: {
                         inherit name;
                         value = render (val // { inherit name; });
                       }) {
    "index.html"      = {
      cwd         = attrsToDirs { rendered = { inherit blog; }; };
      file        = ./index.md;
      SOURCE_PATH = "index.md";
    };
    "blog.html"       = {
      cwd         = attrsToDirs { rendered = { inherit blog; }; };
      file        = ./blog.md;
      SOURCE_PATH = "blog.md";
    };
    "contact.html"    = {
      file        = ./contact.md;
      SOURCE_PATH = "contact.md";
    };
    "projects.html"   = {
      cwd         = attrsToDirs { rendered = { inherit projects; }; };
      file        = ./projects.md;
      SOURCE_PATH = "projects.md";
    };
    "unfinished.html" = {
      cwd         = attrsToDirs { rendered = { inherit unfinished; }; };
      file        = ./unfinished.md;
      SOURCE_PATH = "unfinished.md";
    };
  };

  resources = with rec {
    atom = runCommand "blog.atom"
      {
        blog        = topLevel."blog.html";
        buildInputs = [ hfeed2atom ];
      }
      ''
        ATOM=$("${./static/mkAtom}" < "$blog")

        if [[ "x$ATOM" = "xNone" ]]
        then
          echo "Failed to produce blog.atom. Output: $ATOM" 1>&2
          exit 1
        fi

        echo "$ATOM" > "$out"
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

  redirects =
    with rec {
      go = path: entry: content:
        if isPath content || isDerivation content
           then runCommand "${sanitiseName entry}"
                  {
                    inherit path entry;
                    buildInputs = [ commands.mkRedirectTo ];
                  }
                  ''
                    PRJ_URL="$path/$entry"
                    RESULT=$(mkRedirectTo "$PRJ_URL")
                    echo "$RESULT" > "$out"
                  ''
           else mapAttrs (go (path + "/" + entry)) content;

      # These pages now live in projects/ but there are links in the wild
      # without that prefix. We single them out here to avoid proliferating new
      # redirects for new pages which don't have this compatibility issue.
      toplevelRedirects = [
        "activecode" "arduino" "maze" "nixos" "optimisation" "plumb" "powerplay"
        "procedural" "turtleview"
      ];

      projectDirs = filter (name: let val = projects."${name}";
                                   in isAttrs val &&
                                      (!(isDerivation val)) &&
                                      elem name toplevelRedirects)
                           (attrNames projects);

      redirectDir = entry: {
        "index.html" = runCommand "redirect-${sanitiseName entry}"
          {
            inherit entry;
            buildInputs = [ commands.mkRedirectTo ];
          }
          ''
            RESULT=$(mkRedirectTo "/projects/$entry/index.html")
            echo "$RESULT" > "$out"
          '';
        };

      oldLinks = listToAttrs (map (name: { inherit name;
                                           value = redirectDir name; })
                                  projectDirs);
    };
    oldLinks // {
      essays = mapAttrs (go "/projects") projects;

      "essays.html" = runCommand "mk-essays.html"
        { buildInputs = [ commands.mkRedirectTo ];}
        ''
          RESULT=$(mkRedirectTo "projects.html")
          echo "$RESULT" > "$out"
        '';
    };

  allPagesUntested = mkRel (redirects // topLevel // resources // {
    inherit blog projects unfinished;
    "index.php" = render {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./redirect.md;
      name = "index.php";
    };
  });

  allPages = assert testsPass; allPagesUntested;

  strip = filterAttrs (n: v: !(elem n [ "override" "overrideDerivation" ]));

  untested = attrsToDirs allPagesUntested;

  tests = strip (callPackage ./tests.nix { inherit pages repoPages; });

  testsPass = import (runCommand "site"
                       {
                         # A failing test will fail to build
                         tests = attrsToDirs tests;
                       }
                       ''echo "true" > "$out"'');

  wholeSite = attrsToDirs allPages;

  untestedSite = attrsToDirs allPagesUntested;

  repoUrls = if isPath repoSource    ||
                (isString repoSource &&
                 repoSource != ""    &&
                 substring 0 1 repoSource == "/")
                then map (n: "${repoSource}/${n}")
                         (attrNames (filterAttrs (n: v: v == "directory" &&
                                                        hasSuffix ".git" n)
                                                 (readDir repoSource)))
                else import (runCommand "repoUrls.nix"
                              {
                                inherit repoSource;
                                buildInputs = [ jq wget xidel ];
                              }
                              ''
                                {
                                  echo "["
                                  wget -O- "$repoSource"   |
                                    xidel - -e '//a/@href' |
                                    grep '\.git'           |
                                    jq -R '.'
                                  echo "]"
                                } > "$out"
                              '');

  inherit (callPackage ./repos.nix {
            inherit commands ipfsKeys render repoRefs repoUrls;
          })
    projectRepos repoName repoPages;

  inherit (callPackage ./ipfs.nix {
            inherit allPages attrsToDirs bins commands repoName repoUrls;
          })
    ipfsHash ipfsKeys;
}
