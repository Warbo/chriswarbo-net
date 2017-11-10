{ allDrvsIn, attrsToDirs, callPackage, dirsToAttrs, git, git2html, hfeed2atom,
  ipfs, isPath, jq, latestConfig, lib, mkBin, nothing, pages, pkgs, python,
  repoSource, reverse, runCommand, sanitiseName, stdenv, wget, withDeps, wrap,
  writeScript, xidel }:

with builtins;
with lib;
rec {
  bins = bin: attrsToDirs { inherit bin; };

  cleanup = bins { cleanup = ./static/cleanup; };

  commands = callPackage ./commands.nix {};

  metadata =
    with rec {
      yaml2json = mkBin {
        name   = "yaml2json";
        paths  = [ (python.withPackages (p: [ p.pyyaml ])) ];
        script = ''
          #!/usr/bin/env python
          import json, re, StringIO, sys, yaml
          bits = re.split("^----*$", sys.stdin.read(), flags=re.MULTILINE)
          if len(bits) < 3:
            print '"{}"'
          else:
            data = yaml.load(StringIO.StringIO(filter(None, bits)[0]))
            print json.dumps(json.dumps(data))
        '';
      };

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

      read = file: runCommand "metadata-${prettyName file}.nix"
               {
                 inherit file;
                 buildInputs = [ yaml2json ];
               }
               ''
                 set -e
                 yaml2json < "$file" > "$out"
               '';
    }; file: fromJSON (import (read file));

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

  render = { cwd ? nothing, file, inputs ? [], name ? "page.html", vars ? {},
             relBase ? null, SOURCE_PATH ? "" }:
    with rec {
      md        = metadata file;
      extraPkgs = map (n: (pkgs // commands // pages)."${n}")
                      (md.packages or []);
      dir       = mergeDirs [ cwd (dirContaining ./. (md.dependencies or [])) ];
      rel       = if relBase == null
                     then (x: x)
                     else relTo relBase;
      dupe      = rel (writeScript name (readFile file));
      rendered  = runCommand name
        (vars // {
          buildInputs = [ commands.relativise commands.render_page ] ++
                        inputs ++ extraPkgs;
          inherit dir file SOURCE_PATH;
          TO_ROOT = if relBase == null then "" else relBase;
        })
        ''
          export DEST="$PWD/out.html"

          cd "$dir"
          SOURCE="$file" render_page < "$file"
          if grep '^.' < "$DEST" > /dev/null
          then
            if [[ -n "$TO_ROOT" ]]
            then
              relativise < "$DEST" > "$out"
            else
              mv "$DEST" "$out"
            fi
          else
            echo "No output when rendering, aborting" 1>&2
            exit 1
          fi
        '';
    };
    if hasSuffix ".html" file
       then dupe
       else rendered;

  relTo =
    with rec {
      relToTest = runCommand "relative"
        {
          buildInputs = [ xidel ];
          page        = relToUntested "." (writeScript "test.html" ''
            <html>
              <head>
                <link rel="stylesheet" type="text/css"
                      href="/css/default.css" />
                <meta http-equiv="refresh" content="0;URL=./projects.html" />
              </head>
              <body>
                <a href="/blog.html">Blog</a>
                <script src="/js/foo.js"></script>
                <img src="/images/foo.png" />
              </body>
            </html>
          '');
        }
        ''
          set -e

          function fail() {
            echo "Found absolute paths in $1" 1>&2
            exit 1
          }

          for PAIR in '//a/@href	anchors' \
                      '//link/@href	links'   \
                      '//script/@src	scripts' \
                      '//img/@src	images'  \
                      '//meta/@content	meta'

          do
            QUERY=$(echo "$PAIR" | cut -f1)
             TYPE=$(echo "$PAIR" | cut -f2)
            FOUND=$(xidel -q -e "$QUERY" - < "$page")

            echo "$FOUND" | grep "^/" || continue
            echo "$FOUND" | grep "=/" || continue
            fail "$TYPE"
          done

          if grep 'var url = "/' < "$page"
          then
            fail "script vars"
          fi

          mkdir "$out"
        '';

      relToUntested = TO_ROOT: file: runCommand
        "relative-${baseNameOf (unsafeDiscardStringContext file)}"
        {
          inherit file TO_ROOT;
          buildInputs = [ commands.relativise ];
        }
        ''
          echo "Relativising $file to $TO_ROOT" 1>&2
          relativise < "$file" > "$out"
        '';
    };
    x: y: withDeps [ relToTest ] (relToUntested x y);

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

  blog =
    with rec {
      # Read filenames from ./blog and append to the path './blog', so that each
      # is a standalone path. This way each post only depends on its own source,
      # and won't get rebuilt if e.g. a new post is added to ./blog.
      postNames = attrNames (readDir ./blog);
      posts     = listToAttrs (map (p: {
                                     name  = mdToHtml p;
                                     value = ./blog + "/${p}";
                                   })
                                   postNames);
    };
    mapAttrs (n: v: render {
               file        = v;
               name        = "blog-${n}";
               SOURCE_PATH = "blog/${htmlToMd (baseNameOf n)}";
               relBase     = "./..";
             })
             posts;

  renderAll = prefix: x: mdToHtmlRec (mapAttrs
    (n: v: if isDerivation v || isPath v
              then render {
                     file        = v;
                     name        = mdToHtml n;
                     SOURCE_PATH = concatStringsSep "/" (prefix ++ [n]);
                     relBase     = concatStringsSep "/" (["."] ++ map (_: "..")
                                                                      prefix);
                   }
              else if isAttrs v
                      then renderAll (prefix ++ [n]) v
                      else abort "Can't render ${toJSON { inherit n v; }}")
    x);

  projects     = renderAll ["projects"] (dirsToAttrs ./projects) //
                 { repos = projectRepos; };

  unfinished   = renderAll ["unfinished"] (dirsToAttrs ./unfinished);

  topLevel     = mapAttrs' (name: val: {
                             inherit name;
                             value = render (val // {
                               inherit name;
                               relBase = ".";
                             });
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
      go = paths: entry: content:
        if isPath content || isDerivation content
           then relTo (concatStringsSep "/" (["."] ++ map (_: "..") paths))
                      (mkRedirectTo {
                        from = sanitiseName entry;
                        to   = concatStringsSep "/" ([""] ++ paths ++ [entry]);
                      })
           else mapAttrs (go (paths ++ [entry])) content;

      # These pages now live in projects/ but there are links in the wild
      # without that prefix. We single them out here to avoid proliferating new
      # redirects for new pages which don't have this compatibility issue.
      toplevelRedirects = [
        "activecode" "arduino" "maze" "nixos" "optimisation" "plumb" "powerplay"
        "procedural" "turtleview"
      ];

      projectDirs = filter (name: let val = getAttr name projects;
                                   in isAttrs val &&
                                      (!(isDerivation val)) &&
                                      elem name toplevelRedirects)
                           (attrNames projects);

      redirectDir = entry: {
        "index.html" = relTo "." (mkRedirectTo {
          from = "redirect-${sanitiseName entry}";
          to   = "/projects/${entry}/index.html";
        });
      };

      oldLinks = listToAttrs (map (name: { inherit name;
                                           value = redirectDir name; })
                                  projectDirs);

      mkRedirectTo = { from, to }: runCommand from
        {
          inherit to;
          buildInputs = [ commands.mkRedirectTo ];
        }
        ''
          # We make sure the redirection succeeds before we do anything to $out,
          # to avoid creating empty or partial files
          RESULT=$(mkRedirectTo "$to")
          echo "$RESULT" > "$out"
        '';
    };
    oldLinks // {
      data_custom = {
        "prelude.txt" = mkRedirectTo {
          from = "prelude.txt";
          to   = "/git/php-prelude";
        };
      };

      essays = mapAttrs (go ["projects"]) projects;

      "essays.html" = mkRedirectTo {
        from = "essays.html";
        to   = "projects.html";
      };
    };

  allPages = topLevel // redirects // resources //
    {
      inherit blog projects unfinished;
      "index.php" = render {
        cwd  = attrsToDirs { rendered = { inherit blog; }; };
        file = ./redirect.md;
        name = "index.php";
      };
    };

  tests        = callPackage ./tests.nix { inherit pages repoPages; };

  wholeSite    = withDeps (allDrvsIn tests) untestedSite;

  untestedSite = attrsToDirs allPages;

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
            inherit commands ipfsKeys render repoUrls;
          })
    projectRepos repoName repoPages;

  inherit (callPackage ./ipfs.nix {
            inherit allPages attrsToDirs bins commands repoName repoUrls;
          })
    ipfsHash ipfsKeys;
}
