self: super:

with builtins;
with super.lib;
{
  bins = bin: self.attrsToDirs' "bins" { inherit bin; };

  cleanup = self.bins { cleanup = ./static/cleanup; };

  commands = self.callPackage ./commands.nix {};

  metadata =
    with rec {
      # Choose a nice name, and avoid 'cannot refer to store path' errors
      prettyName = file:
        with rec {
          base       = baseNameOf file;
          bits       = splitString "redirect-to-" base;
          isRedirect = length bits != 1;
          name       = unsafeDiscardStringContext (head (self.reverse bits));
        };
        if isRedirect
           then name
           else base;

      read = file: self.runCommand "metadata-${prettyName file}"
        {
          inherit file;
          buildInputs = [ self.haskellPackages.yaml ];
          nixExpr     = "with builtins; fromJSON (readFile ./metadata.json)";
        }
        ''
          set -e
          mkdir -p "$out"
          echo "$nixExpr" > "$out/default.nix"
          echo '{}' > "$out/metadata.json"

          PAT='^----*'
          YAML=$(grep -B 99999 -m 2 '^----*' < "$file" | grep -v "$PAT") || true
          [[ -z "$YAML" ]] || {
            echo "$YAML" | yaml2json - > "$out/metadata.json"
          }
        '';
    };
    file: import (read file);

  # Create a directory containing (links to) 'files'; the directory structure
  # will be relative to 'base', for example:
  #
  #   dirContaining /foo/bar [ /foo/bar/baz /foo/bar/quux/foobar ]
  #
  # Will produce a directory containing 'baz' and 'quux/foobar'.
  dirContaining = base: files:
    mergeDirs (map (f: self.runCommand "dir"
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
  mergeDirs = dirs: self.runCommand "merged-dirs" { dirs = map toString dirs; } ''
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

  render = { cwd ? self.nothing, file, inputs ? [], name ? "page.html", vars ? {},
             relBase ? null, SOURCE_PATH ? "" }:
    with rec {
      md        = self.metadata file;
      extraPkgs = map (n: getAttr n (self // self.commands))
                      (md.packages or []);
      dir       = self.mergeDirs [ cwd (self.dirContaining ./. (md.dependencies or [])) ];
      rel       = if relBase == null
                     then (x: x)
                     else self.relTo relBase;
      dupe      = rel (self.writeScript name (readFile file));
      rendered  = self.runCommand name
        (vars // {
          buildInputs = [ self.commands.relativise self.commands.render_page ] ++
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
      relToTest = self.runCommand "relative"
        {
          buildInputs = [ self.xidel ];
          page        = relToUntested "." (self.writeScript "test.html" ''
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

      relToUntested = TO_ROOT: file: self.runCommand
        "relative-${baseNameOf (unsafeDiscardStringContext file)}"
        {
          inherit file TO_ROOT;
          buildInputs = [ self.commands.relativise ];
        }
        ''
          echo "Relativising $file to $TO_ROOT" 1>&2
          relativise < "$file" > "$out"
        '';
    };
    x: y: self.withDeps [ relToTest ] (relToUntested x y);

  # Turn ".md" names in to ".html"
  mdToHtml = name: (removeSuffix ".html" (removeSuffix ".md" name)) + ".html";
  mdToHtmlRec = x: if isAttrs x
                      then mapAttrs' (n: v: { name  = if hasSuffix ".md" n
                                                         then self.mdToHtml n
                                                         else n;
                                              value = self.mdToHtmlRec v; })
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
                                     name  = self.mdToHtml p;
                                     value = ./blog + "/${p}";
                                   })
                                   postNames);
    };
    mapAttrs (n: v: self.render {
               file        = v;
               name        = "blog-${n}";
               SOURCE_PATH = "blog/${self.htmlToMd (baseNameOf n)}";
               relBase     = "./..";
             })
             posts;

  renderAll = prefix: x: self.mdToHtmlRec (mapAttrs
    (n: v: if isDerivation v || self.isPath v
              then self.render {
                     file        = v;
                     name        = self.mdToHtml n;
                     SOURCE_PATH = concatStringsSep "/" (prefix ++ [n]);
                     relBase     = concatStringsSep "/" (["."] ++ map (_: "..")
                                                                      prefix);
                   }
              else if isAttrs v
                      then self.renderAll (prefix ++ [n]) v
                      else abort "Can't render ${toJSON { inherit n v; }}")
    x);

  projects     = self.renderAll ["projects"] (self.dirsToAttrs ./projects) //
                 { repos = self.projectRepos; };

  unfinished   = self.renderAll ["unfinished"] (self.dirsToAttrs ./unfinished);

  # Derivations which build entire sub-directories
  blogPages       = self.attrsToDirs' "blog"       self.blog;
  projectPages    = self.attrsToDirs' "projects"   self.projects;
  unfinishedPages = self.attrsToDirs' "unfinished" self.unfinished;

  topLevel     = mapAttrs' (name: val: {
                             inherit name;
                             value = self.render (val // {
                               inherit name;
                               relBase = ".";
                             });
                           }) {
    "index.html"      = {
      vars        = { inherit blogPages; };
      file        = ./index.md;
      SOURCE_PATH = "index.md";
    };
    "blog.html"       = {
      vars        = { inherit blogPages; };
      file        = ./blog.md;
      SOURCE_PATH = "blog.md";
    };
    "contact.html"    = {
      file        = ./contact.md;
      SOURCE_PATH = "contact.md";
    };
    "projects.html"   = {
      vars        = { projects = projectPages; };
      file        = ./projects.md;
      SOURCE_PATH = "projects.md";
    };
    "unfinished.html" = {
      vars        = { inherit unfinishedPages; };
      file        = ./unfinished.md;
      SOURCE_PATH = "unfinished.md";
    };
  };

  resources = with rec {
    atom = self.runCommand "blog.atom"
      {
        blog        = self.topLevel."blog.html";
        buildInputs = [ self.hfeed2atom ];
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

    rss  = self.runCommand "blog.rss"
      {
        inherit atom;
        buildInputs = [ self.commands.mkRss ];
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
        if self.isPath content || isDerivation content
           then self.relTo (concatStringsSep "/" (["."] ++ map (_: "..") paths))
                      (mkRedirectTo {
                        from = self.sanitiseName entry;
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

      projectDirs = filter (name: let val = getAttr name self.projects;
                                   in isAttrs val &&
                                      (!(isDerivation val)) &&
                                      elem name toplevelRedirects)
                           (attrNames self.projects);

      redirectDir = entry: {
        "index.html" = self.relTo "." (mkRedirectTo {
          from = "redirect-${self.sanitiseName entry}";
          to   = "/projects/${entry}/index.html";
        });
      };

      oldLinks = listToAttrs (map (name: { inherit name;
                                           value = redirectDir name; })
                                  projectDirs);

      mkRedirectTo = { from, to }: self.runCommand from
        {
          inherit to;
          buildInputs = [ self.commands.mkRedirectTo ];
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

      essays = mapAttrs (go ["projects"]) self.projects;

      "essays.html" = self.relTo "." (mkRedirectTo {
        from    = "essays.html";
        to      = "projects.html";
      });
    };

  allPages = self.topLevel // self.redirects // self.resources // {
    blog        = blogPages;
    projects    = projectPages;
    unfinished  = unfinishedPages;
    "index.php" = self.render {
      vars = { inherit blogPages; };
      file = ./redirect.md;
      name = "index.php";
    };
  };

  tests        = self.callPackage ./tests.nix { inherit self; };

  wholeSite    = self.withDeps (self.allDrvsIn self.tests) self.untestedSite;

  untestedSite = self.attrsToDirs' "untestedSite" self.allPages;

  repoUrls = if self.isPath self.repoSource    ||
                (isString self.repoSource &&
                 self.repoSource != ""    &&
                 substring 0 1 self.repoSource == "/")
                then map (n: "${self.repoSource}/${n}")
                         (attrNames (filterAttrs (n: v: v == "directory" &&
                                                        hasSuffix ".git" n)
                                                 (readDir self.repoSource)))
                else import (self.runCommand "repoUrls.nix"
                              {
                                inherit (self) repoSource;
                                buildInputs = [ self.jq self.wget self.xidel ];
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

  inherit (self.callPackage ./repos.nix {})
    projectRepos repoName repoPages;

  /*inherit (self.callPackage ./ipfs.nix {
            inherit self.allPages attrsToDirs self.bins self.commands self.repoName self.repoUrls;
          })
    ipfsHash ipfsKeys;*/
}
