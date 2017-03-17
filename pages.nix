{ attrsToDirs, callPackage, dirsToAttrs, git, hfeed2atom, ipfs, jq,
  latestConfig, latestGit, lib, makeWrapper, nix, pandoc, panhandle, panpipe,
  pkgs, pythonPackages, runCommand, stdenv, tidy-html5, wget, withNix,
  writeScript }:

with builtins;
with lib;
with rec { pages = rec {
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

  repos = strip (callPackage ./repos.nix {
    inherit commands latestConfig render repoUrls;
  });

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

  gitRepos =
    with rec {
      unpack = repo: runCommand "unpack"
        {
          inherit repo;
          buildInputs = [ git ];
        }
        ''
          set -e
          shopt -s nullglob

          cp -r "$repo" "$out"
          chmod +w -R "$out"
          cd "$out"

          git repack -A -d
          git update-server-info

          # Unpack git's internal files, so they dedupe better on IPFS. We need
          # to copy them out of .git first, otherwise git ignores them as it
          # already knows about them
          MATCHES=$(find objects/pack -maxdepth 1 -name '*.pack' -print -quit)
          if [[ -n "$MATCHES" ]]
          then
            cp objects/pack/*.pack .
            git unpack-objects < ./*.pack
            rm ./*.pack
          fi
        '';

      addRepo = path: { name = baseNameOf path; value = unpack path; };
    };
    listToAttrs (map addRepo repoUrls);

  gitPages = trace "TODO: Repo pages" {};

  repoUrls =
    let repoSource = getEnv "GIT_REPO_DIR";
     in assert repoSource != "";
        map (n: "${repoSource}/${n}")
            (attrNames (filterAttrs (n: v: v == "directory" &&
                                           hasSuffix ".git" n)
                                    (readDir repoSource)));


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
    git         = gitPages // gitRepos;
    js          = ./js;
  };

  redirects = dirsToAttrs (runCommand "with-redirects" {
    buildInputs = [ commands.mkEssayLinks ];
    projects    = attrsToDirs projects;
  } "mkEssayLinks");

  allPages = mkRel (redirects // topLevel // resources // {
    inherit blog projects unfinished;
    "index.php" = render {
      cwd  = attrsToDirs { rendered = { inherit blog; }; };
      file = ./redirect.md;
      name = "index.php";
    };
  });

  strip = filterAttrs (n: v: !(elem n [ "override" "overrideDerivation" ]));

  tests = strip (callPackage ./tests.nix { inherit pages; });

  untested = attrsToDirs allPages;

  site = runCommand "site"
           {
             inherit untested;
             tests = attrsToDirs tests;
           }
           ''cp -r "$untested" "$out"'';

  ipfsHash = runCommand "site-hash"
    {
      inherit site;
      buildInputs = [ ipfs ];
      IPFS_PATH   = "/var/lib/ipfs/.ipfs";
    }
    ''
      set -e
      set -o pipefail

      NAME=$(basename "$site")

      echo "Adding $site to IPFS" 1>&2
      LINE=$(ipfs add -r "$site" | tail -n1)

      echo "Checking the whole site was added" 1>&2
      echo "$LINE" | grep "^added [^ ]* $NAME\$" > /dev/null || {
        echo "Last line wasn't for site name '$NAME' (did insertion fail?):" >&2
        echo "$LINE" 1>&2
        exit 1
      }

      echo "Getting site hash" 1>&2
      IPFS_HASH=$(echo "$LINE" | sed -e 's/^added //g; s/ [^ ]*$//g')
      echo "$IPFS_HASH" > "$out"
    '';
}; }; pages
