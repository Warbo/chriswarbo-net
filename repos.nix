# Look up available git repos and generate a page for each, including the
# contents of any READMEs we find.
{ attrsToDirs, attrsToIpfs, commands, fetchgit, fetchGitHashless, git, git2html,
  latestConfig, latestGit, lib, nix, render, repoRefs, repoUrls, reverse,
  runCommand, stdenv, tidy-html5, wget, withNix, writeScript }:

with builtins;
with lib;
with rec {

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

  gitOrGiven = url:
    with rec {
      name  = repoName url;
    };
    if repoRefs ? "${name}"
       then fetchGitHashless {
         inherit url;
         leaveDotGit = true;
         deepClone   = true;
         rev         = repoRefs."${name}";
       }
       else latestGit {
         inherit url;
         fetchgitArgs = { leaveDotGit = true; deepClone = true; };
       };

  repoPageOf = url: runCommand "${repoName url}.html"
    {
      inherit url;
      buildInputs = [ tidy-html5 ];
      repo        = gitOrGiven url;
    }
    ''
      set -o pipefail

      echo "Rendering page for $url" 1>&2
      NAME=$(basename "$url" .git)
      ${commands.git2md}/bin/git2md "$NAME"                   |
        sed -e 's/</\&lt;/g'                                  |
        sed -e 's/>/\&gt;/g'                                  |
        SOURCE= DEST= ${commands.render_page}/bin/render_page |
        "${cleanUp}"                                          > "$out"
    '';

  repoName = url: removeSuffix ".git" (baseNameOf url);

  repoPages = listToAttrs (map (url: { name  = repoName url + ".html";
                                       value = repoPageOf url; }) repoUrls);

  gitPages =
    with {
      repoPage = url: rec {
        name  = repoName url;
        value = runCommand "repo-pages-${name}"
          {
            buildInputs = [ git git2html ];
            repoName    = name;
            repoPath    = gitOrGiven url;
          }
          ''
            set -e
            git2html -p "$repoName" -r "file://$repoPath" \
                     -l "http://chriswarbo.net/git/$repoName.git" "$out"
            rm -rf "$out/repository"
          '';
      };
    };
    listToAttrs (map repoPage repoUrls);

  gitRepos =
    with rec {
      unpack = repo: runCommand "unpack"
        {
          inherit repo;
          cachebust = gitOrGiven repo;
          buildInputs = [ git ];
        }
        ''
          set -e
          shopt -s nullglob

          cp -r "$repo" "$out"
          chmod +w -R "$out"
          cd "$out"

          rm -f hooks/*
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
};
{
  inherit gitRepos gitPages;

  projectRepos = repoPages // {
    "index.html" = render {
      file = ./repos.md;
      name = "index.html";
      cwd  = attrsToDirs { repos = repoPages; };
    };
  };
}
