# Look up available git repos and generate a page for each, including the
# contents of any READMEs we find.
{ attrsToDirs, commands, latestConfig, lib, nix, render, repoUrls, reverse,
  runCommand, wget, withNix, writeScript }:

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

  # To force a page's generation, set UPDATE_REPOS to a JSON array
  # containing the repo's name, e.g. UPDATE_REPOS='["warbo-utilities"]'
  checks =
    with rec {
      # The names of any repos whose cache should be invalidated
      regenNames = if getEnv "UPDATE_REPOS" == ""
                      then []
                      else fromJSON (getEnv "UPDATE_REPOS");

      # The repo URLs to invalidate
      regenUrls = filter given repoUrls;

      given = url: elem (repoName url) regenNames;

      # A Nix expression to force regeneration of invalidated repos
      data   = writeScript "repo-regen"
                 (toJSON (genAttrs repoUrls (url: if given url
                                                     then toString currentTime
                                                     else "cached")));

      result = writeScript "result.nix" ''
        with builtins;
        fromJSON (readFile "${data}")
      '';
    };
    runCommand "repo-regen-check"
      (withNix {
        # Avoid the cache by including a timestamp
        inherit currentTime result;
        regen = writeScript "to-regen" (concatStringsSep "\n" regenUrls);
      })
      ''
        while read -r url
        do
          echo "Deleting cached build of '$url'" 1>&2

          # Get the derivation for this page
          DRV=$(nix-instantiate --argstr url "$url" ${repoPageNix})

          nix-store --delete "$DRV"
        done < "$regen"

        cp "$result" "$out"
      '';

  checked = import "${checks}";

  repoName = url: removeSuffix ".git" (baseNameOf url);

  repoPageOf = url:
    with rec {
      name  = repoName url;

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

      mkDrv = ''DRV=$(nix-instantiate --argstr url "$url" ${repoPageNix})'';
    };

    runCommand "repo-page-${name}.html"
      (withNix {
        inherit url;
        cache = "cached-${time}";

        # Will be a timestamp (and hence uncached) if this page needs
        # regenerating or "cached" if not
        clear = checked."${url}";
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

  repoPages = listToAttrs (map (url: { name  = repoName url + ".html";
                                       value = repoPageOf url; }) repoUrls);
};

repoPages // {
  "index.html" = render {
    file = ./repos.md;
    name = "index.html";
    cwd  = attrsToDirs { repos = repoPages; };
  };
}
