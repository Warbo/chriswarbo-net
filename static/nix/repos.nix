# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs', commands, fetchGitHashless, isPath, jq, lib, render, repoSource,
  runCommand, wget, writeScript, xidel }:

with builtins;
with lib;
with rec {
  isAbsPath = p: isString p && p != "" && substring 0 1 p == "/";

  repoName = url: removeSuffix ".git" (baseNameOf url);

  repoPageOf = repo: runCommand "redirect-to-${repo}"
    {
      inherit repo;
      buildInputs = [ commands.mkRedirectTo ];
    }
    ''
      RESULT=$(mkRedirectTo "../../git/$repo")
      echo "$RESULT" > "$out"
    '';

  repoUrls = if isPath repoSource || isAbsPath repoSource
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


  repoPages = listToAttrs (map (url: { name  = repoName url + ".html";
                                       value = repoPageOf (repoName url); })
                               repoUrls);
};
{
  inherit repoName repoPages;

  projectRepos = repoPages // {
    "index.html" = render {
      file        = ../../repos.md;
      name        = "index.html";
      vars        = { repos = attrsToDirs' "repoPages" repoPages; };
      SOURCE_PATH = "repos.md";
    };
  };
}
