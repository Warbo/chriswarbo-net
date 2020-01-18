# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs', isPath, jq, lib, mkRedirectTo, render, repoSource, runCommand,
  wget, xidel }:

with builtins;
with lib;
with rec {
  isAbsPath = p: isString p && p != "" && substring 0 1 p == "/";

  # Fetch a list of repos from the online /git page. If we've opted to use a set
  # of local clones, just list their directory (much faster!)
  repoDirs = if isPath repoSource || isAbsPath repoSource
                then attrNames (filterAttrs (n: v: v == "directory" &&
                                                   hasSuffix ".git" n)
                                            (readDir repoSource))
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

  repoNames = map (d: removeSuffix ".git" (baseNameOf d)) repoDirs;

  contents = listToAttrs (map (repo: rec {
                                name  = repo + ".html";
                                value = mkRedirectTo {
                                  from = name;
                                  to   = "../../git/" + repo;
                                };
                              })
                              repoNames);
};
attrsToDirs' "repos" (contents // {
  "index.html" = render {
    file        = ../../repos.md;
    name        = "index.html";
    vars        = { repos = attrsToDirs' "repoPages" contents; };
    SOURCE_PATH = "repos.md";
    TO_ROOT     = "../..";
  };
})
