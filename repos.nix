# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs, commands, fetchGitHashless, latestGit, lib, render, repoRefs,
  repoUrls, runCommand }:

with builtins;
with lib;
with rec {
  gitOrGiven = url:
    with rec {
      name  = repoName url;
      fetchGitArgs = {
        branchName  = "master";  # Avoid default "fetchgit"
        leaveDotGit = true;
        deepClone   = true;
      };
    };
    if repoRefs ? "${name}"
       then fetchGitHashless (fetchGitArgs // {
         inherit url;
         rev = repoRefs."${name}";
       })
       else latestGit {
         inherit fetchGitArgs url;
       };

  repoName = url: removeSuffix ".git" (baseNameOf url);

  repoPageOf = repo: runCommand "redirect-to-${repo}"
    {
      inherit repo;
      buildInputs = [ commands.mkRedirectTo ];
    }
    ''
      mkRedirectTo "/git/$repo" > "$out"
    '';

  repoPages = listToAttrs (map (url: { name  = repoName url + ".html";
                                       value = repoPageOf (repoName url); })
                               repoUrls);
};
{
  inherit repoPages;

  projectRepos = repoPages // {
    "index.html" = render {
      file        = ./repos.md;
      name        = "index.html";
      cwd         = attrsToDirs { repos = repoPages; };
      SOURCE_PATH = "repos.md";
    };
  };
}
