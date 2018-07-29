# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs, commands, fetchGitHashless, ipfsKeys ? {}, lib, render, repoUrls,
  runCommand, writeScript }:

with builtins;
with lib;
with rec {
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

  repoPages = listToAttrs (map (url: { name  = repoName url + ".html";
                                       value = repoPageOf (repoName url); })
                               repoUrls);
};
{
  inherit repoName repoPages;

  projectRepos = repoPages // {
    "index.html" = render {
      file = ./repos.md;
      name = "index.html";
      vars = {
        repos = attrsToDirs repoPages;
        /*keys  = attrsToDirs (mapAttrs (n: v: writeScript "ipfs-key-${n}" v)
                                      ipfsKeys);*/
      };
      SOURCE_PATH = "repos.md";
    };
  };
}
