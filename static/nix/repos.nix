# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs', lib, redirect, render }:

with builtins;
with lib;
with rec {
  base = with {
    env = getEnv "GIT_REPO_DIR"; # User-provided override
    remote = "http://chriswarbo.net/git"; # "Official" remote; default
  };
    if env != "" then
      env
    else
      trace ''
        INFO: Getting repos from ${remote}. You can override this by setting the
              GIT_REPO_DIR environment variable.
      '' remote;

  # We used to read these from a local folder, or from the directory listing of
  # chriswarbo.net/git. However, that introduced too much coupling, which we
  # prefer to avoid.
  # TODO: Add tests that compare this list to chriswarbo.net/git,
  # github.com/Warbo, and any other places we're mirroring.
  repos = importJSON ./repos.json;

  contents = listToAttrs (map (repo: rec {
    name = repo + ".html";
    value = redirect {
      from = name;
      to = "../../git/" + repo;
    };
  }) repos);
};
attrsToDirs' "repos" (contents // {
  "index.html" = render {
    file = ../../repos.md;
    name = "index.html";
    vars = { repos = attrsToDirs' "repoPages" contents; };
    SOURCE_PATH = "repos.md";
    TO_ROOT = "../..";
  };
})
