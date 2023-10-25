# Give each git repo a page which redirects to the repo's own site
{ attrsToDirs', lib, redirect, render, repoSource }:

with builtins;
with lib;
with rec {
  inherit (repoSource) base repos;

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
