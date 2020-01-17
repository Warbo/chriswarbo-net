{ attrsToDirs', projectRepos, render, renderAll }:

with rec {
  # The content that will be linked to from index.html
  contents = renderAll "projects" // { repos = projectRepos; };

  # All of the contents, including index.html
  projects = contents // {
    "index.html" = render {
      name        = "index.html";
      vars        = { projects = attrsToDirs' "project-contents" contents; };
      file        = ../../projects.md;
      TO_ROOT     = "..";
      SOURCE_PATH = "projects.md";
    };
  };
};
{
  inherit projects;
  projectPages = attrsToDirs' "projects" { inherit projects; };
}
