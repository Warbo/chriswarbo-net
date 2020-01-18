{ attrsToDirs', isPath, lib, mkRedirectTo, render, renderAll, repos,
  sanitiseName }:

with lib;
with rec {
  # The content that will be linked to from index.html
  contents = renderAll "projects" // { inherit repos; };

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

  # This section used to be called "essays", so we maintain redirects from those
  # URLs to these.
  essays = {
    "essays.html" = mkRedirectTo {
      from = "essays.html";
      to   = "/projects";
      rel  = ".";
    };

    essays = mapAttrs (essayLink ["projects"]) projects;
  };

  essayLink = paths: entry: content:
    if isPath content || isDerivation content
       then mkRedirectTo {
              from = sanitiseName entry;
              to   = concatStringsSep "/" ([""] ++ paths ++ [entry]);
              rel  = concatStringsSep "/" (["."] ++ map (_: "..") paths);
            }
       else mapAttrs (essayLink (paths ++ [entry])) content;

  # Some projects used to exist at the top-level, and there are links to them in
  # the wild. We single them out here for special treatment, rather than looping
  # through all projects, since we don't want to clutter the top-level with new
  # projects that have never existed there before.
  oldLinks = mapAttrs (name: _: redirectDir name) projectDirs;

  projectDirs = filterAttrs
    (name: val: isAttrs val           &&
                (!(isDerivation val)) &&
                elem name [ "activecode" "arduino" "maze" "nixos" "optimisation"
                            "plumb" "powerplay" "procedural" "turtleview" ])
    projects;

  redirectDir = entry: {
    "index.html" = mkRedirectTo {
      from = "redirect-${sanitiseName entry}";
      to   = "/projects/${entry}/index.html";
      rel  = ".";
    };
  };
};
oldLinks // essays // {
  inherit projects;

  "projects.html" = mkRedirectTo {
    from = "projects.html";
    to   = "/projects/";
    rel  = ".";
  };
}
