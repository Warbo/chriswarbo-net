{ commands, isPath, lib, projects, relTo, runCommand, sanitiseName }:

with builtins;
with lib;
with rec {
  go = paths: entry: content:
    if isPath content || isDerivation content
       then mkRedirectTo {
              from = sanitiseName entry;
              to   = concatStringsSep "/" ([""] ++ paths ++ [entry]);
              rel  = concatStringsSep "/" (["."] ++ map (_: "..") paths);
            }
       else mapAttrs (go (paths ++ [entry])) content;

  mkRedirectTo = { from, to, rel ? null }:
    (if rel == null then (x: x) else relTo rel) (runCommand from
      {
        inherit to;
        buildInputs = [ commands.mkRedirectTo ];
      }
      ''
        # We make sure the redirection succeeds before we do anything to $out,
        # to avoid creating empty or partial files
        RESULT=$(mkRedirectTo "$to")
        echo "$RESULT" > "$out"
      '');

  essays = mapAttrs (go ["projects"]) projects;

  # These pages now live in projects/ but there are links in the wild
  # without that prefix. We single them out here to avoid proliferating new
  # redirects for new pages which don't have this compatibility issue.
  toplevelRedirects = [
    "activecode" "arduino" "maze" "nixos" "optimisation" "plumb" "powerplay"
    "procedural" "turtleview"
  ];

  projectDirs = filterAttrs (name: val: isAttrs val           &&
                                        (!(isDerivation val)) &&
                                        elem name toplevelRedirects)
                            projects;

  redirectDir = entry: {
    "index.html" = mkRedirectTo {
      from = "redirect-${sanitiseName entry}";
      to   = "/projects/${entry}/index.html";
      rel  = ".";
    };
  };

  oldLinks = mapAttrs (name: _: redirectDir name) projectDirs;
};
{
  redirects = oldLinks // {
    inherit essays;
    data_custom = {
      "prelude.txt" = mkRedirectTo {
        from = "prelude.txt";
        to   = "/git/php-prelude";
      };
    };

    "essays.html" = mkRedirectTo {
      from = "essays.html";
      to   = "/projects";
      rel  = ".";
    };

    "projects.html" = mkRedirectTo {
      from = "projects.html";
      to   = "/projects/";
      rel  = ".";
    };
  };
}
