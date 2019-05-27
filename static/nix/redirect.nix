{ commands, isPath, lib, projects, relTo, runCommand, sanitiseName }:

with builtins;
with lib;
with rec {
  go = paths: entry: content:
    if isPath content || isDerivation content
       then relTo
              (concatStringsSep "/" (["."] ++ map (_: "..") paths))
              (mkRedirectTo {
                from = sanitiseName entry;
                to   = concatStringsSep "/" ([""] ++ paths ++ [entry]);
              })
       else mapAttrs (go (paths ++ [entry])) content;

  mkRedirectTo = { from, to }: runCommand from
    {
      inherit to;
      buildInputs = [ commands.mkRedirectTo ];
    }
    ''
      # We make sure the redirection succeeds before we do anything to $out,
      # to avoid creating empty or partial files
      RESULT=$(mkRedirectTo "$to")
      echo "$RESULT" > "$out"
    '';

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
    "index.html" = relTo "." (mkRedirectTo {
      from = "redirect-${sanitiseName entry}";
      to   = "/projects/${entry}/index.html";
    });
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

    "essays.html" = relTo "." (mkRedirectTo {
      from = "essays.html";
      to   = "projects.html";
    });
  };
}
