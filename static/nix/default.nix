{ warbo-packages ? import ../../../warbo-packages { }
, nix-helpers ? warbo-packages.nix-helpers, nixpkgs ? nix-helpers.nixpkgs }:
with builtins;
with rec {
  # We host a bunch of git repos; look up their location from the environment
  repoSource = with {
    env = getEnv "GIT_REPO_DIR"; # User-provided override
    local = "/home/chris/Programming/repos"; # My local copy; fast
    remote = "http://chriswarbo.net/git"; # "Official" remote; default
  };
    if env != "" then
      env
    else if pathExists local then
      local
    else
      trace ''
        INFO: Getting repos from ${remote}, which may be slow
              as it's remote. You can override this by setting
              GIT_REPO_DIR.
      '' remote;

  extras = nix-helpers // warbo-packages;
};
nixpkgs.newScope (extras // { inherit extras; }) ./overlay.nix { }
