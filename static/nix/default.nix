with builtins;
with rec {
  # We host a bunch of git repos; look up their location from the environment
  repoSource =
    with {
      env    = getEnv "GIT_REPO_DIR";
      local  = "/home/chris/Programming/repos";
      remote = "http://chriswarbo.net/git";
    };
    if env != ""
       then env
       else if pathExists local
               then local
               else trace ''
                      INFO: Getting repos from ${remote}, which may be slow
                            as it's remote. You can override this by setting
                            GIT_REPO_DIR.
                    '' remote;

  overlayed = repo: import repo {
    config   = {};
    overlays = [
      (import "${nix-helpers}/overlay.nix")
      (import "${packages}/overlay.nix")
      (import ./overlay.nix)
      (self: super: {
        inherit repoSource;

        # To force -q instead of -s
        inherit (self.nixpkgs1709) xidel;
      })
    ];
  };

  packages = import ./packages.nix { inherit fetchgit repoSource; };

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  pinnedNixpkgs = overlayed (overlayed <nixpkgs>).repo1803;

  inherit (import <nixpkgs> { config   = {}; overlays = []; }) fetchgit;

  inherit (import "${packages}/helpers.nix" { inherit fetchgit; }) nix-helpers;
};

pinnedNixpkgs
