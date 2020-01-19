with builtins;
with rec {
  inherit (import <nixpkgs> { config = {}; overlays = []; }) fetchgit;

  # We host a bunch of git repos; look up their location from the environment
  repoSource =
    with {
      env    = getEnv "GIT_REPO_DIR";            # User-provided override
      local  = "/home/chris/Programming/repos";  # My local copy; fast
      remote = "http://chriswarbo.net/git";      # "Official" remote; default
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

  # Load our custom packages from repoSource
  packages = import ./packages.nix { inherit fetchgit repoSource; };

  # Load our Nix helper functions from the packages repo
  inherit (import "${packages}/helpers.nix" { inherit fetchgit; }) nix-helpers;

  # Add this site's package overlay, along with the above packages and helpers,
  # to a given Nixpkgs repo
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
};

# Apply our overlays to <nixpkgs>, pick our a pinned nixpkgs revision from that
# (defined by our Nix helpers) apply our overlays to that
overlayed (overlayed <nixpkgs>).repo1803
