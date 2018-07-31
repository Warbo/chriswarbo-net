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
    overlays = [
      (import "${helpers}/overlay.nix")
      (import "${packages}/overlay.nix")
      (import ../../overlay.nix)
      (self: super: {
        inherit repoSource;

        # Force working pandocPkgs
        inherit (overlayed pinnedNixpkgs.repo1803) pandocPkgs;
      })
    ];
  };

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  pinnedNixpkgs = overlayed (overlayed <nixpkgs>).repo1709;

  fetch   = args: (import <nixpkgs> {
                    config   = {};
                    overlays = [];
                  }).fetchgit (args // {
                    url = "${repoSource}/${args.url}";
                  });

  helpers = fetch {
    url    = "nix-helpers.git";
    rev    = "9fc3a6b";
    sha256 = "147zffc8bcvd869dh8snz4rk8y53v4zhxzid9pfp89p2fbhvxb6c";
  };

  packages = fetch {
    url    = "warbo-packages.git";
    rev    = "a236f24";
    sha256 = "0152glhq6xarn017kvbwdmyrrsjrsfwy6fh6i1d3qqa9y2gsks44";
  };
};

pinnedNixpkgs
