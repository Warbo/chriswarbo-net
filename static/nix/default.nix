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
      (import "${helpers}/overlay.nix")
      (import "${packages}/overlay.nix")
      (import ./overlay.nix)
      (self: super: {
        inherit repoSource;

        # To force -q instead of -s
        inherit (self.nixpkgs1709) xidel;
      })
    ];
  };

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  pinnedNixpkgs = overlayed (overlayed <nixpkgs>).repo1803;

  fetch   = args: (import <nixpkgs> {
                    config   = {};
                    overlays = [];
                  }).fetchgit (args // {
                    url = "${repoSource}/${args.url}";
                  });

  helpers = fetch {
    url    = "nix-helpers.git";
    rev    = "72d9d88";
    sha256 = "1kggqr07dz2widv895wp8g1x314lqg19p67nzr3b97pg97amhjsi";
  };

  packages = fetch {
    url    = "warbo-packages.git";
    rev    = "6011511";
    sha256 = "0a4rn41k9vaibyvjadhcc8bxfnmjii2047wx1w24m171b95z6rp2";
  };
};

pinnedNixpkgs
