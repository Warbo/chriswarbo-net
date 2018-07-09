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

  # For speed, we allow the latest commit IDs to be passed in too
  repoRefs = if getEnv "REPO_REFS" == ""
                then {}
                else fromJSON (getEnv "REPO_REFS");

  # We rely on a bunch of helper functions, etc. from the nix-config repo
  url = "${repoSource}/nix-config.git";

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  pinnedNixpkgs = helpers.nixpkgs1709;

  fetch   = args: import (nixpkgs.fetchgit (args // {
    url = "${repoSource}/${args.url}";
  }));

  helpers = nixpkgs.nix-helpers or fetch {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "66f9a00";
    sha256 = "0f84hyqslzb56gwc8yrrn8s95nvdfqn0hf6c9i3cng3bsz3yk53v";
  };

  packages = nixpkgs.warbo-packages or fetch {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9fe8653";
    sha256 = "0nzcqs4sxac4kigg3y8aqx8jiwrp71wvvi7a8dviahf766nb6lb4";
  };

  nixpkgs = import <nixpkgs> {};

  fixed =
    with {
      src  = fetch {
        url    = "nix-config.git";
        rev    = "99bc878";
        sha256 = "0q8f30vzvngnnvszxxp6vhr649y4lvix4r9axhvmpc9wr5afls6s";
      };
    };
    import src {};
};

pinnedNixpkgs // helpers // packages
