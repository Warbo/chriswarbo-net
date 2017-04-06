with builtins;
with rec {
  orig  = import <nixpkgs> {};

  repoEnv    = getEnv "GIT_REPO_DIR";
  repoSource = if repoEnv == ""
                  then "/home/chris/Programming/repos"
                  else repoEnv;

  repoRefs = if getEnv "REPO_REFS" == ""
                then {}
                else fromJSON (getEnv "REPO_REFS" == "");

  fixed = orig.fetchgit {
    url    = "${repoSource}/nix-config.git";
    rev    = "4315680";
    sha256 = "0n3jq176595hpkbp3x472wj3yhdmvsphqxwk2rvvb96vi02pid06";
  };

  withFixed = import <nixpkgs> { config = import "${fixed}/config.nix"; };

  latest = if repoRefs ? nix-config
              then withFixed.fetchGitHashless {
                     url = "${repoSource}/nix-config.git";
                     rev = repoRevs.nix-config;
                   }
              else withFixed.latestGit {
                     url = "${repoSource}/nix-config.git";
                   };
};

assert pathExists repoSource ||
       abort "No git repos found at '${repoSource}', maybe set GIT_REPO_DIR?";
rec {
  inherit repoSource;
  latestConfig   = "${latest}/config.nix";
  configuredPkgs = import <nixpkgs> { config = import latestConfig; };
}
