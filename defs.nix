with builtins;
with rec {
  # System's nixpkgs; might not have everything we need
  orig  = import <nixpkgs> {};

  # We host a bunch of git repos; look up their location from the environment
  repoEnv    = getEnv "GIT_REPO_DIR";
  repoSource = if repoEnv == ""
                  then "/home/chris/Programming/repos"
                  else repoEnv;

  # For speed, we allow the latest commit IDs to be passed in too
  repoRefs = if getEnv "REPO_REFS" == ""
                then {}
                else fromJSON (getEnv "REPO_REFS");

  # Get our custom nix config; nixpkgs forces us to check the hash
  fixed = orig.fetchgit {
    url    = "${repoSource}/nix-config.git";
    rev    = "ffa6543";
    sha256 = "08hi2j38sy89sk5aildja453yyichm2jna8rxk4ad44h0m2wy47n";
  };
  withFixed = import <nixpkgs> { config = import "${fixed}/config.nix"; };

  # Our nix config allows fetching git commits dynamically; get latest config
  latest = if repoRefs ? nix-config
              then withFixed.fetchGitHashless {
                     url = "${repoSource}/nix-config.git";
                     rev = repoRefs.nix-config;
                   }
              else withFixed.latestGit {
                     url = "${repoSource}/nix-config.git";
                   };
};

# Ensure we've got some repos, and return our latest config
assert pathExists repoSource ||
       abort "No git repos found at '${repoSource}', maybe set GIT_REPO_DIR?";
rec {
  inherit repoRefs repoSource;
  latestConfig   = "${latest}/config.nix";
  configuredPkgs = import <nixpkgs> { config = import latestConfig; };
}
