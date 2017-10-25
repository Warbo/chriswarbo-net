args:

with builtins;
with rec {
  origPath = <nixpkgs>;

  orig = import origPath {};

  # We host a bunch of git repos; look up their location from the environment
  repoEnv    = getEnv "GIT_REPO_DIR";
  repoLocal  = "/home/chris/Programming/repos";
  repoRemote = "http://chriswarbo.net/git";
  repoSource = if repoEnv == ""
                  then if pathExists repoLocal
                          then repoLocal
                          else trace ''INFO: Getting repos from ${repoRemote},
                                       which may be slow as it's remote. You can
                                       override this by setting GIT_REPO_DIR.''
                                     repoRemote
                  else repoEnv;

  url = "${repoSource}/nix-config.git";

  # For speed, we allow the latest commit IDs to be passed in too
  repoRefs = if getEnv "REPO_REFS" == ""
                then {}
                else fromJSON (getEnv "REPO_REFS");

  # Get our custom nix config; nixpkgs forces us to check the hash
  fixed = orig.fetchgit {
    inherit url;
    rev    = "ffa6543";
    sha256 = "08hi2j38sy89sk5aildja453yyichm2jna8rxk4ad44h0m2wy47n";
  };
  withFixed = import origPath { config = import "${fixed}/config.nix"; };

  # Our nix config allows fetching git commits dynamically; get latest config
  latest = if repoRefs ? nix-config
              then withFixed.fetchGitHashless {
                     inherit url;
                     rev = repoRefs.nix-config;
                   }
              else withFixed.latestGit { inherit url; };
};

rec {
  inherit repoRefs repoSource;
  latestConfig   = "${latest}/config.nix";
  configuredPkgs = import origPath (args // { config = import latestConfig; });
}
