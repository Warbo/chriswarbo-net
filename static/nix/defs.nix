args:

with builtins;
with rec {
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

  # For speed, we allow the latest commit IDs to be passed in too
  repoRefs = if getEnv "REPO_REFS" == ""
                then {}
                else fromJSON (getEnv "REPO_REFS");

  # We rely on a bunch of helper functions, etc. from the nix-config repo
  url = "${repoSource}/nix-config.git";

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  # If stable = false then we use "unstable", which is <nixpkgs>
  pinnedNixpkgs = if args.stable or true
                     then "nixpkgs1709"
                     else "unstable";

  # Use fetchgit to a particular version of nix-config
  fixed =
    with rec {
      pkgs = import <nixpkgs> { config = {}; };
      src  = pkgs.fetchgit {
        inherit url;
        rev    = "99bc878";
        sha256 = "0q8f30vzvngnnvszxxp6vhr649y4lvix4r9axhvmpc9wr5afls6s";
      };
    };
    import src { defaultVersion = pinnedNixpkgs; };

  # nix-config lets us fetch its latest version without having to specify a hash
  latest = if repoRefs ? nix-config
              # Avoids having to lookup the latest revision
              then import "${fixed.fetchGitHashless {
                               inherit url;
                               rev = repoRefs.nix-config;
                             }}/unstable.nix"
              # Looks up the latest version, fetches it and imports it
              else fixed.latestNixCfg;
};

rec {
  inherit repoRefs repoSource;
  configuredPkgs = if args.stable or true
                      then fixed
                      else latest;
}
