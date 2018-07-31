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
    ];
  };

  # Pin to a particular version of nixpkgs, to avoid updates breaking things.
  pinnedNixpkgs = overlayed (overlayed <nixpkgs>).repo1709;

  fetch   = args: (import <nixpkgs> {}).fetchgit (args // {
    url = "${repoSource}/${args.url}";
  });

  helpers = fetch {
    url    = "nix-helpers.git";
    rev    = "66f9a00";
    sha256 = "0f84hyqslzb56gwc8yrrn8s95nvdfqn0hf6c9i3cng3bsz3yk53v";
  };

  packages = fetch {
    url    = "warbo-packages.git";
    rev    = "9fe8653";
    sha256 = "0nzcqs4sxac4kigg3y8aqx8jiwrp71wvvi7a8dviahf766nb6lb4";
  };
};

pinnedNixpkgs // {
  inherit repoRefs repoSource;

  # Force working pandocPkgs
  inherit (overlayed pinnedNixpkgs.repo1803) pandocPkgs;
}
