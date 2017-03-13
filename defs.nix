with rec {
  orig  = import <nixpkgs> {};

  fixed = orig.fetchFromGitHub {
    owner  = "Warbo";
    repo   = "nix-config";
    rev    = "4315680";
    sha256 = "0n3jq176595hpkbp3x472wj3yhdmvsphqxwk2rvvb96vi02pid06";
  };

  withFixed = import <nixpkgs> { config = import "${fixed}/config.nix"; };

  latest = withFixed.latestGit {
    url = "http://chriswarbo.net/git/nix-config.git";
  };
};
rec {
  latestConfig   = "${latest}/config.nix";
  configuredPkgs = import <nixpkgs> { config = import latestConfig; };
}
