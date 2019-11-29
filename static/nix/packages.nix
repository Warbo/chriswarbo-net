{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "6626cc0";
  sha256 = "1pdrxcq5r1pqyijd4216fgs3663wsi07js5cfq4bnh53qv687rkf";
}
