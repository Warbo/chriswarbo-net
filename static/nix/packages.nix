{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "b8f0069";
  sha256 = "1bbs7zy68sdr1d6di5rlzjkh3zj6da97sa0yi03plwmp289sqp6d";
}
