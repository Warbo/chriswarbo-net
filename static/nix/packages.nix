{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "499d01c";
  sha256 = "13w276nz8lkvj5hh1xdmnhhym8k1qg2q17yksll8db41vjvicvz6";
}
