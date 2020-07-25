{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "e325b63";
  sha256 = "1caa0747ss83sjpmbvsamsf9nibvjqhl9b94h7k1jxl7zkhj5x6z";
}
