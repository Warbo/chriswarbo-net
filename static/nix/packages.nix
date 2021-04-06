{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "0399585";
  sha256 = "1ijpnk3fx8cd527idv1hjf3na873hyqwsr6jbr2fr94zqr3v0q14";
}
