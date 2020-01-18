{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "9725758";
  sha256 = "041sz2xrk85cnx7gi2jsy9lpd5sp72bfr8bq7dc5akr1k7lf4zyk";
}
