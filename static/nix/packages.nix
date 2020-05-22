{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "c472887";
  sha256 = "0gjgimwhgg8q42bvswjnja1f1cg9akmi6j60s2czqlgqv15isjdj";
}
