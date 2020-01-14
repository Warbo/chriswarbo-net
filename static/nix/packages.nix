{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "8205708";
  sha256 = "04ch47rxva07wmdy2mhg4afjrn2sxp29as5h5q1d3sqz782f6ach";
}
