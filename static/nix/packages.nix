{ repoSource ? "http://chriswarbo.net/git" }:

builtins.fetchGit {
  url = "${repoSource}/warbo-packages.git";
  rev = "01aea9eb68725aace0d8dda0d27f11413ff545bb";
}
