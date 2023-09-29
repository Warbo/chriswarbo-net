{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "6316535f0c9abddf2fa77c4f2861acc8b35375cb";
}) { }
