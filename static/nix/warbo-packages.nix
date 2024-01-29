{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "1f8eaa3001e1ab98a1daf751c4f1edcab642c9a0";
}) { }
