{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "0111eead4402ab48dcd32bc5de5dbad5734abc21";
}) { }
