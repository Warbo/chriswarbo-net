{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "50c238318788b0856743325d77d933ff34636994";
}) { }
