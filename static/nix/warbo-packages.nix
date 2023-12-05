{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "acd6402cd0c764ae4233f11ecb56d2a3176cb01f";
}) { }
