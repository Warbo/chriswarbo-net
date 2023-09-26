{ }:
import (builtins.fetchGit {
  name = "warbo-packages";
  url = "http://chriswarbo.net/git/warbo-packages.git";
  ref = "master";
  rev = "76c0beb850f96a7678b027a6e6eb79af35ab01d8";
}) { }
