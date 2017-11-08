with import ./static/nix {};
with configuredPkgs;
runCommand "dummy" {} "exit 1"
