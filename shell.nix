with import ./defs.nix;
with configuredPkgs;
runCommand "dummy" {
  buildInputs = [ pandoc panhandle panpipe ];
} "exit 1"