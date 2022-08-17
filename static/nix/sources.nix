rec {
  nix-helpers    = import ./sources/nix-helpers.nix;
  nixpkgs        = (import nix-helpers).repoLatest;
  warbo-packages = import ./sources/warbo-packages.nix;
}
