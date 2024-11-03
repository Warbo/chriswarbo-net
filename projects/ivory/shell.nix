with { inherit (import ../../static/nix { }) commands nixpkgs; };
nixpkgs.mkShell {
  name = "ivory";
  packages = [ commands.racketWithRackCheck ];
}
