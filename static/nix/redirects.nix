{ commands, isPath, lib, projects, relTo, runCommand, sanitiseName }:

with builtins;
with lib;
rec {
  mkRedirectTo = { from, to, rel ? null }:
    (if rel == null then (x: x) else relTo rel) (runCommand from
      {
        inherit to;
        buildInputs = [ commands.mkRedirectTo ];
      }
      ''
        # We make sure the redirection succeeds before we do anything to $out,
        # to avoid creating empty or partial files
        RESULT=$(mkRedirectTo "$to")
        echo "$RESULT" > "$out"
      '');

  redirects = oldLinks // {
    inherit essays;
    data_custom = {
      "prelude.txt" = mkRedirectTo {
        from = "prelude.txt";
        to   = "/git/php-prelude";
      };
    };
  };
}
