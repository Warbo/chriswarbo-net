{ commands, relTo, runCommand }:

{ from, to, rel ? null }:
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
    '')
