{ fail, lib, runCommand, html-tidy, wrap, xidel }:

with rec {
  base = ../..;

  testScript = testName:
    { buildInputs ? [ ] }:
    wrap {
      name = testName;
      file = base + "/tests/${testName}";
      paths = buildInputs;
    };
};
lib.mapAttrs testScript {
  code_not_indented = { buildInputs = [ fail ]; };
  no_absolutes = { buildInputs = [ xidel ]; };
  no_blogspot = { buildInputs = [ xidel ]; };
  no_gitorious = { };
  no_selfclosing_scripts = { buildInputs = [ fail xidel ]; };
  tidy = { buildInputs = [ html-tidy ]; };
} // {
  nonempty = wrap {
    name = "non-empty";
    paths = [ fail ];
    script = ''
      #!/usr/bin/env bash
      set -e
      SIZE=$(stat -L --printf="%s" "$1")
      [[ "$SIZE" -gt 5 ]] || fail "File '$1' has size '$SIZE'"
      exit 0
    '';
  };
}
