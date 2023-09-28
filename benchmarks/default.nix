# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir  ? ./.., # Path to the revision containing the benchmarks
  root ? ./.., # Path to the revision being benchmarked
  ...
}:

with builtins;
with trace (toJSON { inherit dir root; }) {
  fixed    = import "${dir }/static/nix";
  measured = import "${root}/static/nix";
};
with fixed.lib;

fixed.mkBin {
  name  = "python";
  paths = [ fixed.python3 ];
  vars  = {
    eval_wholeSite = fixed.wrap {
      name   = "eval_wholeSite";
      paths = with fixed; [ bash nix ];
      vars   = fixed.withNix { dir = toString "${root}/static/nix"; };
      script = ''
        #!/usr/bin/env bash
        set -e
        nix-instantiate --read-write-mode -E "(import \"$dir\").wholeSite"
      '';
    };
  };
  script = ''
    #!/usr/bin/env bash
    python3 "$@"
  '';
}
