{ attrsToDirs, bash, buildEnv, commands, coq, dirContaining, haskellPackages,
  hfeed2atom, git, glibcLocales, lib, libxslt, mergeDirs, mkBin, pandocPkgs,
  python, replace, runCommand, wget, withNix, wrap, xidel, xmlstarlet }:

with builtins;
with lib;
with rec {
  nixVars = withNix {
    CALLING_NIX_RECURSIVELY = "1";
    REPO_REFS               = if getEnv "REPO_REFS" == ""
                                 then "{}"
                                 else getEnv "REPO_REFS";
  };

  bins = bin: attrsToDirs { inherit bin; };

  includingDeps = xs:
    filter (x: x != null)
           (xs ++ concatMap (x: x.propagatedNativeBuildInputs) xs
               ++ concatMap (x: x.propagatedBuildInputs)       xs);

  bs = python.withPackages (p: [ p.python p.beautifulsoup4 ]);

  extras = {
    # Avoids depending on GTK, Gnome, etc.
    coqNoIde = coq.override { buildIde = false; };

    nix-instantiate = mkBin {
      name   = "nix-instantiate";
      paths  = [ bash ] ++ (withNix {}).buildInputs;
      vars   = nixVars;
      script = ''
        #!/usr/bin/env bash
        exec nix-instantiate "$@"
      '';
    };

    nix-shell = mkBin {
      name   = "nix-shell";
      paths  = [ bash ] ++ (withNix {}).buildInputs;
      vars   = nixVars;
      script = ''
        #!/usr/bin/env bash
        exec nix-shell "$@"
      '';
    };

  };

  wrapScript = name: vals: mkBin (vals // {
    inherit name;
    file  = ./.. + "/${name}";
    paths = includingDeps (vals.paths or []);
  });
};

extras // mapAttrs wrapScript {
  cleanup = {
    paths = [ commands.stripEmptyPreCode commands.summariseTables ];
  };

  file2img = {};

  git2md = {
    paths = [ git wget ];
  };

  mkRedirectTo = {
    vars = { TEMPLATE = ../redirectTemplate.html; };
  };

  mkRss = {
    paths = [ libxslt ];
    vars  = { XSL = ../atom2rss.xsl; };
  };

  relativise = {
    paths = [ xmlstarlet ];
  };

  relTo = {
    paths = [ python ];
  };

  render_page = {
    paths = [ commands.cleanup pandocPkgs ];
    vars  = {
      defaultTemplate = ../../templates/default.html;
      LANG            = "en_US.UTF-8";
      LOCALE_ARCHIVE  = "${glibcLocales}/lib/locale/locale-archive";
    };
  };

  renderHcard = {};

  showPost = {
    paths = [ replace xidel ];
    vars  = { RANTS = ../rants; };
  };

  showPosts = {
    paths = [ commands.showPost ];
  };

  stripEmptyPreCode = {
    paths = [ bs ];
  };

  stripTitle = {
    paths = [ bs ];
  };

  summariseTables = {
    paths = [ bs ];
  };

  wrapCode = {};
}
