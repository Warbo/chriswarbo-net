{ attrsToDirs, bash, coq, fail, git, glibcLocales, haskellPackages, lib,
  libxslt, mkBin, pandocPkgs, python, replace, wget, withNix, xidel }:

with builtins;
with lib;
with rec {
  nixVars = withNix {
    CALLING_NIX_RECURSIVELY = "1";
  };

  includingDeps = xs:
    filter (x: x != null)
           (xs ++ concatMap (x: x.propagatedNativeBuildInputs) xs
               ++ concatMap (x: x.propagatedBuildInputs)       xs);

  bs = python.withPackages (p: [ p.python p.beautifulsoup4 ]);

  extras = {
    # Avoids depending on GTK, Gnome, etc.
    coqNoIde = coq.override { buildIde = false; };

    ghcWithQuickCheck = haskellPackages.ghcWithPackages (h: [
      h.QuickCheck
      h.tasty-quickcheck
    ]);

    nix-instantiate = mkBin {
      name   = "nix-instantiate";
      paths  = [ bash ] ++ (withNix {}).buildInputs;
      vars   = nixVars;
      script = ''
        #!${bash}/bin/bash
        exec nix-instantiate "$@"
      '';
    };

    nix-shell = mkBin {
      name   = "nix-shell";
      paths  = [ bash ] ++ (withNix {}).buildInputs;
      vars   = nixVars;
      script = ''
        #!${bash}/bin/bash
        exec nix-shell "$@"
      '';
    };

  };

  wrapScript = name: vals: mkBin (vals // {
    inherit name;
    file  = ./.. + "/${name}";
    paths = includingDeps (vals.paths or []);
  });

  wrapped = extras // mapAttrs wrapScript {
    cleanup = {
      paths = with wrapped; [
        stripEmptyPreCode
        summariseTables
        unwrapSummaries
      ];
    };

    file2img = {};

    git2md = {
      paths = [ git wget ];
    };

    mkRedirectTo = {
      vars = { TEMPLATE = ../redirectTemplate.html; };
    };

    relativise = {
      paths = [ bs ];
    };

    relTo = {
      paths = [ python ];
    };

    render_page = {
      paths = [ wrapped.cleanup fail pandocPkgs ];
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
      paths = [ wrapped.showPost ];
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

    unwrapSummaries = {
      paths = [ bs ];
    };

    wrapCode = {};
  };
};
wrapped
