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
    file  = vals.name or (./.. + "/${name}");
    paths = includingDeps (vals.paths or []);
  });

  pythonScripts = genAttrs
    [ "cleanup" "htmlUnwrap" "relativise" "stripTitle" ]
    (name: wrapScript name {
      paths = [ (python.withPackages (p: [ p.python p.beautifulsoup4 ])) ];
    });

  wrapped = extras // pythonScripts // mapAttrs wrapScript {
    file2img = {};

    git2md = { paths = [ git wget ]; };

    mkRedirectTo = {
      vars = { TEMPLATE = ../redirectTemplate.html; };
    };

    relTo = { paths = [ python ]; };

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

    showPosts = { paths = [ wrapped.showPost ]; };
    wrapCode = {};
  };
};
wrapped
