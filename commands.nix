{ attrsToDirs, buildEnv, dirContaining, haskellPackages, hfeed2atom, git,
  glibcLocales, lib, libxslt, makeWrapper, mergeDirs, nix, nixpkgs1803, python,
  replace, runCommand, wget, wrap, xidel, xmlstarlet }:

with builtins;
with lib;
with rec {
  nixVars = {
    CALLING_NIX_RECURSIVELY = "1";
    NIX_PATH   = getEnv "NIX_PATH";
    NIX_REMOTE = "daemon";
    REPO_REFS  = if getEnv "REPO_REFS" == ""
                    then "{}"
                    else getEnv "REPO_REFS";
  };

  bins = bin: attrsToDirs { inherit bin; };

  includingDeps = xs:
    filter (x: x != null)
           (xs ++ concatMap (x: x.propagatedNativeBuildInputs) xs
               ++ concatMap (x: x.propagatedBuildInputs)       xs);

  bs = python.withPackages (p: [ p.python p.beautifulsoup4 ]);

  commands = mapAttrs (n: v: bins {
                        "${n}" = wrap (v // {
                          paths = includingDeps (v.paths or []);
                        });
                      })
                      (entries commands);

  entries = self: with self; {
    cleanup = {
      paths = [ stripEmptyPreCode summariseTables ];
      file  = ./static/cleanup;
    };

    file2img = { file = ./static/file2img; };

    git2md = {
      paths = [ git wget ];
      file  = ./static/git2md;
    };

    mkRedirectTo = {
      vars = { TEMPLATE = ./static/redirectTemplate.html; };
      file = ./static/mkRedirectTo;
    };

    mkRss = {
      paths = [ libxslt ];
      vars  = { XSL = ./static/atom2rss.xsl; };
      file  = ./static/mkRss;
    };

    nix-instantiate = {
      paths = [ nix ];
      vars  = nixVars;
      file  = "${nix}/bin/nix-instantiate";
    };

    nix-shell = {
      paths = [ nix ];
      vars  = nixVars;
      file  = "${nix}/bin/nix-shell";
    };

    relativise = {
      paths = [ xmlstarlet ];
      file  = ./static/relativise;
    };

    relTo = {
      paths = [ python ];
      file  = ./static/relTo;
    };

    render_page = {
      paths = [ cleanup nixpkgs1803.pandocPkgs ];
      vars  = {
        defaultTemplate = ./templates/default.html;
        LANG            = "en_US.UTF-8";
        LOCALE_ARCHIVE  = "${glibcLocales}/lib/locale/locale-archive";
      };
      file  = ./static/render_page;
    };

    renderHcard = { file = ./static/renderHcard; };

    showPost = {
      paths = [ replace xidel ];
      vars  = { RANTS = ./rants; };
      file  = ./static/showPost;
    };

    showPosts = {
      paths = [ showPost ];
      file  = ./static/showPosts;
    };

    stripEmptyPreCode = {
      paths = [ bs ];
      file  = ./static/stripEmptyPreCode;
    };

    stripTitle = {
      paths = [ bs ];
      file  = ./static/stripTitle;
    };

    summariseTables = {
      paths = [ bs ];
      file  = ./static/summariseTables;
    };

    wrapCode = { file = ./static/wrapCode; };
  };
};
commands
