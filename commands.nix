{ attrsToDirs, buildEnv, dirContaining, haskellPackages, hfeed2atom, git,
  glibcLocales, lib, libxslt, makeWrapper, mergeDirs, nix, pandoc, panhandle,
  panpipe, pythonPackages, runCommand, wget, wrap, xidel, xmlstarlet }:

with builtins;
with lib;
with rec {
  NIX_PATH =
    with rec {
      inherit (tryEval <real>) success;

      # We need this mirrors.nix file in place if we're to spoof <nixpkgs>
      wrapped = runCommand "chriswarbo.net-nixpkgs"
        {
          custom = ./static/nix;
          real   = toString <nixpkgs>;
        }
        ''
          # Put our custom nixpkgs in place
          cp -as "$custom" "$out"
          chmod +w -R "$out"

          pushd "$out"
            # Put nixpkgs symlink in place, to resolve names
            ln -s . nixpkgs

            # Link to real nixpkgs for mirrors.nix
            ln -s "$real/pkgs" pkgs
          popd
        '';

      set = if success
               then { real = <real>;    nixpkgs = <nixpkgs>;  }
               else { real = <nixpkgs>; nixpkgs = wrapped; };

    };
    # toString turns paths into strings as-is, whilst ${..} would add them to
    # the store which causes issues with relative symlinks and the like.
    concatStringsSep ":" (attrValues (mapAttrs (n: v: n + "=" + toString v)
                                               set));

  NIX_REMOTE = "daemon";

  bins = bin: attrsToDirs { inherit bin; };

  includingDeps = xs:
    filter (x: x != null)
           (xs ++ concatMap (x: x.propagatedNativeBuildInputs) xs
               ++ concatMap (x: x.propagatedBuildInputs)       xs);

  bs = pythonPackages.python.withPackages (p: [ p.python p.beautifulsoup4 ]);

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
      vars  = { inherit NIX_PATH NIX_REMOTE; };
      file  = "${nix}/bin/nix-instantiate";
    };

    nix-shell = {
      paths = [ nix ];
      vars  = { inherit NIX_PATH NIX_REMOTE; };
      file  = "${nix}/bin/nix-shell";
    };

    relativise = {
      paths = [ xmlstarlet ];
      file  = ./static/relativise;
    };

    relTo = {
      paths = [ pythonPackages.python ];
      file  = ./static/relTo;
    };

    render_page = {
      paths = [ cleanup pandoc panhandle panpipe ];
      vars  = {
        defaultTemplate = ./templates/default.html;
        LANG            = "en_US.UTF-8";
        LOCALE_ARCHIVE  = "${glibcLocales}/lib/locale/locale-archive";
      };
      file  = ./static/render_page;
    };

    renderHcard = { file = ./static/renderHcard; };

    showPost = {
      paths = [ xidel ];
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
