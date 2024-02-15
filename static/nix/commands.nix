{ applyPatches, attrsToDirs', bash, buildEnv, cacert, coq, fail, git
, glibcLocales, haskellPackages, jq, lib, libxslt, mkBin, nix
, nix-helpers-source, nixpkgs, nixpkgs1803, pandoc, panhandle, panpipe, python3
, racketWithPackages, replace, suffixedFilesIn, wget, xidel }:

with builtins;
with lib;
with rec {
  gitVars = rec {
    GIT_SSL_CAINFO = SSL_CERT_FILE;
    NIX_SSL_CERT_FILE = SSL_CERT_FILE;
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  };

  nixVars = gitVars // {
    NIX_REMOTE = getEnv "NIX_REMOTE";
    NIX_PATH = getEnv "NIX_PATH";
  };

  includingDeps = xs:
    filter (x: x != null) (xs ++ concatMap (x: x.propagatedNativeBuildInputs) xs
      ++ concatMap (x: x.propagatedBuildInputs) xs);

  extras = {
    # Avoids depending on GTK, Gnome, etc.
    coqNoIde = coq.override { buildIde = false; };

    ghcWithQuickCheck =
      haskellPackages.ghcWithPackages (h: [ h.QuickCheck h.tasty-quickcheck ]);

    ghcWithOmega =
      haskellPackages.ghcWithPackages (h: [ h.control-monad-omega ]);

    git = mkBin {
      name = "git";
      paths = [ git ];
      vars = gitVars;
      script = ''
        #!${bash}/bin/bash
        exec git "$@"
      '';
    };

    mathml = with rec {
      # XSL stylesheets related to MathML
      web-xslt = fetchGit {
        url = "https://github.com/davidcarlisle/web-xslt.git";
        ref = "main";
        rev = "e2d0023bd9b0e5f0538e2ec3df34a5ec5872f549";
      };

      # Scripts to generate MathML
      scripts = mapAttrs (name: file:
        mkBin {
          inherit name file;
          paths = [ jq libxslt ];
          vars = {
            CTOP = "${web-xslt}/ctop/ctop.xsl";
            BARS = ../mathml/bars.xsl;
            OURS = ../mathml/ours.xsl;
          };
        }) (suffixedFilesIn ".sh" ../mathml);
    };
      buildEnv {
        name = "mathml";
        paths = builtins.attrValues scripts;
      };

    matplotlib = python3.withPackages (p: [ p.matplotlib p.numpy ]);

    nix-instantiate = mkBin {
      name = "nix-instantiate";
      paths = [ bash git nix ];
      vars = nixVars;
      script = ''
        #!${bash}/bin/bash
        exec nix-instantiate "$@"
      '';
    };

    nix-shell = mkBin {
      name = "nix-shell";
      paths = [ bash git nix ];
      vars = nixVars;
      script = ''
        #!${bash}/bin/bash
        exec nix-shell "$@"
      '';
    };

    racketWithRackCheck = racketWithPackages [
      (applyPatches {
        name = "rackcheck-with-main";
        src = fetchGit {
          name = "rackcheck-src";
          url = "https://github.com/Bogdanp/rackcheck.git";
          ref = "master";
          rev = "21dcda3edf86c28d9594887e92c5d7bef589897c";
        };
        postPatch = ''
          rm -r examples
          rm -r rackcheck
        '';
      })
    ];

    repo-copies = mkBin {
      name = "repo-copies";
      vars = {
        repos = attrsToDirs' "repo-copies"
          (genAttrs [ "js-plumb" "php-core" "php-prelude" "php-easycheck" ]
            (name:
              (fetchGit {
                inherit name;
                url = "http://chriswarbo.net/git/${name}.git";
              }).outPath) // {
                nixpkgs = "${nixpkgs.path}";
                nix-helpers = "${nix-helpers-source}";
              });
      };
      script = ''
        #!${bash}/bin/bash
        echo "$repos"
      '';
    };
  };

  wrapScript = name: vals:
    mkBin (vals // {
      inherit name;
      file = vals.name or (./.. + "/${name}");
      paths = includingDeps (vals.paths or [ ]);
    });

  pythonScripts =
    genAttrs [ "cleanup" "htmlUnwrap" "relativise" "relTo" "stripTitle" ] (name:
      wrapScript name {
        paths = [ (python3.withPackages (p: [ p.python p.beautifulsoup4 ])) ];
      });

  wrapped = extras // pythonScripts // mapAttrs wrapScript {
    file2img = { };
    git2md = { paths = [ git wget ]; };
    mkRedirectTo = { vars = { TEMPLATE = ../redirectTemplate.html; }; };
    renderHcard = { };
    showPosts = { paths = [ wrapped.showPost ]; };
    wrapCode = { };

    render_page = {
      paths = [ fail pandoc panhandle panpipe ];
      vars = {
        defaultTemplate = ../../templates/default.html;
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
      };
    };

    showPost = {
      paths = [ replace xidel ];
      vars = { RANTS = ../rants; };
    };
  };
};
wrapped
