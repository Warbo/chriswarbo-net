{ attrsToDirs', commands, darkhttpd, fail, git, haskell, haskellPackages, lib,
  linkchecker, mf2py, procps, pythonPackages, repos, runCommand, tidy-html5,
  untestedSite, utillinux, wget, wrap, xidel }:

with builtins;
with lib;
with rec {
  base = ../..;

  testScript = name: { buildInputs ? [], includeSite ? true }:
    runCommand "test-script-${name}"
      {
        inherit buildInputs;
        __noChroot = true;
        rendered   = if includeSite then untestedSite else "";
        script     = wrap {
          inherit name;
          file = base + "/tests/${name}";
        };
        # Files which tests might need
        static = attrsToDirs' "test-script-resources" {
          linkcheckerrc = base + "/static/linkcheckerrc";
        };
      }
      ''
        set -e

        # Put config files in place
        cp -rs "$static" ./static

        # Run the test
        "$script"
        mkdir "$out"
      '';
};
mapAttrs testScript {
  all_pages_reachable           = {
    buildInputs = [ darkhttpd procps utillinux wget ];
  };
  archive_is_hfeed              = {
    buildInputs = [ pythonPackages.python mf2py ];
  };
  broken_links                  = { buildInputs = [ linkchecker ]; };
  cleanupTables                 = {
    buildInputs = [ commands.cleanup ];
    includeSite = false;
  };
  code_not_indented             = { buildInputs = [ fail ]; };
  dirs_have_indices             = {};
  empty_panpipe_blocks_stripped = {
    buildInputs = [ commands.render_page ];
    includeSite = false;
  };
  essays_redirects_to_projects  = { buildInputs = [ commands.relTo ]; };
  everything_suffixed           = {};
  have_all_posts                = { buildInputs = [ utillinux xidel ]; };
  have_all_projects             = { buildInputs = [ utillinux xidel ]; };
  have_all_repos                = {};
  have_feeds                    = {};
  have_readmes                  = { buildInputs = [ git ]; };
  hcard_test                    = {
    buildInputs = [ pythonPackages.python mf2py commands.renderHcard ];
    includeSite = false;
  };
  homepage_has_hcard            = {
    buildInputs = [ pythonPackages.python mf2py ];
  };
  imagesWontCompressFurther     = { includeSite = false; };
  index_pages                   = {};
  no_absolutes                  = { buildInputs = [ xidel ]; };
  no_blogspot                   = { buildInputs = [ xidel ]; };
  no_cruft                      = {};
  no_empty_files                = {};
  no_essays_links               = {};
  no_gitorious                  = {};
  no_selfclosing_scripts        = { buildInputs = [ fail xidel ]; };
  posts_are_hentries            =
    with {
      hsPkgs = haskellPackages.override (old: {
        overrides = lib.composeExtensions
          (old.overrides or (_: _: {}))
          (self: super: {
            # Avoid doctest failure:
            #   expected: [("a",Number 4.0),("b",Number 7.0)]
            #   but got: [("b",Number 7.0),("a",Number 4.0)]
            lens-aeson = haskell.lib.dontCheck super.lens-aeson;
          });
      });
    };
    {
      buildInputs = [
        (hsPkgs.ghcWithPackages (h: [
          h.microformats2-parser
          h.directory
          h.bytestring
        ]))
      ];
    };
  posts_have_titles = { buildInputs = [ fail xidel ]; };
  redirect_posts    = {                               };
  scripts_in_place  = { buildInputs = [ fail xidel ]; };
  tidy_html5        = { buildInputs = [ tidy-html5 ]; };
  xidel_args        = { buildInputs = [ xidel      ]; };
} // {
  reposRedirect = runCommand "reposRedirect"
    { pages = attrsToDirs' "repos" repos; }
    ''
      FOUND=0
      for page in "$pages"/*
      do
        FOUND=1
        grep '/git/[^"]' < "$page" > /dev/null || {
          echo "No redirect found in '$page'" 1>&2
          exit 1
        }
      done

      [[ "$FOUND" -gt 0 ]] || {
        echo "No repo pages found" 1>&2
        exit 1
      }

      echo "pass" > "$out"
    '';
}
