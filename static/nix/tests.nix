{ attrsToDirs', commands, darkhttpd, fail, git, haskell, haskellPackages, lib,
  linkchecker, mf2py, pandocPkgs, procps, pythonPackages, repos, runCommand,
  tidy-html5, untestedSite, utillinux, wget, wrap, xidel }:

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
        ln -s "$static" ./static

        # Run the test
        "$script"
        mkdir "$out"
      '';
};
mapAttrs testScript {
  all_pages_reachable            = {
    buildInputs = [ darkhttpd procps utillinux wget ];
  };
  archive_is_hfeed               = {
    buildInputs = [ pythonPackages.python mf2py ];
  };
  broken_links                   = { buildInputs = [ linkchecker ]; };
  cleanupTables                  = {
    buildInputs = [ commands.cleanup ];
    includeSite = false;
  };
  code_not_indented              = { buildInputs = [ fail ]; };
  dirs_have_indices              = {};
  empty_panpipe_blocks_stripped  = {
    buildInputs = [ commands.render_page ];
    includeSite = false;
  };
  essays_redirects_to_projects   = { buildInputs = [ commands.relTo ]; };
  everything_suffixed            = {};
  have_all_posts                 = { buildInputs = [ utillinux xidel ]; };
  have_all_projects              = { buildInputs = [ utillinux xidel ]; };
  have_all_repos                 = {};
  have_feeds                     = {};
  have_readmes                   = { buildInputs = [ git ]; };
  hcard_test                     = {
    buildInputs = [ pythonPackages.python mf2py commands.renderHcard ];
    includeSite = false;
  };
  homepage_has_hcard             = {
    buildInputs = [ pythonPackages.python mf2py ];
  };
  htmlunwrap_unwraps_blocks      = {
    buildInputs = [ commands.htmlUnwrap fail ];
    includeSite = false;
  };
  imagesWontCompressFurther      = { includeSite = false;          };
  index_pages                    = {};
  no_absolutes                   = { buildInputs = [      xidel ]; };
  no_blogspot                    = { buildInputs = [      xidel ]; };
  no_cruft                       = {};
  no_empty_files                 = {};
  no_essays_links                = {};
  no_gitorious                   = {};
  no_selfclosing_scripts         = { buildInputs = [ fail xidel ]; };
  posts_are_hentries             = {
    buildInputs = [ (haskellPackages.ghcWithPackages (h: [
      h.microformats2-parser
      h.directory
      h.bytestring
    ])) ];
  };
  posts_have_titles  = { buildInputs = [ fail xidel                  ]; };
  redirect_posts     = {};
  scripts_in_place   = { buildInputs = [ fail xidel                  ]; };
  summariesUnwrapped = {
    buildInputs = [ commands.cleanup pandocPkgs ];
    includeSite = false;
  };
  tidy_html5         = { buildInputs = [ tidy-html5                  ]; };
  xidel_args         = {
    buildInputs = [ xidel ];
    includeSite = false;
  };
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
