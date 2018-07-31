{ attrsToDirs, git, haskellPackages, lib, linkchecker, mf2py, pages, procps,
  pythonPackages, repoPages, runCommand, tidy-html5, utillinux, wget,
  writeScript, xidel }:

with builtins;
with lib;
with pages;
with rec {
  base = ../..;

  testFile = f: base + "/tests/${f}";

  testScript = name: { buildInputs ? [], script, includeSite ? true }:
    runCommand "test-script-${name}"
      {
        inherit buildInputs script;
        rendered = if includeSite then untestedSite else "";
        # Files which tests might need
        static = attrsToDirs {
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
    buildInputs = [ procps pythonPackages.python utillinux wget ];
    script      = testFile "all_pages_reachable";
  };
  archive_is_hfeed              = {
    buildInputs = [ pythonPackages.python mf2py ];
    script      = testFile "archive_is_hfeed";
  };
  broken_links                  = {
    buildInputs = [ linkchecker ];
    script      = testFile "broken_links";
  };
  cleanupTables                 = {
    buildInputs = [ commands.cleanup ];
    script      = testFile "cleanupTables";
    includeSite = false;
  };
  code_not_indented               = {
    script = testFile "code_not_indented";
  };
  dirs_have_indices             = {
    script = testFile "dirs_have_indices";
  };
  empty_panpipe_blocks_stripped = {
    buildInputs = [ commands.render_page ];
    script      = testFile "empty_panpipe_blocks_stripped";
    includeSite = false;
  };
  essays_redirects_to_projects  = {
    buildInputs = [ commands.relTo ];
    script      = testFile "essays_redirects_to_projects";
  };
  everything_suffixed           = {
    script = testFile "everything_suffixed";
  };
  have_all_posts                = {
    buildInputs = [ utillinux xidel ];
    script      = testFile "have_all_posts";
  };
  have_all_projects             = {
    buildInputs = [ utillinux xidel ];
    script      = testFile "have_all_projects";
  };
  have_all_repos                = {
    script = testFile "have_all_repos";
  };
  have_feeds                    = {
    script = testFile "have_feeds";
  };
  have_readmes                  = {
    buildInputs = [ git ];
    script      = testFile "have_readmes";
  };
  hcard_test                    = {
    buildInputs = [ pythonPackages.python mf2py commands.renderHcard ];
    script      = testFile "hcard_test";
    includeSite = false;
  };
  homepage_has_hcard            = {
    buildInputs = [ pythonPackages.python mf2py ];
    script      = testFile "homepage_has_hcard";
  };
  imagesWontCompressFurther     = {
    script      = testFile "imagesWontCompressFurther";
    includeSite = false;
  };
  no_absolutes                  = {
    buildInputs = [ xidel ];
    script      = testFile "no_absolutes";
  };
  no_blogspot                   = {
    buildInputs = [ xidel ];
    script      = testFile "no_blogspot";
  };
  no_cruft                      = {
    script = testFile "no_cruft";
  };
  no_empty_files                = {
    script = testFile "no_empty_files";
  };
  no_essays_links               = {
    script = testFile "no_essays_links";
  };
  no_gitorious                  = {
    script = testFile "no_gitorious";
  };
  posts_are_hentries            = {
    buildInputs = [ (haskellPackages.ghcWithPackages (h: [
                      h.microformats2-parser
                      h.directory
                      h.bytestring
                    ])) ];
    script      = testFile "posts_are_hentries";
  };
  posts_have_titles             = {
    buildInputs = [ xidel ];
    script      = testFile "posts_have_titles";
  };
  redirect_posts                = {
    script = testFile "redirect_posts";
  };
  tidy_html5                    = {
    buildInputs = [ tidy-html5 ];
    script      = testFile "tidy_html5";
  };
} // {
  reposRedirect = runCommand "reposRedirect"
    {
      pages = attrsToDirs repoPages;
    }
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
