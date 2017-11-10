{ attrsToDirs, git, haskellPackages, lib, linkchecker, mf2py, pages, procps,
  pythonPackages, repoPages, runCommand, tidy-html5, utillinux, wget,
  writeScript, xidel }:

with builtins;
with lib;
with pages;
with rec {
  testScript = name: { buildInputs ? [], script }:
    runCommand "test-script-${name}"
      {
        inherit buildInputs script untestedSite;

        # Files which tests might need
        static = attrsToDirs { linkcheckerrc = ./static/linkcheckerrc; };
      }
      ''
        set -e

        # Put config files in place
        cp -rs "$static" ./static

        # Tests look in 'rendered' for the site's HTML, etc.
        cp -rs "$untestedSite" rendered

        # Make an empty "git", to prevent the broken link checker flagging it
        chmod +w -R rendered
        touch rendered/git

        # Run the test
        "$script"
        mkdir "$out"
      '';
};
mapAttrs testScript {
  all_pages_reachable           = {
    buildInputs = [ procps pythonPackages.python utillinux wget ];
    script      = ./tests/all_pages_reachable;
  };
  archive_is_hfeed              = {
    buildInputs = [ pythonPackages.python mf2py ];
    script      = ./tests/archive_is_hfeed;
  };
  broken_links                  = {
    buildInputs = [ linkchecker ];
    script      = ./tests/broken_links;
  };
  cleanupTables                 = {
    buildInputs = [ commands.cleanup ];
    script = ./tests/cleanupTables;
  };
  code_not_indented               = {
    script = ./tests/code_not_indented;
  };
  dirs_have_indices             = {
    script = ./tests/dirs_have_indices;
  };
  empty_panpipe_blocks_stripped = {
    buildInputs = [ commands.render_page ];
    script = ./tests/empty_panpipe_blocks_stripped;
  };
  essays_redirects_to_projects  = {
    buildInputs = [ commands.relTo ];
    script      = ./tests/essays_redirects_to_projects;
  };
  everything_suffixed           = {
    script = ./tests/everything_suffixed;
  };
  have_all_posts                = {
    buildInputs = [ utillinux xidel ];
    script      = ./tests/have_all_posts;
  };
  have_all_projects             = {
    buildInputs = [ utillinux xidel ];
    script      = ./tests/have_all_projects;
  };
  have_all_repos                = {
    script = ./tests/have_all_repos;
  };
  have_feeds                    = {
    script = ./tests/have_feeds;
  };
  have_readmes                  = {
    buildInputs = [ git ];
    script      = ./tests/have_readmes;
  };
  hcard_test                    = {
    buildInputs = [ pythonPackages.python mf2py commands.renderHcard ];
    script      = ./tests/hcard_test;
  };
  homepage_has_hcard            = {
    buildInputs = [ pythonPackages.python mf2py ];
    script      = ./tests/homepage_has_hcard;
  };
  imagesWontCompressFurther     = {
    script = ./tests/imagesWontCompressFurther;
  };
  no_absolutes                  = {
    buildInputs = [ xidel ];
    script      = ./tests/no_absolutes;
  };
  no_blogspot                   = {
    buildInputs = [ xidel ];
    script      = ./tests/no_blogspot;
  };
  no_cruft                      = {
    script = ./tests/no_cruft;
  };
  no_empty_files                = {
    script = ./tests/no_empty_files;
  };
  no_essays_links               = {
    script = ./tests/no_essays_links;
  };
  no_gitorious                  = {
    script = ./tests/no_gitorious;
  };
  posts_are_hentries            = {
    buildInputs = [ (haskellPackages.ghcWithPackages (h: [
                      h.microformats2-parser
                      h.directory
                      h.bytestring
                    ])) ];
    script      = ./tests/posts_are_hentries;
  };
  posts_have_titles             = {
    buildInputs = [ xidel ];
    script      = ./tests/posts_have_titles;
  };
  redirect_posts                = {
    script = ./tests/redirect_posts;
  };
  tidy_html5                    = {
    buildInputs = [ tidy-html5 ];
    script      = ./tests/tidy_html5;
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
