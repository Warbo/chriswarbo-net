{ git, haskellPackages, lib, linkchecker, mf2py, pages, procps, pythonPackages,
  runCommand, tidy-html5, utillinux, wget, writeScript, xidel }:

with builtins;
with lib;
with pages;
with rec {
  relPage = (mkRel {
    "test.html" = writeScript "test.html" ''
      <html>
        <head>
          <link rel="stylesheet" type="text/css" href="/css/default.css" />
          <meta http-equiv="refresh" content="0;URL=./projects.html" />
        </head>
        <body>
          <a href="/blog.html">Blog</a>
          <script src="/js/foo.js"></script>
          <img src="/images/foo.png" />
        </body>
      </html>
    '';
  })."test.html";

  testScript = { buildInputs ? [], script }: runCommand "test-script"
    { inherit buildInputs script untested; }
    ''
      cp -r "$untested" rendered
      chmod +w -R rendered
      rm -r rendered/git
      touch rendered/git
      "$script" && touch "$out"
    '';
};
mapAttrs (n: testScript) {
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
  relative = runCommand "relative"
    {
      buildInputs = [ xidel ];
      page        = relPage;
    }
    ''
      function fail() {
        echo "Found absolute paths in $1" 1>&2
        exit 1
      }

      xidel -q -e '//a/@href'       - < "$page" | grep "^/" && fail "anchors"
      xidel -q -e '//link/@href'    - < "$page" | grep "^/" && fail "links"
      xidel -q -e '//script/@src'   - < "$page" | grep "^/" && fail "scripts"
      xidel -q -e '//img/@src'      - < "$page" | grep "^/" && fail "images"
      xidel -q -e '//meta/@content' - < "$page" | grep "=/" && fail "meta"

      grep 'var url = "/' < "$page" && fail "script vars"

      touch "$out"
    '';
}
