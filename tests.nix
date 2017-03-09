{ pages, runCommand, writeScript, xidel }:

with builtins;
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
};
rec {
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
