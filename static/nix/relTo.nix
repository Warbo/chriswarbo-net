{ commands, runCommand, withDeps, writeScript, xidel }:

with builtins;
with rec {
  relToTest = runCommand "relative" {
    buildInputs = [ xidel ];
    page = relToUntested "." (writeScript "test.html" ''
      <html>
        <head>
          <link rel="stylesheet" type="text/css"
                href="/css/default.css" />
          <meta http-equiv="refresh" content="0;URL=./projects.html" />
        </head>
        <body>
          <a href="/blog.html">Blog</a>
          <script src="/js/foo.js"></script>
          <img src="/images/foo.png" />
        </body>
      </html>
    '');
  } ''
    set -e

    function fail() {
      echo "Found absolute paths in $1" 1>&2
      exit 1
    }

    for PAIR in '//a/@href	anchors' \
                '//link/@href	links'   \
                '//script/@src	scripts' \
                '//img/@src	images'  \
                '//meta/@content	meta'
    do
      QUERY=$(echo "$PAIR" | cut -f1)
       TYPE=$(echo "$PAIR" | cut -f2)
      FOUND=$(xidel -s -e "$QUERY" - < "$page")

      echo "$FOUND" | grep "^/" || continue
      echo "$FOUND" | grep "=/" || continue
      fail "$TYPE"
    done

    if grep 'var url = "/' < "$page"
    then
      fail "script vars"
    fi

    mkdir "$out"
  '';

  relToUntested = TO_ROOT: file:
    runCommand "relative-${baseNameOf (unsafeDiscardStringContext file)}" {
      inherit file TO_ROOT;
      buildInputs = [ commands.relativise ];
    } ''
      echo "Relativising $file to $TO_ROOT" 1>&2
      relativise < "$file" > "$out"
    '';
};
x: y:
withDeps [ relToTest ] (relToUntested x y)
