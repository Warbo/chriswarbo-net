{ buildEnv, libxslt, makeWrapper, pythonPackages, runCommand, xidel }:

runCommand "rendering-commands"
  {
    buildInputs       = [ makeWrapper ];
  }
  ''
    mkdir -p "$out/bin"

    makeWrapper "${./static/stripEmptyPreCode}" "$out/bin/stripEmptyPreCode" \
      --prefix PATH : "${buildEnv {
          name  = "stripemptyprecode-env";
          paths = [ pythonPackages.python pythonPackages.beautifulsoup4 ];
        }}/bin"

    makeWrapper "${./static/relativise}" "$out/bin/relativise" \
      --prefix PATH : "${buildEnv {
          name  = "relativise-env";
          paths = [ libxslt ];
        }}/bin" \
      --set XSL "${./static/rel.xsl}"

    makeWrapper "${./static/showPost}" "$out/bin/showPost" \
      --prefix PATH : "${buildEnv {
          name  = "showpost-env";
          paths = [ xidel ];
        }}/bin"


    makeWrapper "${./static/showPosts}" "$out/bin/showPosts"

    makeWrapper "${./static/stripTitle}" "$out/bin/stripTitle" \
      --prefix PATH : "${buildEnv {
          name  = "striptitle-env";
          paths = [ pythonPackages.python pythonPackages.beautifulsoup4 ];
        }}/bin"
  ''
