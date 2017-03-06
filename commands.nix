{ buildEnv, lib, libxslt, makeWrapper, nix, pandoc, panhandle, panpipe,
  pythonPackages, runCommand, xidel }:

with builtins;
with lib;
with {
  wrap = deps: vars: file: runCommand "wrapped"
    {
      inherit file;
      buildInputs = [ makeWrapper ];
      command     = baseNameOf file;
      env         = buildEnv {
                      name  = "stripemptyprecode-env";
                      paths = deps;
                    };
      vars        = concatStringsSep " "
                      (fold (n: r: let v = vars."${n}";
                                    in [''--set "${n}" "${v}"''] ++ r)
                            []
                            (attrNames vars));
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$file" "$out/bin/$command" $vars --prefix PATH : "$env/bin"
  '';
};
rec {
  cleanup =
    wrap [ stripEmptyPreCode summariseTables ] {} ./static/cleanup;

  nixShell =
    wrap [ nix ] { NIX_PATH   = getEnv "NIX_PATH";
                   NIX_REMOTE = getEnv "NIX_REMOTE"; } "${nix}/bin/nix-shell";

  relativise =
    wrap [ libxslt ] { XSL = ./static/rel.xsl; } ./static/relativise;

  render_page =
    wrap [ cleanup pandoc panhandle panpipe ]
         { defaultTemplate = ./templates/default.html; }
         ./static/render_page;

  showPost =
    wrap [ xidel ] {} ./static/showPost;

  showPosts =
    wrap [ showPost ] {} ./static/showPosts;

  stripEmptyPreCode =
    wrap [ pythonPackages.python pythonPackages.beautifulsoup4 ] {}
         ./static/stripEmptyPreCode;

  stripTitle =
    wrap [ pythonPackages.python pythonPackages.beautifulsoup4 ] {}
         ./static/stripTitle;

  summariseTables =
    wrap [ pythonPackages.python pythonPackages.beautifulsoup4 ] {}
         ./static/summariseTables;
}
