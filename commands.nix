{ buildEnv, git, glibcLocales, lib, makeWrapper, nix, pandoc, panhandle,
  panpipe, pythonPackages, runCommand, wget, xidel, xmlstarlet }:

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

  file2img =
    wrap [] {} ./static/file2img;

  git2md =
    wrap [ git wget ] {} ./static/git2md;

  nixInstantiate =
    wrap [ nix ] { NIX_PATH   = getEnv "NIX_PATH";
                   NIX_REMOTE = getEnv "NIX_REMOTE"; }
         "${nix}/bin/nix-instantiate";

  nixShell =
    wrap [ nix ] { NIX_PATH   = getEnv "NIX_PATH";
                   NIX_REMOTE = getEnv "NIX_REMOTE"; } "${nix}/bin/nix-shell";


  relativise =
    wrap [ xmlstarlet ] {} ./static/relativise;

  render_page =
    wrap [ cleanup pandoc panhandle panpipe ]
         { defaultTemplate = ./templates/default.html;
           LANG = "en_US.UTF-8";
           LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
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

  wrapCode =
    wrap [] {} ./static/wrapCode;
}
