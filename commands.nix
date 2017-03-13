{ buildEnv, haskellPackages, hfeed2atom, git, glibcLocales, lib, libxslt,
  makeWrapper, nix, pandoc, panhandle, panpipe, pythonPackages, runCommand,
  wget, xidel, xmlstarlet }:

with builtins;
with lib;
with {
  wrap = deps: vars: file: runCommand "wrapped"
    rec {
      inherit file;
      buildInputs = [ makeWrapper ];
      command     = baseNameOf file;
      env         = buildEnv {
                      name  = unsafeDiscardStringContext command + "-env";
                      paths = deps ++ map (d: d.propagatedBuildInputs ++
                                              d.propagatedNativeBuildInputs)
                                          deps;
                    };
      varargs     = concatStringsSep " "
                      (fold (n: r: let v = vars."${n}";
                                    in [''--set "${n}" "${v}"''] ++ r)
                            []
                            (attrNames vars));
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$file" "$out/bin/$command" $varargs \
                  --prefix PATH : "$env/bin"
    '';
};
rec {
  cleanup =
    wrap [ stripEmptyPreCode summariseTables ] {} ./static/cleanup;

  file2img =
    wrap [] {} ./static/file2img;

  git2md =
    wrap [ git wget ] {} ./static/git2md;

  mkEssayLinks =
    wrap [ mkRedirectTo ] {} ./static/mkEssayLinks;

  mkRedirectTo =
    wrap [] { TEMPLATE = ./static/redirectTemplate.html; }
         ./static/mkRedirectTo;

  mkRss =
    wrap [ libxslt ] { XSL = ./static/atom2rss.xsl; } ./static/mkRss;

  nixInstantiate =
    wrap [ nix ] { NIX_PATH   = getEnv "NIX_PATH";
                   NIX_REMOTE = getEnv "NIX_REMOTE"; }
         "${nix}/bin/nix-instantiate";

  nixShell =
    wrap [ nix ] { NIX_PATH   = getEnv "NIX_PATH";
                   NIX_REMOTE = getEnv "NIX_REMOTE"; } "${nix}/bin/nix-shell";

  relativise =
    wrap [ xmlstarlet ] {} ./static/relativise;

  relTo =
    wrap [ haskellPackages.ghc ] {} ./static/relTo;

  render_page =
    wrap [ cleanup pandoc panhandle panpipe ]
         { defaultTemplate = ./templates/default.html;
           LANG = "en_US.UTF-8";
           LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; }
         ./static/render_page;

  renderHcard =
    wrap [] {} ./static/renderHcard;

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
