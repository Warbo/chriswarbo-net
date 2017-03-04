{ callPackage, lib, makeWrapper, pandoc, panhandle, panpipe, runCommand,
  stdenv }:

with builtins;
with lib;
rec {
  attrsToDirs = attrs:
    with rec {
      names     = attrNames attrs;
      dataOf    = name: attrsToDirs attrs."${name}";
      nameToCmd = name: ''
        cp -r "${dataOf name}" "$out/${name}"
      '';
    };
    if typeOf attrs == "path" || attrs ? builder
       then attrs
       else stdenv.mkDerivation {
              name = "collated-data";
              buildCommand = ''
                mkdir -p "$out"
                ${concatStringsSep "\n" (map nameToCmd names)}
              '';
            };

  cleanup = runCommand "cleanup"
    {
      cleanup = ./static/cleanup;
      buildInputs = [ makeWrapper ];
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$cleanup" "$out/bin/cleanup"
    '';

  commands = callPackage ./commands.nix {};

  render_page = runCommand "render_page"
    {
      buildInputs = [ makeWrapper ];
      script      = ./static/render_page;
      template    = ./templates/default.html;
      inherit cleanup pandoc panhandle panpipe;
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$script" "$out/bin/render_page" \
        --prefix PATH : "$commands/bin"  \
        --prefix PATH : "$cleanup/bin"   \
        --prefix PATH : "$pandoc/bin"    \
        --prefix PATH : "$panhandle/bin" \
        --prefix PATH : "$panpipe/bin"   \
        --set           defaultTemplate "$template"
    '';

  render = { cwd, file, inputs, vars }: runCommand "rendered.html" (vars // {
             buildInputs = [ commands render_page ] ++ inputs;
             inherit cwd file;
           })
           ''
             cd "$cwd"
             SOURCE="$file" DEST="$out" render_page < "$file" > "$out"
           '';

  rel = dir: runCommand "relativised" { buildInputs = [ commands ]; } ''
    cp -r "${dir}" "$out"
    chmod +w -R "$out"
    cd "$out"

    while read -r F
    do
      DIR=$(dirname "$F" | sed -e 's@/[^/][^/]*@/..@g')
      TO_ROOT="$DIR" relativise "$F"
    done < <(find . -name "*.html")
  '';

  site = rel (attrsToDirs {
    "index.html" = render {
      cwd    = attrsToDirs { static = ./static; rendered = { blog = {}; }; };
      file   = ./index.md;
      inputs = [];
      vars   = {};
    };
  });
}
