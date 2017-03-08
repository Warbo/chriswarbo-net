{ pages, runCommand, writeScript, xidel }:

with builtins;
with pages;
with rec {
  relPage = (mkRel {
    "index.html" = ./projects/index.html;
  })."index.html";
};
rec {
  relAnchors = runCommand "relAnchors"
    {
      buildInputs = [ xidel ];
      page        = relPage;
    }
    ''
      if xidel -q -e '//a/@href' - < "$page" | grep "^/"
      then
        echo "Found absolute paths in anchors" 1>&2
        exit 1
      fi
      touch "$out"
    '';

  relLinks = runCommand "relLinks"
    {
      buildInputs = [ xidel ];
      page        = relPage;
    }
    ''
      if xidel -q -e '//link/@href' - < "$page" | grep "^/"
      then
        echo "Found absolute paths in links" 1>&2
        exit 1
      fi
      touch "$out"
    '';
}
