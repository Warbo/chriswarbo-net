{ fail, hfeed2atom, libxslt, runCommand, topLevel }:

with rec {
  atom = runCommand "blog.atom"
    {
      blog        = topLevel."blog.html";
      buildInputs = [ hfeed2atom ];
    }
    ../mkAtom;

  rss = runCommand "blog.rss"
    {
      inherit atom;
      XSL         = ../atom2rss.xsl;
      buildInputs = [ fail libxslt.bin ];
    }
    ../mkRss;
};
{
  resources = {
    "blog.atom" = atom;
    "blog.rss"  = rss;
    css         = ../../css;
    js          = ../../js;
  };
}
