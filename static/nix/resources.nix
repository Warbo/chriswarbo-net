{ fail, hfeed2atom, libxslt, run, topLevel }:

with rec {
  atom = run {
    name  = "blog.atom";
    file  = ../mkAtom;
    paths = [ hfeed2atom ];
    vars  = { blog = topLevel."blog.html"; };
  };

  rss = run {
    name  = "blog.rss";
    file  = ../mkRss;
    paths = [ fail libxslt.bin ];
    vars  = {
      inherit atom;
      XSL = ../atom2rss.xsl;
    };
  };
};
{
  resources = {
    "blog.atom" = atom;
    "blog.rss"  = rss;
    css         = ../../css;
    js          = ../../js;
  };
}
