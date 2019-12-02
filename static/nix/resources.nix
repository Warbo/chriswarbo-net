{ beautifulsoup-custom, fail, hfeed2atom, libxslt, python, run, topLevel }:

with rec {
  py = python.withPackages (p: [
    hfeed2atom
    beautifulsoup-custom
  ]);

  atom = run {
    name  = "blog.atom";
    file  = ../mkAtom;
    paths = [ hfeed2atom py ];
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
