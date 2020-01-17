{ beautifulsoup-custom, blog, fail, hfeed2atom, libxslt, python, render, run }:

with rec {
  py = python.withPackages (p: [
    hfeed2atom
    beautifulsoup-custom
  ]);

  atom = run {
    name  = "blog.atom";
    file  = ../mkAtom;
    paths = [ hfeed2atom py ];
    vars  = { inherit blog; };
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
    "blog.atom"    = atom;
    "blog.rss"     = rss;
    css            = ../../css;
    js             = ../../js;
    "contact.html" = render {
      name        = "contact.html";
      file        = ../../contact.md;
      TO_ROOT     = ".";
      SOURCE_PATH = "contact.md";
    };
  };
}
