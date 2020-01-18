{ beautifulsoup-custom, blog, fail, hfeed2atom, libxslt, mkRedirectTo, python,
  render, run }:

with rec {
  py = python.withPackages (p: [
    hfeed2atom
    beautifulsoup-custom
  ]);

  atom = run {
    name  = "blog.atom";
    file  = ../mkAtom;
    paths = [ hfeed2atom py ];
    vars  = { blog = blog.blog."index.html"; };
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
  "blog.atom"    = atom;
  "blog.rss"     = rss;
  css            = ../../css;
  js             = ../../js;
  data_custom    = {
    # There are links to this in the wild, but data_custom itself is left over
    # from when the site used ocPortal
    "prelude.txt" = mkRedirectTo {
      from = "prelude.txt";
      to   = "/git/php-prelude";
    };
  };
  "contact.html" = render {
    name        = "contact.html";
    file        = ../../contact.md;
    TO_ROOT     = ".";
    SOURCE_PATH = "contact.md";
  };
}
