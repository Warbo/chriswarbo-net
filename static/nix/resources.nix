{ blog, fail, hfeed2atom, libxslt, redirect, python3, render, run }:

with rec {
  py = python3.withPackages (p: [
    hfeed2atom
    p.beautifulsoup4
  ]);

  atom = run {
    name  = "blog.atom";
    file  = ../mkAtom;
    paths = [ py ];
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
    "prelude.txt" = redirect {
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
