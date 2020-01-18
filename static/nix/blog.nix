{ attrsToDirs', mkRedirectTo, render, renderAll }:

with rec {
  contents  = renderAll "blog";
  blogPages = attrsToDirs' "blogPages" contents;
};

# Posts, redirects, index pages and other things which need a list of blog posts
{
  blog = contents // {
    "index.html" = render {
      name        = "index.html";
      vars        = { inherit blogPages; };
      file        = ../../blog.md;
      TO_ROOT     = "..";
      SOURCE_PATH = "blog.md";
    };
  };

  "blog.html" = mkRedirectTo {
    from = "blog.html";
    to   = "/blog/";
    rel  = ".";
  };

  "index.html" = render {
    name        = "index.html";
    vars        = { inherit blogPages; };
    file        = ../../index.md;
    TO_ROOT     = ".";
    SOURCE_PATH = "index.md";
  };

  "index.php" = render {
    name        = "index.php";
    vars        = { inherit blogPages; };
    file        = ../../redirect.md;
    SOURCE_PATH = "redirect.md";
  };
}
