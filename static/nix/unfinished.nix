{ attrsToDirs', redirect, render, renderAll }:

with rec {
  contents        = renderAll "unfinished";
  unfinishedPages = attrsToDirs' "unfinishedPages" contents;
};
{
  unfinished = contents // {
    "index.html" = render {
      name        = "index.html";
      vars        = { inherit unfinishedPages; };
      file        = ../../unfinished.md;
      TO_ROOT     = "..";
      SOURCE_PATH = "unfinished.md";
    };
  };

  "unfinished.html" = redirect {
    from = "unfinished.html";
    to   = "/unfinished/";
    rel  = ".";
  };
}
