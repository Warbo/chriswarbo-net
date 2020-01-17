self: super:

with builtins;
with super.lib;
assert super ? nix-helpers || abort (toJSON {
  error = "'nix-helpers' not found; has nix-helpers overlay been included?";
});
{
  blog       = self.callPackage ./blog.nix       {};
  commands   = self.callPackage ./commands.nix   {};
  projects   = self.callPackage ./projects.nix   {};
  relTo      = self.callPackage ./relTo.nix      {};
  unfinished = self.callPackage ./unfinished.nix {};

  inherit (self.callPackage ./resources.nix {               }) resources;
  inherit (self.callPackage ./redirects.nix {               }) redirects mkRedirectTo;
  inherit (self.callPackage ./render.nix    { inherit self; }) render renderAll;

  topLevel = {
    "contact.md" = render {
      name        = "contact.html";
      file        = ../../contact.md;
      TO_ROOT     = ".";
      SOURCE_PATH = "contact.md";
    };
  };

  allPages = self.blog      //
             self.projects  //
             self.redirects //
             self.resources //
             self.topLevel  //
             self.unfinished;

  tests        = self.callPackage ./tests.nix {};

  untestedSite = self.attrsToDirs' "untestedSite" self.allPages;

  wholeSite    = self.withDeps (self.allDrvsIn self.tests) self.untestedSite;

  inherit (self.callPackage ./repos.nix {})
    projectRepos repoPages;
}
