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
  repos      = self.callPackage ./repos.nix      {};
  resources  = self.callPackage ./resources.nix  {};
  tests      = self.callPackage ./tests.nix      {};
  unfinished = self.callPackage ./unfinished.nix {};


  mkRedirectTo = self.callPackage ./redirects.nix {};
  inherit (self.callPackage ./render.nix    { inherit self; }) render renderAll;


  allPages = self.stripOverrides (self.merge [
    self.blog
    self.projects
    self.resources
    self.unfinished
  ]);

  untestedSite = self.attrsToDirs' "untestedSite" self.allPages;
  wholeSite    = self.withDeps' "site" (self.allDrvsIn self.tests)
                                       self.untestedSite;
}
