self: super:

with builtins;
with super.lib;
(if super ? nix-helpers then (x: x) else trace (toJSON {
  warning = "'nix-helpers' not found; has nix-helpers overlay been included?";
}))
{
  blog       = self.callPackage ./blog.nix       {};
  commands   = self.callPackage ./commands.nix   {};
  projects   = self.callPackage ./projects.nix   {};
  redirect   = self.callPackage ./redirect.nix   {};
  relTo      = self.callPackage ./relTo.nix      {};
  render     = self.callPackage ./render.nix     {};
  renderAll  = self.callPackage ./renderAll.nix  {};
  repos      = self.callPackage ./repos.nix      {};
  resources  = self.callPackage ./resources.nix  {};
  tests      = self.callPackage ./tests.nix      {};
  unfinished = self.callPackage ./unfinished.nix {};

  untestedSite = self.attrsToDirs' "untestedSite"
    (self.stripOverrides (self.merge [
      self.blog
      self.projects
      self.resources
      self.unfinished
    ]));

  wholeSite = self.withDeps' "site" (self.allDrvsIn self.tests)
                                    self.untestedSite;
}
