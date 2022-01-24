self: super:

with builtins;
with super.lib;
(if super ? nix-helpers then (x: x) else trace (toJSON {
  warning = "'nix-helpers' not found; has nix-helpers overlay been included?";
}))
{
  # Load our components
  blog       = self.callPackage ./blog.nix       {};
  commands   = self.callPackage ./commands.nix   {};
  projects   = self.callPackage ./projects.nix   {};
  redirect   = self.callPackage ./redirect.nix   {};
  relTo      = self.callPackage ./relTo.nix      {};
  render     = self.callPackage ./render.nix     {};
  renderAll  = self.callPackage ./renderAll.nix  {};
  repos      = self.callPackage ./repos.nix      {};
  resources  = self.callPackage ./resources.nix  {};
  siteTests  = self.callPackage ./siteTests.nix  {};
  unfinished = self.callPackage ./unfinished.nix {};

  # Combine all pages together into a directory
  untestedSite = with self; attrsToDirs' "untestedSite" (stripOverrides (merge [
    blog
    projects
    resources
    unfinished
  ]));

  # Provide all pages, with all of our tests as dependencies
  wholeSite = self.withDeps' "chriswarbo.net" (self.allDrvsIn self.siteTests)
                                              self.untestedSite;
}
