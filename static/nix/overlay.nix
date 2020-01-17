self: super:

with builtins;
with super.lib;
assert super ? nix-helpers || abort (toJSON {
  error = "'nix-helpers' not found; has nix-helpers overlay been included?";
});
{
  blog     = self.callPackage ./blog.nix     {};
  commands = self.callPackage ./commands.nix {};
  projects = self.callPackage ./projects.nix {};
  relTo    = self.callPackage ./relTo.nix    {};

  inherit (self.callPackage ./resources.nix {               }) resources;
  inherit (self.callPackage ./redirects.nix {               }) redirects mkRedirectTo;
  inherit (self.callPackage ./render.nix    { inherit self; }) render renderAll;

  # Attrsets of rendered sub-directory pages
  unfinished = self.renderAll "unfinished";

  # Derivations for whole sub-directories
  unfinishedPages = self.attrsToDirs' "unfinished" self.unfinished;

  topLevel     = mapAttrs' (n: val: rec {
                             name  = "${removeSuffix ".md" n}.html";
                             value = self.render (val // {
                               inherit name;
                               file        = ../.. + "/${n}";
                               TO_ROOT     = ".";
                               SOURCE_PATH = n;
                             });
                           }) {
    "contact.md"    = {                                             };
    "unfinished.md" = { vars = { inherit (self) unfinishedPages; }; };
  };

  allPages = self.blog      //
             self.projects  //
             self.redirects //
             self.resources //
             self.topLevel  // {
    unfinished  = self.unfinishedPages;
  };

  tests        = self.callPackage ./tests.nix {};

  untestedSite = self.attrsToDirs' "untestedSite" self.allPages;

  wholeSite    = self.withDeps (self.allDrvsIn self.tests) self.untestedSite;

  inherit (self.callPackage ./repos.nix {})
    projectRepos repoPages;
}
