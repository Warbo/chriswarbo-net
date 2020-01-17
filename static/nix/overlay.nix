self: super:

with builtins;
with super.lib;
assert super ? nix-helpers || abort (toJSON {
  error = "'nix-helpers' not found; has nix-helpers overlay been included?";
});
{
  commands = self.callPackage ./commands.nix {};
  relTo    = self.callPackage ./relTo.nix    {};

  inherit (self.callPackage ./projects.nix  {               }) projectPages projects;
  inherit (self.callPackage ./resources.nix {               }) resources;
  inherit (self.callPackage ./redirects.nix {               }) redirects;
  inherit (self.callPackage ./render.nix    { inherit self; }) render renderAll;

  # Attrsets of rendered sub-directory pages
  blog       = self.renderAll "blog";
  unfinished = self.renderAll "unfinished";

  # Derivations for whole sub-directories
  blogPages       = self.attrsToDirs' "blog"       self.blog;
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
    "index.md"      = { vars = { inherit (self) blogPages;       }; };
    "blog.md"       = { vars = { inherit (self) blogPages;       }; };
    "contact.md"    = {                                             };
    "unfinished.md" = { vars = { inherit (self) unfinishedPages; }; };
  };

  allPages = self.topLevel // self.redirects // self.resources // {
    blog        = self.blogPages;
    projects    = self.projectPages;
    unfinished  = self.unfinishedPages;
    "index.php" = self.render {
      vars        = { inherit (self) blogPages; };
      file        = ../../redirect.md;
      name        = "index.php";
      SOURCE_PATH = "redirect.md";
    };
  };

  tests        = self.callPackage ./tests.nix {};

  untestedSite = self.attrsToDirs' "untestedSite" self.allPages;

  wholeSite    = self.withDeps (self.allDrvsIn self.tests) self.untestedSite;

  inherit (self.callPackage ./repos.nix {})
    projectRepos repoPages;
}
