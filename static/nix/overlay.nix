self: super:

with builtins;
with super.lib;
assert super ? nix-helpers || abort (toJSON {
  error = "'nix-helpers' not found; has nix-helpers overlay been included?";
});
{
  commands = self.callPackage ./commands.nix {};
  render   = self.callPackage ./render.nix   { inherit self; };
  relTo    = self.callPackage ./relTo.nix    {};

  renderAll = dir:
    with rec {
      mdToHtml = n: if hasSuffix ".md" n
                       then "${removeSuffix ".md" n}.html"
                       else n;

      renderGo = prefix: n: v: rec {
        name  = mdToHtml n;
        value = if isDerivation v || self.isPath v
                   then self.render {
                     inherit name;
                     file        = v;
                     SOURCE_PATH = concatStringsSep "/" (prefix ++ [n]);
                     TO_ROOT     = concatStringsSep "/" (["."] ++ map (_: "..")
                                                                      prefix);
                   }
                   else if isAttrs v
                           then go (prefix ++ [n]) v
                           else abort "Can't render ${toJSON { inherit n v; }}";
      };

      go = prefix: mapAttrs' (renderGo prefix);
    };
    go [ dir ] (self.dirsToAttrs (../.. + "/${dir}"));

  # Attrsets of rendered sub-directory pages
  blog       = self.renderAll "blog";
  unfinished = self.renderAll "unfinished";
  projects   = self.renderAll "projects" // { repos = self.projectRepos; };

  # Derivations for whole sub-directories
  blogPages       = self.attrsToDirs' "blog"       self.blog;
  projectPages    = self.attrsToDirs' "projects"   self.projects;
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
    "projects.md"   = { vars = { projects = self.projectPages;   }; };
    "unfinished.md" = { vars = { inherit (self) unfinishedPages; }; };
  };

  resources = with rec {
    atom = self.runCommand "blog.atom"
      {
        blog        = self.topLevel."blog.html";
        buildInputs = [ self.hfeed2atom ];
      }
      ''
        ATOM=$("${../mkAtom}" < "$blog")

        if [[ "x$ATOM" = "xNone" ]]
        then
          echo "Failed to produce blog.atom. Output: $ATOM" 1>&2
          exit 1
        fi

        echo "$ATOM" > "$out"
      '';

    rss  = self.runCommand "blog.rss"
      {
        inherit atom;
        buildInputs = [ self.commands.mkRss ];
      }
      ''
        mkRss < "$atom" > "$out"
      '';
  };
  {
    "blog.atom" = atom;
    "blog.rss"  = rss;
    css         = ../../css;
    js          = ../../js;
  };

  redirects =
    with self.callPackage ./redirect.nix {};
    oldLinks // {
      inherit essays;
      data_custom = {
        "prelude.txt" = mkRedirectTo {
          from = "prelude.txt";
          to   = "/git/php-prelude";
        };
      };

      "essays.html" = self.relTo "." (mkRedirectTo {
        from = "essays.html";
        to   = "projects.html";
      });
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
    projectRepos repoName repoPages;
}
