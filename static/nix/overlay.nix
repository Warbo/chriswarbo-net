{ allDrvsIn, attrsToDirs', stripOverrides, extras, merge, newScope, withDeps' }:

with rec {
  call = newScope (extras // components);

  components = rec {
    # Load our components
    blog = call ./blog.nix { };
    commands = call ./commands.nix { };
    projects = call ./projects.nix { };
    redirect = call ./redirect.nix { };
    relTo = call ./relTo.nix { };
    render = call ./render.nix { };
    renderAll = call ./renderAll.nix { };
    repos = call ./repos.nix { };
    resources = call ./resources.nix { };
    siteTests = call ./siteTests.nix { };
    unfinished = call ./unfinished.nix { };

    # Combine all pages together into a directory
    untestedSite = attrsToDirs' "untestedSite"
      (stripOverrides (merge [ blog projects resources unfinished ]));

    # Provide all pages, with all of our tests as dependencies
    wholeSite = withDeps' "chriswarbo.net" (allDrvsIn siteTests) untestedSite;
  };
};
components
