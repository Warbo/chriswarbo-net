---
title: Developing on NixOS
---

I've now been using NixOS for over 6 months, and it's finally starting to make
sense and become second-nature to me. I didn't have any prior experience using
Nix, so it's been quite a learning curve to base my whole OS on it!

Here's a rough guide to how I've been getting along, in chronological order from
"struggling to make sense of what these blog posts and Wiki pages say", up to
"even my one-liner utility scripts are integrated into Nix now".

## `/etc/nixos/configuration.nix` ##

This was the first clear benefit. Nix packages provide various hooks into the
software they contain, which lets us alter their configuration from one place
(`/etc/nixos/configuration.nix` for system-wide things like Grub, Linux, X, etc.
and `~/.nixpkgs/config.nix` for per-user settings).

Also, since this is all integrated into Nix, new packages will be installed
automatically when our configuration happens to use them (eg. designating a
display manager for X), plus all changes are atomic and we can roll back to
previous configurations whenever we want (past system configurations are also
available via Grub, in case your new config doesn't boot).

## `nix-env`{.bash} ##

After getting to grips with `configuration.nix`, I carried on my normal approach
of installing packages as and when I needed them. I basically replaced my usual
habit of running `apt-get install foo`{.bash} with running
`nix-env -i foo`{.bash} instead.

Internally, Nix gives every user their own "profile", which is just a Nix
package. Running `nix-env -i foo`{.bash} will actually create a new version of
my profile, with `foo` added to its dependencies. For convenience, this new
profile will be installed immediately and the symlinks in `~/.nix-profile/` will
be updated accordingly, so I will be able to run `foo`{.bash} straight after
`nix-env`{.bash} has finished.

There are few benefits to `nix-env`{.bash} over `apt-get`{.bash}:

 1) Packages are installed per-user, rather than system-wide
 1) Multiple versions can be installed side-by-side
 1) I can roll back to previous profiles whenever I like

Despite these advantages, there are actually much better ways of using NixOS.

## nixpkgs ##

I spend a lot of time developing software, so I usually have a load of little
helper scripts living in `/usr/local` (these are usually symlinks to a git
repository somewhere in `~`).

Doing this on NixOS would undermine the benefits of Nix, so I looked for a way
to add them gracefully.

The Nix package repository is maintained
[on Github](https://github.com/NixOS/nixpkgs) so with a bit of effort we can get
a copy and see how the packages work.

The `-f`{.bash} option of the `nix-env`{.bash} command allows us to [specify our
own copy of nixpkgs](https://nixos.org/wiki/Create_and_debug_nix_packages)

This lets us add our own packages to a local clone of nixpkgs, without having
to push these packages upstream. Since nixpkgs is all in git, we can even
maintain our changes in separate branches if we want.

The key to using custom software in a Nix package is the package's `src`. This
specifies where the code will come from, and among other things it allows URLs,
VCS repositories and local file paths. If your code is maintained in a VCS, then
you might as well specify that. If it's stuff you'd rather not put online, then
use a file path (remember that you can use git and co. offline too!)

The problem with using a separate nixpkgs for custom code is that there is a
separation between the software itself and its Nix dependency information. Of
course, the *ability* to maintain them separately is vital, but since we're
maintaining both, it would be nice to keep them in lock-step.

Also, whilst using a different tree for a few `nix-env`{.bash} commands is
useful for resolving dependencies between multiple custom packages, Nix will not
have access to these custom definitions at any other time; unless we use our
custom nixpkgs clone as the system's default repo, which imposes a maintenance
burden.

## `nix-shell`{.bash} ##

To keep package definitions in the same source repository as our software, we
just need to put them in a `default.nix` file. If we do this, we gain a
remarkable new ability: by running `nix-shell`{.bash} from the directory
containing `default.nix`, we're dropped into a shell with all of the specified
dependencies available. When we exit the shell, they're free to be
garbage-collected.

In other words: we don't need to install the package in order to use it!

I've been making extensive use of `default.nix` files (and, if there are a bunch
of `nix-shell`{.bash}-specific things that don't belong in the regular package,
the file `shell.nix` can also be used). These ensure a clean, consistent,
reproducible development/test/deployment environment.

In particular, we can use `nix-shell --pure`{.bash} to ensure that *only* the
specified dependencies are available. For example, if your user profile has
`bash`{.bash} available, but it's not listed as a dependency in `default.nix`,
then using `nix-shell`{.bash} without `--pure`{.bash} will cause `bash`{.bash}
to be available inside the resulting shell.

This may be convenient for one-off tasks (eg. if you want to launch an editor
from inside the shell to edit the code in a particular way, but don't want to
add it as a dependency), but it's not a good idea for frequent or important
work, since that software may become an implicit dependency without you
realising it. For example, imagine if you only ever run your test suites with
`nix-shell`{.bash}, not `nix-shell --pure`{.bash}; your tests might fail without
some unspecified dependency, but you'd never know!

Instead, we should strive to always use `nix-shell --pure`{.bash}; that way, if
we try to use something which isn't listed as a dependency, we will get an error
(eg. `to install bash, run nix-env -i bash`). We can then add that dependency to
`default.nix` to ensure that it will *always* be available whenever our software
is used.

## Continuous Integration ##

Since writing this page, I've built up [a new habit](continuous_integration.html)
which is analogous to continuous integration in the world of online services.
Namely, I tie my Nix packages to their git repositories, such that the package
source is always the latest commit, and a simple `git push` can make any change
instantly available to `nix-env` or `nix-shell`.

## Conclusions ##

That's about as far as I've got with Nix/NixOS so far. If I develop some better
habits, I may update this page with more info!
