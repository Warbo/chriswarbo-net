---
title: Upgrading Ubuntu's Nix
---

I've been changing my Nix repos to use git-hashing, but this requires a recent
version of Nix (at least 2.27, to (a) support `git-hashing`, (b) support the
`git:sha1` hash algorithm, and (c) avoid some bugs in the implementation).

This can lead to some difficulties if we try updating a system that's on an
older version of Nix; e.g. we can't get the newer Nix by switching to an
upgraded NixOS configuration, if that NixOS configuration is using git-hashing
somewhere! This isn't *too* bad if we make sure to define the newer Nix in a way
that can be built separately to our full system (e.g. I've done this in an
overlay); then we can update a NixOS system in two steps: first adding the newer
Nix, so we get a system that can use git-hashing; then using that to upgrade to
the git-hashing-using NixOS configuration.

One place this *won't* work, however, is when our Nix installation is not being
managed by Nix. This is the case on one of my systems: an Ubuntu VM I use for
work. In this case, I've installed the `nix-bin` and `nix-setup-systemd`
packages from the Ubuntu repos.

## `nix bundle` ##

A few years ago I used `nix bundle` to solve a similar issue: using Nix to put a
known-good JVM on to a non-Nix system. I ran `nix bundle` to create a standalone
Arx file (a tarball, made self-extracting by prefixing it with a shell script),
containing the "runtime closure" of the desired JVM commands, so that all of its
dependencies would be extracted into place before the JVM was invoked.

This time I want a similar thing, but using Dpkg (Ubuntu's native package
manager) instead of Arx. Thankfully `nix bundle` is modular, and there is
already a `toDEB` bundler which can do this, so I ran the following to make a
package containing the newer Nix:

```sh
nix bundle \
  --bundler github:NixOS/bundlers#toDEB \
  --impure \
  --expr '{ nix = (import ./myPinnedNixpkgs.nix {
              config = {};
              overlays = [ (import ./myNixOverlay.nix) ];
            }).nix-backport; }' \
  nix
```

This created a `deb-single-nix` symlink (rather than the `result` that
`nix-build` usually makes) containing our package, which can be installed like
`sudo dpkg -i deb-single-nix/nix_1.0_amd64.deb` and, surprisingly,
the resulting Nix works! (You may want to `systemctl restart nix-daemon` too)

## Why it works ##

There are a few reasons why this is enough to make our newer Nix work:

 - Firstly, our package has a different name (`nix`) than Ubuntu's (`nix-bin`),
   so the latter won't get replaced; we'll have *both* packages installed.
 - Secondly, the `/bin/nix` executable provided by our package *will* replace
   the old version installed by `nix-bin`. Since the path doesn't change, all
   existing references (in `PATH`, etc.) will see our new version.
 - Finally, the `nix-bin` package provides a symlink
   `/bin/nix-daemon -> /bin/nix`. Since that just points to `/bin/nix`, it means
   our SystemD service will *also* use our new version, despite our `nix`
   package not providing any `nix-daemon` command itself!
