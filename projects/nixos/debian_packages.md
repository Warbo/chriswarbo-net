---
title: Using Debian Packages on NixOS
---

## Intro and Motivation ##

The NixOS Linux distribution takes its packages from the nixpkgs project: this
packages up a whole bunch of software for installation, but more than that it
provides a flexible architecture for defining our own packages, overriding the
defaults (e.g. applying patches, using different dependencies, etc.). This is
great, and pretty extensive, but it doesn't include *everything* we might want.

Most of the time, if something we want isn't available in nixpkgs then it's
pretty easy to make a package: if it's a Haskell program we might use
`cabal2nix`, if it's Python we might use `buildPythonPackage`, if it's C we
might use `stdenv.mkDerivation`, and so on.

Yet there are some occasions where this isn't the best route to take. In my
case, I wanted a particular version of the Chromium browser, and whilst Chromium
*is* available in nixpkgs, my system's customisations caused it to require
compilation (Nix fetches pre-built packages from a 'binary cache', but there
obviously won't be a pre-built version available which has my personal
customisations). Compiling Chromium is notoriously resource-intensive, so I
wanted to avoid this if possible; I could fetch an unmodified version of the
package, but I wanted to try a different approach and use the version from
Debian instead.

## The Idea ##

We're going to make a Nix package which contains a small Debian installation,
and run our application (Chromium) via `chroot`. This is like a "poor man's
container", but we'll be using Nix to manage all of the files and scripts rather
than something like Docker.

Since we want all of this to be usable by an unprivileged user account, we'll
also be using `proot` rather than `chroot`, since `chroot` requires root
privileges (in contrast, `proot` works by intercepting system calls).

## The Setup ##

There are two approaches we can take to getting Debian set up: one is to use
`debootstrap`, which will download and install the required packages into a
directory. The other is to use a pre-built version (basically an archive of the
directory made by `debootstrap`). Since we're aiming to use pre-built binaries,
I opted to also use a pre-built filesystem image.

Here's a Nix expression for getting a Debian filesystem image, which we take
from a collection of Docker resources:

```
{ fetchurl }:
rec {
  rootVersion = "67a0101a76eed558d4b61a484a27c9f9d7a119f4/stretch";

  rootRepo = "debuerreotype/docker-debian-artifacts";

  rootfs = fetchurl {
    url    = "https://github.com/${rootRepo}/raw/${rootVersion}/rootfs.tar.xz";
    sha256 = "1ff2qjvfj6fbwwj17wgmn3a4mlka1xv1p3jyj465dbf4qf3x0ijm";
  };
}
```

If you're following along at home, you can save the above to a file like
`debian-rootfs.nix` and play around with it, e.g. in `nix-repl`, like this:

```
nix-repl> with import <nixpkgs> {}; callPackage ./debian-rootfs.nix {}
```

OK, now that we have a Debian filesystem image, we'll need to unpack it and add
in the changes we need (e.g. installing Chromium). I'll do this in one go, to
prevent cluttering up our Nix store with intermediate results. The approach we
take is to write all of our customisations in a shell script, then run that
shell script within the Debian environment using `proot`. Note that Nix packages
get built by unprivileged users (usually called something like `nixbld`), and
without access to an interactive terminal (since that would be impure), which is
why we use `proot` rather than `chroot`:

```
{ cacert, callPackage, proot, runCommand, writeScript }:
with rec {
  inherit (callPackage ./debian-rootfs.nix {}) rootfs;

  # See https://github.com/proot-me/PRoot/issues/106
  PROOT_NO_SECCOMP = "1";
};
runCommand "debian-with-chromium"
  {
    inherit rootfs PROOT_NO_SECCOMP;
    buildInputs      = [ proot ];
    SSL_CERT_FILE    = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    script           = writeScript "setup.sh" ''
      #!/usr/bin/env bash
      set -e
      apt-get update
      apt-get install -y chromium
      chmod 4755 /usr/lib/chromium/chrome-sandbox
    '';
  }
  ''
    echo "Unpacking Debian" 1>&2
    mkdir "$out"
    pushd "$out"
      tar xf "$rootfs"
    popd

    echo "Installing setup script" 1>&2
    cp "$script" "$out/setup.sh"

    echo "Pointing PATH to Debian binaries" 1>&2
    export PATH="/bin:/usr/bin:/sbin:/usr/sbin:$PATH"

    echo "Resetting /tmp variables" 1>&2
    export TMPDIR=/tmp
    export TEMPDIR=/tmp
    export TMP=/tmp
    export TEMP=/tmp

    echo "Setting up" 1>&2
    proot -r "$out" -b /proc -b /dev -0 /setup.sh
  ''
```

If we save this to a file like `debian-with-chromium.nix` (in the same directory
as `debian-rootfs.nix`, or else adjust the path given to `callPackage`), then we
can build the package with a command like:

```
nix-build --show-trace debian-with-chromium.nix
```

So far so good, but this doesn't actually let us *run* Chromium.

## Using the Debian Environment ##

To run a command in this Debian environment, we'll again use `proot`. This time
we *could* use `sudo chroot`, if our user is privileged, but I think that's
overly restrictive (what about non-privileged accounts?), too much hassle
(typing in passwords), and has a larger scope to go wrong (`sudo` gives full
system access).

I'll make use of the `wrap` helper function defined in
[my nix config](/git/nix-config). Again, save this to a
file like `debian-chromium.nix`, in the same directory as
`debian-with-chromium.nix` (or adjust paths appropriately):

```
{ bash, callPackage, proot, wrap }
wrap {
  name   = "chromium-exec";
  paths  = [ bash proot ];
  vars   = {
    env = callPackage ./debian-with-chromium.nix {};

    # See https://github.com/proot-me/PRoot/issues/106
    PROOT_NO_SECCOMP = "1";
  };
  script = ''
    #!/usr/bin/env bash
    export PATH="/bin:/usr/bin:/sbin:/usr/sbin:$PATH"
    export TMPDIR=/tmp
    export TEMPDIR=/tmp
    export TMP=/tmp
    export TEMP=/tmp

    # shellcheck disable=SC2154
    proot -r "$env" -b /proc -b /dev -b /nix -b /tmp -b /home "$@"
  '';
}
```

Now we can launch Chromium. Since `wrap` outputs a fully self-contained script,
we can just call that from our usual shell:

```
$(nix-build --show-trace \
            -E '(import <nixpkgs> {}).callPackage ./debian-chromium.nix {}')
```

If you want to add the resulting script to your `PATH` then you could use
`mkBin` instead of `wrap`. The result can be installed just like any other Nix
package, either system-wide (if we have permission), or just to our user
profile, or just ad-hoc via nix-shell.

Happy hacking :)
