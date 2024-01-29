---
title: Nix caching on a LAN
---

I use Nix to build a lot of stuff on my main computer (a PinePhone), but it has
limited on-board storage. However, there's plenty of storage on my local network
(a RaspberryPi running mergerfs across a bunch of hard drives). Nix can be used
with little storage, but doing so requires frequent garbage-collection, which
can in turn cause a lot of re-downloading or re-building. The latter could be
mitigated if I use LAN storage as a cache: downloads would be fast Ethernet
transfers, and the cache would contain my custom builds.

## Intermittent connectivity ##

The main problem for implementing this is intermittent connectivity: my phone
won't always be connected to my LAN, my RaspberryPi may be offline, etc. There
are several bug reports and feature requests asking for Nix to skip unreachable
caches, rather than either failing (default) or building from source (if the
`fallback` option is set).

Until that's working automatically, one of the comments had an intriguing
workaround: options in `nix.conf` which take a *list* of values, can be
augmented by an "extra" set of entries. The rationale seems to be for CLI usage,
where we *sometimes* want to replace a list, and *sometimes* want to append to a
list. For example, say our `nix.conf` file sets the following:

```
substituters = http://example.com ssh://me@example.org
```

We can override this per-command, using the `--option` argument; e.g. if we know
that a bunch of the things we want are already cached on `example.net`, we could
say:

```
nix-build --option substituters 'ssh://you@example.net'
```

That command will *replace* the `substituters`: it tells Nix to use the cache
`ssh://you@example.net` and *not* to use `http://example.com` or
`ssh://me@example.org`. If we instead want to use *all* of those caches, we
can say:

```
nix-build --option extra-substituters 'ssh://you@example.net'
```

### Setting unreliable substituters ###

The trick we're going to pull is to set *both* `substituters` *and*
`extra-substituters` in our `nix.conf` file. We'll use the former for reliable
substituters, and the latter for unreliable ones, like this:

```
substituters = http://example.com ssh://reliable@example.info
extra-substituters = ssh://flaky@example.gov
```

Now consider the possible CLI options we can use:

 - Using `--option substituters foo --option extra-substituters bar` lets us
   replace all of the substituters. This might be useful, but rarely.
 - Using `--option substituters foo` will replace the reliable substituters, and
   use the unreliable ones from `nix.conf`. There's no compelling use-case for
   doing this.
 - Using `--option extra-substituters foo` will replace the unreliable
   substituters, and use the reliable ones from `nix.conf`. In particular, we
   can say `--option extra-substituters ''` to *ignore* the unreliable ones!
 - Giving no `--option` arguments (i.e. the default) will use all of the
   reliable and unreliable substituters.

It's those last two invocations that are the most useful: we can use all of our
caches by default (e.g. when I'm on my LAN), but if the unreliable ones aren't
available (e.g. I'm away from home) I can say `--option extra-substituters ''`
to skip them. The downside of this setup is that I can no longer *append*
substituters via the commandline; though I can still achieve the same result by
including the existing list in my command, which is only mildly annoying.

## Substituting over LAN ##

Now I just need to decide how I'll utilise my RaspberryPi's storage as a Nix
cache. There are many approaches, but the most important decision is whether to
use a "remote store" (treating the RaspberryPi as a machine with Nix installed,
and copying to/from its store) or a "binary cache" (treating the RaspberryPi as
a file server).

The most appropriate choice will vary depending on your circumstances, but I've
opted to use a remote store:

 - The RaspberryPi already has Nix installed so it's not adding any extra burden
 - Binary caches use archives (in "NAR" format), rather than using the existing
   store contents. That would add overhead/duplication, since it already has a
   Nix store.

### Accessing the remote store ###

Next I needed to decide how I'd access the RaspberryPi as a remote store. It's
already set up for SSH access, so I wanted Nix to access it that way too. There
are actually a few different ways I could have implemented this.

Nix supports SSH directly, by using `ssh://` or `ssh-ng://` to specify the
substituter. This is the most straightforward, but it requires the Nix *daemon*
(usually running as `root`) to have SSH access to the desired machine. If you're
doing this yourself, a good setup might be:

 - Generating a fresh key for `root`
 - Setting `root`'s SSH config to use that key when accessing the RaspberryPi
 - Creating a new user on the RaspberryPi
 - Adding the new (public) key to that user's authorised keys
 - Configuring SSH on the RaspberryPi to restrict that user to only running Nix

However, I didn't want the extra work of setting that up and maintaining it
going forward. Instead, I wanted to use my regular user's SSH key; that's
complicated by its use of a passphrase, but potentially solved by connecting to
my user's SSH agent.

I considered using `sshfs` or `rclone` to mount the RaspberryPi's Nix store in
a local directory, and telling Nix to use that; however, that may be inadvisable
when both machines are building at the same time. Also, if we try to use that
directory when its not mounted, Nix will fill it with directories and databases,
which we'd have to clean up.

Instead, I decided to set up an SSH tunnel between the RaspberryPi's Nix daemon
socket and a local socket. I've wrapped this into a SystemD service, which
starts/stops depending on whether the RaspberryPi is available. Here's the
HomeManager config for that service:

```
rpi-nix-daemon = {
  Unit = {
    Description = "Tunnel RPi's nix daemon socket to our /tmp";
    After = [ "rpi-accessible.target" ];
    PartOf = [ "rpi-accessible.target" ];
    BindsTo = [ "rpi-accessible.target" ];
    Requires = [ "rpi-accessible.target" ];
  };
  Service = with { sock = "/tmp/rpi-nix-daemon.sock"; }; {
    ExecStart = "${pkgs.writeShellScript "rpi-nix-daemon.sh" ''
      set -ex
      . ~/.bashrc
      function cleanUp {
        rm -f ${sock}
      }
      trap cleanUp EXIT
      cleanUp
      ssh \
        -L ${sock}:/nix/var/nix/daemon-socket/socket \
        -N \
        rpi
    ''}";
    ExecStop = "rm -f ${sock}";
    Restart = "on-failure";
  };
  Install = { };
};
```

Note the use of `. ~/.bashrc`{.sh}, which ensures that the required env vars are
set (including `SSH_AUTH_SOCK`). **TIP:** The default `bashrc` in some distros
starts with a command like `[ -z "$PS1" ] && return` to skip non-interactive
shells; make sure you set the required env vars *above* such a line!

This tunnel relies on the `rpi-accessible.target` to know whether the
RaspberryPi is available or not. I created that target a while ago to toggle my
network mounts, so it made sense to re-use it here. It's kept up to date by a
NetworkManager dispatcher script which runs every time the network changes. That
uses `getent ahosts` to check whether the RaspberryPi is accessible (my LAN
relies on mDNS addresses), and runs
`systemctl --user --no-block start rpi-accessible.target` (or `stop`) to set the
target's status.

With that in place, I can set the following in my `nix.conf`:

```
trusted-substituters = unix:///tmp/rpi-nix-daemon.sock
extra-substituters = unix:///tmp/rpi-nix-daemon.sock
```

We use `trusted-substituters` to tell Nix that it's always OK to fetch from this
cache.

## Populating the cache ##

Nix should now query this cache, as long as I'm on my LAN (and hence the SystemD
service is tunneling the socket). When I'm *not* on my LAN, the socket will
disappear and Nix will complain; which I can avoid by passing it
`--option extra-substituters ''`. However, this cache is currently rather
useless, since we're not writing anythign to it! To achieve this, we use Nix's
"post-build hook". Here's the script I'm using, which is adapted from that given
in the Nix manual:

```{.sh}
#!/usr/bin/env bash
set -e
set -f # disable globbing
export IFS=' '
WANT='my-username
REMOTE='ssh://remote-user@rpi.local'

if [[ "$USER" = "$WANT" ]]
then
    . ~/.bashrc
    if rpi-available > /dev/null
    then
        echo "Uploading paths $OUT_PATHS" 1>&2
        ts -S1
        TMPDIR=/tmp ts nix copy --to "$REMOTE" $DRV_PATH $OUT_PATHS
    else
        echo "RaspberryPi not available, skipping upload" 1>&2
    fi
else
    if [[ "${GIVE_UP:-0}" -eq 1 ]]
    then
        echo "Running as '$USER' instead of '$WANT', aborting" 1>&2
    else
        export GIVE_UP=1  # Avoids infinite recursion
        exec sudo GIVE_UP=1 OUT_PATHS="$OUT_PATHS" DRV_PATH="$DRV_PATH" \
             -u "$WANT" "$0" "$@"
    fi
fi
true
```

The important parts:

 - The `IFS=' '` and `set -f` lines come from the Nix manual example. They
   prevent potential issues with using the `$OUT_PATHS` variable unquoted.
 - This script is invoked by the Nix daemon, but we want it to run as our normal
   user (for its SSH setup). We achieve this by checking the `$USER` variable,
   and using `sudo -u` to re-run this script (`$0`, with args `$@`) as the
   desired user. We must specify the env vars to pass along, and I also include
   a `GIVE_UP` variable to prevent infinite recursion if the user-switching
   doesn't work as expected!
 - We use `. ~/.bashrc` to again set up important env vars like `SSH_AUTH_SOCK`.
   We also rely on this to set `PATH` (via `. /etc/profile.d/nix.sh`)
 - We'll use `nix copy` to transfer build products to the cache, but we don't
   want to run it synchronously (which would slow down our builds). Instead, we
   use the `ts` command from TaskSpooler, which adds it to a queue (with `-S1`
   setting its concurrency to 1). We set `TMPDIR` ensure TaskSpooler uses our
   user's normal queue (which we can inspect by running the `ts` command)
 - The `rpi-available` command is used to check if the RaspberryPi is
   accessible. Since TaskSpooler makes the copying asynchronous, we could just
   let it fail in that case; but I'd rather avoid "expected errors", since they
   tend to obscure unexpected problems!

## Conclusion ##

With the above setup, I can run Nix's garbage collector more aggressively on my
PinePhone to free up space, safe in the knowledge that previous build products
will be fetched from my RaspberryPi; and also not worry too much about having no
access to that cache (it would be nice for Nix to automatically ignore
connectivity failures; but passing an `--option` argument is reasonable for
now)
