---
title: Switching Browser
---

I've been a big fan of the [Conkeror](http://conkeror.org/) Web browser for many
years, using it as my "main" browser (opening occasional sites in Firefox,
Chromium, Dillo, w3m, etc. if the mood takes me); however, this has become less
tenable recently, so I've finally decided to bite the bullet and switch
away. Here I'll document my reasoning and the alternative setups I've tried.

## Conkeror ##

Conkeror is an alternative GUI for Firefox, replacing the "chrome" (menu bars,
etc.) with a minimalist, commandline-based, keyboard-driven UI, mostly
following Emacs conventions. It also has nice features like Javascript
extensibility, "web jumps" (commandline hooks into search engines, with
suggestions) and a nice keyboard-driven link/image following/copying UI.

Conkeror was originally written using XUL, but that was dropped by Mozilla as a
'standalone' toolkit, so Conkeror changed to use the Firefox binary itself. More
recently, as part of their "Project Quantum", Mozilla have dropped support for
many old APIs that Conkeror relied on (I've got nothing against Mozilla for
doing this BTW; Conkeror is just one of those niche projects that suffered as a
result). For a while I stuck to the last supported version of Firefox (52),
which works but is a *very* bad idea when it comes to security (I'm happy to use
unmaintained software locally, but browsers are far too exposed to remote
attackers for this to be tenable). I knew I needed to switch to something else,
but would like as little disruption to my workflows and muscle-memory as
possible.

## Alternative Browsers ##

I want my "main" browser to support a decent amount of "modern" Web features:
especially Javascript, but possibly also video (or else have good integration
with a proper external player). Unfortunately, the spectrum of browser features
is pretty polarised: the "big" engines (Firefox/Gecko and WebKit/Blink) support
basically everything (including crap I'd rather avoid, like DRM and microphone
access (my laptop thankfully doesn't have a camera)); all other browsers (Dillo,
Netsurf, Konqueror/KHTML) support effectively no Javascript in my experience.

Hence I'm best off using one of the "big" browsers: Firefox, Chromium, Midori,
etc. As you might be able to tell from my FOSS zealotry, I consider the ethics
of software to be far more important than any particular functionality, so my
choice is mostly going to be ideological. I find Chromium's ties to Google
distasteful; this extends a little to WebKit browsers by association, but am
otherwise mostly neutral about them. I consider Mozilla and Firefox to be
(overall) a *positive* influence on the world, so I'd rather stick with them. I
also use Firefox semi-frequently, and appreciate things like NoScript and
uBlockOrigin; it also uses GTK properly, since it handles dark themes without
issue, which Chromium seems to fail at.

## Staying Up To Date ##

The only reason I'm ditching Conkeror is that I want to use a maintained,
up-to-date browser for security reasons; however, packages from official Linux
distro repositories can often be out of date. Also, I use NixOS, which tends to
build from source with a cache of pre-built binaries; those caches have been
dropped for i686 users recently, and I'd rather avoid repeatedly building
something as hefty as Firefox from source.

To solve both of these issues I've created a Nix package out of the latest
binary release of Firefox from Mozilla, which I've written below. Some notes:

 - NixOS avoids paths like `/usr/lib`, which may be hard-coded into binaries
   like Mozilla's Firefox builds. The work around this we use the
   `buildFHSUserEnv` function to generate a wrapper which starts the program in
   a chroot environment, where such paths are available.
 - We would like to know when there's a new version out. We do this by defining
   a "package" (`latest`) whose builder fetches the Mozilla "releases" page and
   extracts the latest version. If this is higher than the `version` we've
   specified, a warning is written to stderr during evaluation.
 - Nix caches packages, which will cause old versions of `latest` to be used. To
   avoid this we invalidate the cache by including the `currentTime` as a
   dependency.
 - Newer Nix versions try to sandbox their builders, to improve reproducibility.
   Since `latest` needs Internet access (and is, by design, not reproducible),
   we need to specify `__noChroot`.
 - We force the `latest` check to run by making it a dependency of our Firefox
   package; but it's not strictly needed and might fail (e.g. if we're offline).
   To avoid such problems, we allow it to be skipped by setting an env var.
 - Firefox has a lot of runtime dependencies, which need to be made available in
   the chroot. I got it working by copy/pasting the dependencies of the normal
   Firefox package.

```
with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  # Update these as needed
  version = "70.0.1";
  sha256  = "06xzb22qa2l04nw6992y648isnhdscpx3v75ba2vqw2b07ghfcam";

  # Fetches latest version number from mozilla.org
  latest = import (runCommand "latest-firefox-version.nix"
    {
      __noChroot    = true;
      buildInputs   = [ wget ];
      cacheBuster   = toString currentTime;
      url           = https://www.mozilla.org/en-US/firefox/releases;
      SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    }
    ''
      wget -O- "$url" | grep -o 'data-latest-firefox="[^"]*"' |
                        grep -o '".*"' > "$out"
    '');

  # Warn if version isn't the latest. onlineCheck comes from my Nix config; an
  # env var lets us skip these checks (e.g. if we're offline)
  warn = if onlineCheck && compareVersions version latest == -1
            then trace (toJSON {
                   inherit latest version;
                   WARNING = "Newer Firefox is out";
                 })
            else (x: x);

  url = warn concatStrings [
    "https://archive.mozilla.org/pub/firefox/releases/"
    version
    "/linux-i686/en-GB/firefox-"
    version
    ".tar.bz2"
  ];

  # unpack comes from my Nix config; it extracts the output of a derivation.
  contents = unpack (fetchurl { inherit sha256 url; });

  # This script will run in the chroot, launching Firefox with needed env vars.
  # mkBin comes from my Nix config: it writes 'script' to a file in bin/ and
  # uses 'makeWrapper' to add 'paths' to PATH and 'vars' to the environment,
  raw = mkBin {
    name   = "firefoxWrapper";
    paths  = [ bash fail ];
    vars   = {
      inherit contents;

      # Avoid "Locale not supported by C library"
      LANG   = "C";
      LOCALE = "C";

      # Avoid Fontconfig error: Cannot load default config file
      FONTCONFIG_FILE = makeFontsConf {
        fontDirectories = [];
      };
    };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Make gsettings schemas available, to avoid file dialogues crashing
      function addSchemas {
        FOUND=0
        for D in "$1"*
        do
          FOUND=1
          export XDG_DATA_DIRS="$D:$XDG_DATA_DIRS"
        done
        [[ "$FOUND" -eq 1 ]] || fail "No schemas found for '$1'"
      }
      addSchemas "${gsettings-desktop-schemas.out}/share/gsettings-schemas/gsettings-desktop-schemas-"
      addSchemas "${gtk3.out}/share/gsettings-schemas/gtk"

      # Make GTK icons, etc. available
      export XDG_DATA_DIRS="${concatStringsSep ":" [
        "${gnome3.adwaita-icon-theme}/share"
        "${gnome2.gnome_icon_theme}/share"
        "${hicolor-icon-theme}/share"
        "$XDG_DATA_DIRS"
      ]}"

      exec "$contents/firefox" "$@"
    '';
  };
};
buildFHSUserEnv {
  name       = "firefox";
  targetPkgs = pkgs: [ raw ] ++ (with pkgs; with xorg; [
    # These are copypasta from the nixpkgs firefox dependencies
    alsaLib
    atk
    cairo
    cups
    dbus-glib
    dbus
    fontconfig
    freetype
    gdk_pixbuf
    glib
    glibc
    gtk2
    gtk3
    kerberos
    libX11
    libXScrnSaver
    libXcomposite
    libXcursor
    libxcb
    libXdamage
    libXext
    libXfixes
    libXi
    libXinerama
    libXrender
    libXt
    libcanberra-gtk2
    libnotify
    libGLU_combined
    nspr
    nss
    pango
    libheimdal
    libpulseaudio
    (lib.getDev libpulseaudio)
    ffmpeg

    # Required for glib schemas; without this, file dialogue boxen will crash
    gnome3.dconf

    # Avoid 'The 'hicolor' theme was not found'
    hicolor-icon-theme
    gnome2.gnome_icon_theme
    gnome3.adwaita-icon-theme
  ]);
  runScript = "firefoxWrapper";  # Corresponds to the 'name' of 'raw'
}
```

## Recreating My Conkeror Workflow ##

I'm not [*too* attached to my current habits](https://xkcd.com/1172); but
likewise I see no reason to adopt the "default" way of doing things if there's a
not-too-hacky alternative that suits me better.

### Keyboard Navigation ###

Conkeror's nice link-following method is to press a key (e.g. `f` to follow a
link, `c` to copy a link's URL, `i` to select an image, etc.) which switches to
a new mode, where each link/frame/image/etc. is labelled with a number. Typing
in that number chooses that object.

An Firefox extension which provides similar functionality (although using letter
sequences rather than numbers) is [Saka key](
https://addons.mozilla.org/en-GB/firefox/addon/saka-key/).

This extension also makes it easy to associate custom keybindings to various
browser actions, e.g. `[` and `]` to switch tabs instead of the clunkier
`Ctrl-Tab`, etc. This has allowed me to recreate some of the bindings that are
in my muscle-memory, like `q` to close the current tab. The preferences UI
handily points out when bindings conflict; this is usually easy to resolve,
since there's lots of functionality I have no problems disabling.

One annoyance to note is that some Web pages will hijack key presses; sometimes
taking complete control of the keyboard and suppressing key press events that
we'd like the browser to receive. I don't know of a general solution to this: it
also happens in Conkeror, where `Alt-x` can *always* bring up a commandline
which we can use to close the offending tab; I think my best bet in Firefox is
using NoScript to turn off such page's Javascript.

## UI Chrome ##

I don't want Firefox's toolbars, location bar, etc. cluttering up the screen all
the time (even my Xmonad "window decorations" are only 1 pixel wide!). In more
recent versions the menu bar will auto-hide (press `Alt` to show it), but I'd
also like to hide the rest.

Thankfully this can be done by styling Firefox's "chrome" with CSS, via the
`chrome/userChrome.css` file in Firefox's profile directory. Doing a little
digging (on [StackOverflow](https://superuser.com/a/1269912/297735),
[Mozilla's support site](https://support.mozilla.org/en-US/questions/1199205)
and [Reddit](https://www.reddit.com/r/FirefoxCSS)), I came up with the following
`userChrome.css` code which:

 - Hides the tab bar completely (I'm happy to cycle through tabs individually).
 - Hides the location bar *unless* anything inside it is focused (e.g. the
   address entry box, which can be focused with `Ctrl-L`).

``` css
/* Always hide the tab bar */
#TabsToolbar { visibility: collapse; }

/* Hide address bar and surrounding buttons unless anything inside is focused */
#nav-bar:not(:focus-within), #nav-bar:not(:focus-within) * {
        border : none !important;
        margin : 0    !important;
    max-height : 0    !important;
    min-height : 0    !important;
       padding : 0    !important;
}
```

Using `visibility: collapse;` or `display: none;` on the `#nav-bar` seems to
permanently hide it, hence the use of `border`, `margin`, etc.

The nice thing about using `not(:focus-within)` on the whole `#nav-bar`, rather
than just the focus of the address bar, is that things will remain visible when
using context menus (e.g. from the built-in "burger menu", or from extensions
like NoScript), even though they take focus away from the address bar.

## Bookmarks ##

One annoyance with Conkeror is that it has no bookmark manager. It stores
bookmarks in the same way as Firefox (in an SQLite database called `places`),
but provides no UI to manage them. I ended up using `sqlitebrowser` to do so.

Firefox, of course, has a very nice bookmark manager. I copied each one across
manually, from SQLiteBrowser to Firefox's UI.

## Passwords ##

I've been using Firefox in "permanent private browsing" mode, which is better
from a security and tracking perspective, but inconvenient for sites that
require logging in.

As far as I see it, there are three setups I could use:

 - Log in (manually or automatically), and allow persistent cookies to keep me
   logged in across browser invocations, reboots, etc.
 - Don't allow persistent cookies, but save passwords to make logging in more
   convenient.
 - Don't allow persistent cookies and don't save passwords. Log in manually.

I was using the first with Conkeror and the last with Firefox. Now that I'm
using Firefox full-time, I'd like to keep the benefits of browsing without
permanent cookies, but make life more convenient by opting for the middle setup.

I already used a password manager, KeePassX, which means that:

 - I don't know most of my passwords, since they're auto-generated gibberish
 - My computer *does* already know my passwords

KeePassX is actually unmaintained (it's a local, offline program; so that's OK).
I was happy enough with it, but it turns out that the fork
[KeePassXC](https://keepassxc.org) ("C" for "Community") is not only maintained,
but it also provides a [Firefox plugin](
https://addons.mozilla.org/en-US/firefox/addon/keepassxc-browser) too!

Getting this to work was a little tricky, since the documentation only covers
the happy path ("click this and it should work!"). The error messages were
rather uninformative too, and stepping through the addon's Javascript with
Firefox's debugger wasn't particularly enlightening. Someone in IRC
(`#keepassxc` on Freenode) suggested I use the latest release of KeepassXC
(2.4.3) since the browser plugin might not work with older versions.

This, in turn, turned out to be rather tricky. There's currently a bug in
Nixpkgs for 32bit x86 Linux which (AFAIK) causes some packages to be built with
the wrong compiler (that used to "bootstrap" the system). This results in
runtime crashes about unresolved names. Long story short: the `qtbase` package
is broken from Nixpkgs 18.03 onwards.

I've been working around this by pinning Qt packages from older versions of
Nixpkgs, e.g. Picard and Basket. Yet I need the newer KeePassXC packages.
Thankfully Nixpkgs makes it easy to override dependencies, so I tried the
following override in my Nixpkgs overlay:

```
keepassx-community = trace
  "FIXME: Overriding dependencies of keepassx-community to avoid broken Qt"
  super.keepassx-community.override (old: {
    inherit (self.nixpkgs1709)
      cmake
      curl
      glibcLocales
      libargon2
      libgcrypt
      libgpgerror
      libmicrohttpd
      libsodium
      libyubikey
      stdenv
      yubikey-personalization
      zlib;
    inherit (self.nixpkgs1709.qt5)
      qtbase
      qttools
      qtx11extras;
    inherit (self.nixpkgs1709.xorg)
      libXi
      libXtst;
  });
```

Note that `self` is the whole Nixpkgs set (including my overrides) and
`nixpkgs1709` is my pinned copy of Nixpkgs 17.09 packages. I've added the
`FIXME` as a reminder to myself, so if Qt ever starts working again upstream I
can remove this override.

Unfortunately that wasn't enough, since the version of KeePassXC in Nixpkgs
19.03 isn't 2.4.3, it's 2.3.4. So I also ended up overriding this override
(yikes!) to fiddle with its parameters. Since then version 2.5.0 has come out,
which I'm using via this package (commented in-line):

In particular:

 - Replacing `src` with the 2.4.3 tarball.
 - Adding `qtsvg` and `qrencode` to the dependencies, since they seem to be new
   requirements.
 - Adding `pkgconfig` to the dependencies, so that CMake can find `qrencode`.
 - Removing existing patches. One is for Mac, which I don't need; the other is
   a oneliner which fails to apply, since that change has now been made
   upstream.
 - Copying `checkPhase` from the Nixpkgs master branch, since it sets some env
   vars that the tests will fail without (specifically, `testcli` fails to find
   the `xcb` plugin for Qt)

```
keepassx-community =
  with rec {
    # Update this as needed
    version = "2.5.0";
    src     = self.unpack (self.fetchurl {
      url    = "https://github.com/keepassxreboot/keepassxc/releases/" +
               "download/${version}/keepassxc-${version}-src.tar.xz";
      sha256 = "10bq2934xqpjpr99wbjg2vwmi73fcq0419cb3v78n2kj5fbwwnb3";
    });

    # Checks for the latest version number
    latest = import (self.runCommand "latest-keepassxc"
      {
        __noChroot  = true;
        buildInputs = [ self.utillinux self.wget self.xidel ];
        nix         = self.writeScript "read-version.nix" ''
          with builtins;
          readFile ./version.txt
        '';
        pat = "//a[contains(text(),'Latest release')]/../..//a/@href";
        url = https://github.com/keepassxreboot/keepassxc/releases/latest;
      }
      ''
        mkdir "$out"
        wget -q --no-check-certificate -O- "$url" |
          xidel - -s -e "$pat"                    |
          grep tag                                |
          rev                                     |
          cut -d / -f1                            |
          rev                                     |
          sed -e 's/^/"/g' -e 's/$/"/g' > "$out/default.nix"
      '');

    # Use known-good dependencies, to avoid broken Qt, etc.
    fixedDeps = super.keepassx-community.override (old: {
      inherit (self.nixpkgs1709)
        cmake
        curl
        glibcLocales
        libargon2
        libgcrypt
        libgpgerror
        libmicrohttpd
        libsodium
        libyubikey
        stdenv
        yubikey-personalization
        zlib;
      inherit (self.nixpkgs1709.qt5)
        qtbase
        qttools
        qtx11extras;
      inherit (self.nixpkgs1709.xorg)
        libXi
        libXtst;
    });

    # Use newer version, for fixed and better browser integration
    updated = fixedDeps.overrideAttrs (old: rec {
      # Use the new version
      inherit src version;
      name        = "keepassxc-${version}";
      buildInputs = old.buildInputs ++ [
        self.nixpkgs1709.pkgconfig                # Needed to find qrencode
        self.qt5.qtsvg self.nixpkgs1709.qrencode  # New dependencies
      ];

      # Taken from newer Nixpkgs, avoids missing plugin error when testing
      checkPhase = ''
        export LC_ALL="en_US.UTF-8"
        export QT_QPA_PLATFORM=offscreen
        export QT_PLUGIN_PATH="${with self.nixpkgs1709.qt5.qtbase;
                                 "${bin}/${qtPluginPrefix}"}"
        make test ARGS+="-E testgui --output-on-failure"
      '';

      # Ignore the 2 patches in Nixpkgs (one Mac-only, the other upstreamed)
      patches = [];
    });
  };
  # Remind ourselves of this override, so we can remove it when no longer needed
  trace "FIXME: Overriding deps of keepassx-community to avoid broken Qt"
        # Emit a warning if there's a new version out; allow skipping the check
        (if self.onlineCheck && (compareVersions version latest != 0)
            then trace (toJSON {
                   inherit latest version;
                   warning = "KeePassXC version doesn't match latest";
                 })
            else (x: x))
        updated;
```

This gives me the latest KeePassXC (which I might as well use), but it still
wasn't enough to fix the browser integration! I finally figured out the problem
was that the `keepassxc-proxy` binary wasn't running properly from the chroot
environment that I'm running Firefox from.

To remedy this I wrote my own replacement:

```python
#!/usr/bin/env python
import json
import socket
import sys
import threading
import time

# Enable this for debugging
logFile = open('/home/chris/DELETEME/keepassxc-proxy-alternative.log', 'a')
log = lambda s: None #(logFile.write(s), logFile.flush(), None)[-1]

# Talk to KeePassXC via a socket
sock           = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
server_address = '/run/user/1000/kpxc_server'
log('connecting to %s\n' % server_address)
try:
    sock.connect(server_address)
except socket.error, msg:
    log(msg)
    sys.exit(1)

def tryParse(get, so_far=""):
    """Incrementally read characters using the given 'get' function, appending
    them to the 'so_far' string. Ignore anything before the first '{', then
    parse and return one complete JSON object."""
    try:
        # Try parsing JSON from the string we've read so far
        val = json.loads(so_far)
        return val
    except:
        # If it fails, read another character
        c = ''
        while c == '':
            c = get()

        # Ignore crap at the start that's not JSON
        if so_far == "" and c != "{":
            log("Skipping char '%s'\n" % c)
            return tryParse(get, so_far)
        return tryParse(get, so_far + c)

# Communication with the KeePassXC-Browser plugin happens via stdio. Each
# message is prefixed with its length (which we handle via the 'struct' module).
import struct
def send_to_browser(message):
    sys.stdout.write(struct.pack('I', len(message)))
    sys.stdout.write(message)
    sys.stdout.flush()

def read_from_browser():
    text_length_bytes = sys.stdin.read(4)

    if len(text_length_bytes) == 0:
        sys.exit(0)

    text_length = struct.unpack('i', text_length_bytes)[0]
    text        = sys.stdin.read(text_length).decode('utf-8')
    return text

# We have two main loops: one reads from the browser and sends to the socket
def browserToSocketMain():
    try:
        while True:
            message = read_from_browser()
            log('sending "%s"\n' % message)
            sock.sendall(message)
    except Exception as e:
        log('Got exception: ' + repr(e))
    finally:
        log('closing socket')
        sock.close()
        sys.exit(0)

browserToSocketThread = threading.Thread(target=browserToSocketMain,
                                         args=())
browserToSocketThread.daemon = True
browserToSocketThread.start()

# The second main loop reads from the socket and sends to the browser
try:
    while True:
        response = tryParse(lambda: sock.recv(1))
        dumped   = json.dumps(response)
        log('Received: %s\n' % dumped)
        send_to_browser(dumped)
except Exception as e:
    log('Got exception: ' + repr(e))
finally:
    log('closing socket')
    sock.close()
    sys.exit(0)
```

Pointing the `path` entry of
`~/.mozilla/native-messaging-hosts/org.keepassxc.keepassxc_browser.json` to this
program was enough to get the plugin talking to KeePassXC, and filling in
password forms for me.

Now I can stay in private browsing mode, yet log in to sites as needed with a
simple keyboard shortcut.
