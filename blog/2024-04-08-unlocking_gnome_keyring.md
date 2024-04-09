---
title: Unlocking the GNOME keyring
---

The [GNOME keyring](https://wiki.gnome.org/Projects/GnomeKeyring) is a service
which runs in the background when logged in to a [GNOME](https://www.gnome.org)
desktop session (or, in my case, [Phosh](https://phosh.mobi) on my
[PinePhone](https://pine64.org/devices/pinephone); many of
[its APIs](https://specifications.freedesktop.org/secret-service/latest) are
also compatible with [KWallet](https://en.wikipedia.org/wiki/KWallet) too). Its
job is to store (collections of) secrets, like passwords:

 - Each collection can be encrypted with a password, so their cleartext is not
   stored on disk.
 - Collections can be "locked", preventing any reading or writing of their
   contents. Every collection starts out locked.
 - Collections can be "unlocked". If they're encrypted with a password, this is
   required for unlocking:
    - If a user has just logged in by typing their user account's password, that
      can be used to unlock an associated "login" collection. This doesn't work
      when logging in automatically (i.e. without having to type a password).
    - If the password is available in another collection that's currently
      unlocked, that value can be used.
    - In a graphical session, the user can be prompted for the password.
    - Passwords can also be piped into the `gnome-keyring-daemon --unlock`
      command, e.g. by a script; but this starts a new instance of the daemon 😞
 - Applications can interact with the keyring over
   [DBus](https://en.wikipedia.org/wiki/D-Bus), for locking/unlocking, as well
   as CRUD actions on the secrets.
 - Special support is provided for [GPG](https://www.gnupg.org) and
   [SSH](https://en.wikipedia.org/wiki/Secure_Shell) keys, plus
   [their](https://www.gnupg.org/documentation/manuals/gnupg/Invoking-GPG_002dAGENT.html)
   ["agents"](https://en.wikipedia.org/wiki/Ssh-agent).

The current setup on my PinePhone was rather akward, having evolved and
congealed over the past few years, resulting in the following:

 - My SSH key has a passphrase, making it convenient to use an SSH agent
 - I was using GNOME Keyring to provide that SSH agent
 - I'm using [RClone](https://rclone.org) to mount some
   [SFTP](https://en.wikipedia.org/wiki/SSH_File_Transfer_Protocol) shares
 - Those shares are auto-mounted using a [SystemD](https://systemd.io)
   [user service](https://wiki.archlinux.org/title/systemd/User)
 - That service starts and stops depending whether we're connected to my LAN
 - My user account is automatically logged into a Phosh session, without any
   password being entered

This combination had a rather unfortunate problem: my GNOME keyring must be
unlocked before that SystemD service can access those SFTP shares. This causes
a password prompt to appear as soon as my phone starts up, and the service
itself may retry several times before I've managed to type it in correctly.

## GCR ##

Recently, the SSH agent functionality of GNOME keyring has been
[removed](https://joshtronic.com/2024/03/10/gnome-keyring-disables-ssh-agent).
AFAIK this was due to it essentially re-implementing the functionality of the
standard `ssh-agent` command, but doing so in a way that was less flexible and
configurable than the latter. That's now gone, in favour of GCR which is just a
simple [wrapper around
`ssh-agent`](https://bugzilla.gnome.org/show_bug.cgi?id=775981), allowing its
usual configuration, etc. to be used.

Unfortunately, this switch also happened to change the default location used by
the agent's socket, from `$XDG_RUNTIME_DIR/keyring/ssh` to
`$XDG_RUNTIME_DIR/gcr/ssh`. This required me to update my SystemD service, so I
thought it was about time I improved the overall process a little.

## A command to prompt unlocking ##

Since unlocking requires user interaction (to enter the password) I think it's
reasonable to *also* require the user to initiate the process; rather than being
hit with a prompt when logged in, or after connecting to my home WiFi, etc. My
idea is to have a script, which can be bound to a `.desktop` entry, hotkey, etc.
which does the following:

 - Ask GNOME keyring to unlock the collection with my SSH key's passphrase
 - Cause a GUI prompt for the password (if needed)
 - Set a SystemD `.target`, which can trigger other services like my SFTP mounts

It turns out to be
[surprisingly hard](https://unix.stackexchange.com/questions/480074/how-to-force-gnome-keyring-daemon-to-ask-for-my-passphrase)
to simply get GNOME keyring to unlock!  Commands like `secret-tool` are designed
to look up secrets with a specified name, which I don't want to do. I eventually
tried the `dbus-send` command, to invoke the collection's `Unlock` method
directly: that returns a `Prompt` object, with methods to either show or dismiss
it. Unfortunately, that `Promp` seems to only exist so long as our DBus
connection remains open: since `dbus-send` sends a single message then
disconnects, we never get a chance to tell a `Prompt` to show!

The solution is to use a "proper" programming language, with a DBus library we
can use to keep a single connection open for several messages. Python's
[`secretstorage` library](https://pypi.org/project/SecretStorage) is
particularly simple, and available in
[Nixpkg's](https://github.com/NixOS/nixpkgs) `python3Packages` attrset. Here's
the script I'm now using:

```python
#!/usr/bin/env python3
import secretstorage
connection = secretstorage.dbus_init()

# This should be login keyring, it should have the same password as our user,
# but it will still be locked if we were automatically logged in to our user
# session (since we didn't type in the password in that case).
default = secretstorage.get_default_collection(connection)

# All keyrings (useful if we put all of our credentials in a different keyring)
keyrings = secretstorage.collection.get_all_collections(connection)

# (Try to) unlock them all, starting with default since that can be used to
# store passwords of other keyrings!
for k in ([default] + [kr for kr in keyrings]):
    if k.is_locked():
        # Waits for password to be entered, or dialogue to be dismissed
        k.unlock()
```

This script successfully unlocks the collections in GNOME keyring (which can be
validated by using [Seahorse](https://wiki.gnome.org/Apps/Seahorse) to
lock/unlock them manually for testing). Since `.unlock()` is synchronous, this
loop allows multiple collections to be unlocked from a single password prompt,
as long as the default collection contains passwords for the others (this was
useful to me since I had two keyrings, as a historical accident).

My next task will be to set up the SystemD services appropriately, which is easy
enough using [Home Manager](https://nix-community.github.io/home-manager).
