---
title: Nix Packages
dependencies: [ 'static/file2img.sh' ]
packages: [ 'graphviz' ]
---

```{pipe="tee graph.sh > /dev/null"}
#!/bin/sh
dot -Tpng | ./root/static/file2img.sh | pandoc -t json
```

```{pipe="sh > /dev/null"}
chmod +x graph.sh
```

Since switching to [NixOS][nixos] [a few months ago][nixpost], I've been learning more and more about using the [Nix package manager][nix] that it's based on.

I'm still nowhere near an expert, but thought I'd document some of the things I've found out, what I misunderstood and which approaches tend to work better than others. This post is an introduction and a reproduction of a [comment I made on Hacker News][hackernews].

# Package Managers #

Nix is a "package manager". This is a familiar concept to most Linux users, who've been using them for at least 20 years. If you've not come across the term before, then think of an "App Store" without any money being involved.

There are a few advantages to using a package manager. Without a package manager, applications tend to come with copies of all of the third-party code they make use of. This wastes space, since many applications might include copies of the same thing. It can also be insecure, since vulnerabilities in these third-party components must be patched in every since copy to remove the threat. The alternative is to have the user install one copy of the third-party code, and have every application use that copy. This is painful to do by hand, and is what package managers are great for.

One problem with "traditional" package managers is that they affect the whole system. Updating one package, like Firefox, will affect all users on the system. Not only that, but if the new Firefox package depends on, say, an updated JPEG library, every other package using the JPEG library will be switched to this new version. This can cause "dependency hell", when the dependencies of two packages can't be reconciled at the same time. For example, if LibreOffice doesn't work with the new JPEG library that Firefox requires.

# The Nix Package Manager #

Nix is a package manager which runs (as far as I'm aware) on Linux, Mac OSX and Windows. There are ports to other systems being worked on too. It can be installed alongside another package manager (like those found in "traditional" Linux systems) without interfering with it.

The key to Nix is that its packages don't affect the whole system. Instead, packages live in isolated directories on a read-only filesystem (ie. their files can't be changed once installed). The installation process for each package is repeatable: installing a particular Nix package will always give you the same result, and that result cannot be interfered with afterwards.

Let's take another look at our Firefox example above. Say we're using Firefox version 10 and LibreOffice 1, and they're both using libjpeg version 1:

```{.unwrap pipe="./graph.sh"}
digraph {
  "libjpeg v1" -> {"Firefox v10" "LibreOffice v1"}
}
```

Let's say we ask Nix to update our system, and it sees that libjpeg version 2 is available, which the Firefox developers have adopted because it's faster. Since the packages are read-only, Nix *can't* alter any of libjpeg v1's files at all. Instead, libjpeg v2 must be installed *alongside* the existing v1. At this point, Firefox and LibreOffice don't notice, since they're still using libjpeg v1:

```{.unwrap pipe="./graph.sh"}
digraph {
  l1 [label="libjpeg v1"]
  l2 [label="libjpeg v2"]
  l1 -> "Firefox v10"
  l1 -> "LibreOffice v1"
}
```

Nix can now have Firefox use libjpeg version 2, but again it can't alter any of the files from Firefox v10. Instead, we must get a *separate copy* of Firefox v10, which uses libjpeg v2 as a dependency. Since they're isolated from each other, these two copies of Firefox won't interfere with each other:

```{.unwrap pipe="./graph.sh"}
digraph {
  l1 [label="libjpeg v1"]
  l2 [label="libjpeg v2"]
  f1 [label="Firefox v10"]
  f2 [label="Firefox v10"]

  l1 -> f1
  l1 -> "LibreOffice v1"
  l2 -> f2
}
```

Now let's say version 11 of Firefox is released, which must use libjpeg version 2. We ask Nix to update our system, and it installs a *separate copy* of Firefox, alongside the others:

```{.unwrap pipe="./graph.sh"}
digraph {
  l1 [label="libjpeg v1"]
  l2 [label="libjpeg v2"]
  f1 [label="Firefox v10"]
  f2 [label="Firefox v10"]
  f3 [label="Firefox v11"]

  l1 -> f1
  l1 -> "LibreOffice v1"
  l2 -> f2
  l2 -> f3
}
```

We now have three copies of Firefox (two of version 10, one of version 11). How does the system know which copy of Firefox to run when we click the icon?

Firstly, Nix identifies packages by hashing (calculating a "fingerprint") of them *and their dependencies*. This lets us have two copies of Firefox v10, since their dependencies (libjpeg) are different.

Secondly, each user has a "profile" listing which packages they want. So what's a profile? It's just another package! When you "install a package", you're actually just creating a new version of your profile package, which has different dependencies than the old version. Hence, a more complete version of the above diagram would be:

```{.unwrap pipe="./graph.sh"}
digraph {
  l1 [label="libjpeg v1"]
  l2 [label="libjpeg v2"]
  f1 [label="Firefox v10"]
  f2 [label="Firefox v10"]
  f3 [label="Firefox v11"]
  p1 [label="chris profile 1"]
  p2 [label="chris profile 2"]
  p3 [label="chris profile 3" style="bold"]
  lo [label="LibreOffice v1"]

  l1 -> f1
  l1 -> lo
  l2 -> f2
  l2 -> f3

  f1 -> p1
  lo -> p1

  f2 -> p2
  lo -> p2

  f3 -> p3
  lo -> p3
}
```

Each user has an "active" profile (drawn in bold above), usually the latest version. Since the old versions are still installed, we can easily "roll back" any changes we make by activating an old profile version. The changes will still be installed too, if we want to re-activate the newer profile.

Of course, we don't want old versions hanging around forever. To reclaim disk space we can do a "garbage collection"; that will get rid of old profile packages, along with anything that isn't depended on any more. If `chris profile 3` is active and we do a garbage-collect, we'd end up with this:

```{.unwrap pipe="./graph.sh"}
digraph {
  cp [label="chris profile 3"]
  lo [label="LibreOffice v1"]
  ff [label="Firefox v11"]

  "libjpeg v1" -> lo
  lo -> cp
  "libjpeg v2" -> ff
  ff -> cp
}
```

# Replication #

Getting two machines into the same state is the raison d'etre of tools like Puppet. With Nix, we can just install the same profile package on both machines.

Nix actually facilitates this use-case. The "closure" of a package is a list of all its dependencies, and their dependencies, and so on. Nix can make an archive for us containing a package and its closure, which we can take to another machine (eg. on a USB stick) to get the same setup on both.

# NixOS #

There's a Linux distribution called NixOS which uses Nix to manage all of the packages and configuration of the whole system. That's what I use on my laptop.

# Containers #

Since we can install multiple profile packages side-by-side, we can use profiles as a poor man's "container".

Containers are used to pretend that we have lots of small computers, when in fact we only have one big computer. One use-case of containers is shared hosting, when we sell access to each container to a different customer. Nix profiles are not suitable for that, since users could easily find ways to "break out" of their container into someone else's. However, another common use is from a system administration perspective: if we want to keep some systems separate to prevent interference (eg. our Web server from our database server), we might find Nix profiles to be a simple, lightweight way to do it.

# NixOps #

For the stronger, more secure containment scenario, we can use NixOps. This allows Nix packages to depend on *other machines*, which might be real, virtual, in the "cloud", etc. For example, we might have a "DB server" package which runs a database on a particular machine.

# DisNix #

If we have a large collection of machines to manage, we can abstract over them using DisNix. We create Nix packages for each "service", for example "Users database", "Customer database", "Accounting database", etc. (which may each use a "DB server" package to manage their machine). We then use dependencies to create, for example, a "Web Site" package which depends on all of these.

I have to admit I've not used NixOps or DisNix!

[nixos]: http://nixos.org
[nixpost]: /projects/nixos/switching_to_nixos.html
[hackernews]: https://news.ycombinator.com/item?id=8729061
