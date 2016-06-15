---
title: Continuous Integration
---

Whilst Nix's strict control of software versions and dependencies is excellent when handling *other people's* programs, it can be a little frustrating when developing one's own software.

In particular, version control systems like Git have their own notion of immutable state (commits) and dependencies (each commit's hash includes that of its parent, just like Nix derivations include their dependencies' hashes). This similarity can often lead to overlaps, where I need to synchronise Nix packages with git updates, or vice versa.

I've tried several methods of resolving this, but have finally found one that I'm happy with. Here I'll explain these approaches.

## Manual Git Revisions ##

This is the most tedious, but it ensures you're using a known-good configuration. For example, this is the way you'd manage an official package in nixpkgs.

Let's say we're maintaining the following packages `foo` and `bar`. For the sake of argument, let's say we're maintaining these definitions in our `~/.nixpkgs/config.nix` file:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = fetchgit {
                 url    = "https://example.com/foo.git";
                 rev    = "f000001";
                 sha256 = "f000000000000000000000000000000000000000000000000001";
               };
      };

bar = stdenv.mkDerivation {
        name = "bar";
        src  = fetchgit {
                 url    = "https://example.com/bar.git";
                 rev    = "b000001";
                 sha256 = "b000000000000000000000000000000000000000000000000001";
               };
        buildInputs = [ foo ];
      };
```

Now let's say we make a change to `foo`, which lives in a new git revision `f000002`. We need to go and edit our package definition:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = fetchgit {
                 url    = "https://example.com/foo.git";
                 rev    = "f000002";
                 sha256 = "f000000000000000000000000000000000000000000000000001";
               };
      };
```

In fact, this package will not build, since the SHA256 hash will not match. Keep in mind that the SHA sum of the git commit doesn't have anything to do with the SHA sum of the nix derivation, so we can't just copy it over. Instead, we must ask Nix what the new SHA sum should be, and the easiest way to do that is to attempt to build the package:

```
$ nix-shell -p foo
...
output path ‘/nix/store/...foo’ should have r:sha256 hash ‘f000000000000000000000000000000000000000000000000001’, instead has ‘f000000000000000000000000000000000000000000000000002’
...
```

This is what we were expecting, so we can now safely copy that new hash in place of the old one:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = fetchgit {
                 url    = "https://example.com/foo.git";
                 rev    = "f000002";
                 sha256 = "f000000000000000000000000000000000000000000000000002";
               };
      };
```

Clearly, this is a tedious process. The worst part is that we need to do the same thing even for *tiny* changes. Let's say that `bar` requires some small change to the API of `foo`, e.g. changing a private function into a public one (before you scoff, read on for a treatment of the software engineering aspects!).

Even if we have [a nice development workflow for `bar`](developing_on_nixos.html), with this setup we will still need to bump the version of `foo` *globally*, just to have the new version available to `bar`.

This is problematic in a couple of ways: firstly, *all* package definitions will now point to the new version of `foo`. Of course, thanks to the way Nix works this won't affect any *existing* derivations (i.e. all of our installed packages will carry on working as-is), however it will affect *new* derivations, like those instantiated by `nix-shell`. Symptoms might include a load of expensive rebuilds, or subtle breakages that we don't want to contend with *right now* (software engineers, read on!).

The other problem is that our change might not work! With this approach, we must go to all this effort and bump the global version of `foo` *just to try something out*!

As for the software engineering perspective, it's certainly true that any change in public API should lead to a version increase, and is not considered "tiny". It's also true that we should try not to release new API versions if they're known to break existing clients, at least without understanding whether the breakages are acceptable.

*However*, our problem shouldn't have anything to do with releases and versions! Even at the stage of *experimenting* and *prototyping*, to see if the proposed change will even work in the first place, we are suddenly forced to deal with a release management scenario!

This is obviously a bad state of affairs, in particular because it penalises modularity: these headaches in tying packages together will subconsciously bias us against re-using existing components, or splitting up our task into smaller sub-tasks.

## Ignoring Git ##

We can also go to the opposite extreme and ignore version control altogether! Consider the following:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = ~/Programming/foo;
      };

bar = stdenv.mkDerivation {
        name = "bar";
        src  = ~/Programming/bar;
        buildInputs = [ foo ];
      };
```

Using hard-coded paths is clearly a bit of a code smell, as these package definitions are no longer portable to other machines. However, look at what we've gained! We no longer need to specify a git revision or an SHA256 checksum: Nix will scan the contents of the directories, and if they've changed from the last build it will copy them into the store and make a new derivation.

Unfortunately, there are obvious problems with this approach. In particular, we've still got the issue that any changes we make will have a global effect. In fact, we no longer have to commit something in order to alter our system! If we have unstaged changes in the `foo` or `bar` directories, any new derivations depending on these packages will get those changes, regardless of whether we intended them to or not.

This undermines a lot of the purpose of Nix, since we want to have confidence in our system integrity *and* the freedom to develop software without fear of breaking anything with half-finished changes.

## Automating Updates ##

Another solution I tried is to automate the process of updating revisions and hashes. I wrote two scripts, called `bumpCommit` and `bumpEverything`, to aid this process. First, the packages would be defined to load their revision and hash attributes from external files:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = fetchgit {
                 url    = "https://example.com/foo.git";
                 rev    = import "./foo.rev.nix";
                 sha256 = import "./foo.sha256.nix";
               };
      };

bar = stdenv.mkDerivation {
        name = "bar";
        src  = fetchgit {
                 url    = "https://example.com/bar.git";
                 rev    = import "./bar.rev.nix";
                 sha256 = import "./bar.sha256.nix";
               };
        buildInputs = [ foo ];
      };
```

`bumpCommit` would look at the current working directory, and use a look up table to find the corresponding `rev.nix` and `sha256.nix` files, which it would overwrite with the new versions.

Since these package definitions *themselves* are stored in git repositories, running `bumpCommit` on a project may cause changes in other projects, which requires more commits, and so on.

That's where `bumpEverything` came in: it would loop through all projects in dependency order, commit any outstanding changes to `rev.nix` or `sha256.nix` files, then invoke `bumpCommit`.

This was clearly a hack, and was very fragile in the face of changing projects, e.g. splitting a project into two parts. It also generated a large number of git commits, which would do nothing other than bump revision numbers and hashes.

## latestGit ##

My current solution abandons the `bumpCommit` and `bumpEverything` commands. Instead, we try to combine the best features of the git approach *and* the no-git approach.

The aim is to treat a git URL as if it were a local directory: no need to specify a particular revision (just fetch a sensible default like `HEAD`, `master`, etc.) and no need to specify a particular hash (just like with local directories, if it's changed then build a new derivation; of course, in practice a particular git revision *will not* change, and is hence safe to cache).

To do this, we abuse Nix's idea of derivations in order to run arbitrary scripts and cache the results in the Nix store for subsequent access.

First, we write a "package" which is just a single file, containing the latest git revision of a particular repository:

```
with import <nixpkgs> {};
with builtins;

getHeadRev = { url, ref ? "HEAD" }:
  stdenv.mkDerivation {
    inherit url ref;
    name    = "repo-head-${hashString "sha256" url}";
    version = toString currentTime;

    # Required for SSL
    GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

    buildInputs  = [ git gnused ];
    buildCommand = ''
      source $stdenv/setup
      # printf is an ugly way to avoid trailing newlines
      printf "%s" $(git ls-remote "$url" "$ref" | head -n1 | sed -e 's/\s.*//g') > "$out"
    '';
  };
```

The `getHeadRev` function takes a `url` parameter and, optionally, a `ref` (defaulting to `HEAD`). It then generates a package with a name derived from the given URL, and a version based on the current time; this ensures that we avoid the Nix cache.

The contents of the package, stored in the file at location `$out`, are generated by the `buildCommand`, which queries the given URL for the latest revision ID.

Next, we need a way to access this revision information from within Nix. We do this by coercing the packages generated by `getHeadRev` into strings, which will correspond the the `$out` path at which they're stored. We use `readFile` to read the contents of the generated files:

```
rev = args: unsafeDiscardStringContext (readFile "${getHeadRev args}");
```

We use `unsafeDiscardStringContext` to ignore the "dependencies" of this string, i.e. the particular invocation of the `git ls-remote` command; all we care about is the revision, not the time at which it was looked up.

Now that we have a git revision, we just need to avoid the hash requirement of `fetchgit`, to prevent it from being a so-called "fixed-output derivation". We do this in two steps, utilising Nix's lazy evaluation. First, we do a regular `fetchgit` invocation:

```
fg = args: fetchgit {
       url = args.url;
       rev = rev args;

       # Dummy hash
       sha256 = hashString "sha256" args.url;
     };
```

Finally, we override this derivation to strip out all of the hashing information:

```
latestGit = args: stdenv.lib.overrideDerivation (fg args) (old: {
              outputHash     = null;
              outputHashAlgo = null;
              outputHashMode = null;
              sha256         = null;
            });
```

Without these details, Nix will use the hashes it calculates from the source. Now we can use this `latestGit` function to specify our package sources:

```
foo = stdenv.mkDerivation {
        name = "foo";
        src  = latestGit {
                 url    = "https://example.com/foo.git";
               };
      };

bar = stdenv.mkDerivation {
        name = "bar";
        src  = latestGit {
                 url    = "https://example.com/bar.git";
               };
        buildInputs = [ foo ];
      };
```

This ensures that Nix and git are always synchronised, since git revisions are now the canonical form of package versions, and Nix will ask Git if these have changed when a new derivation is being instantiated.

From a development point of view, this gives us the freedom to tinker and experiment without fear of breaking our system. When our local changes are suitable for wider use, we can do the usual `git push` to make them available to the world; which now includes *our* installation of Nix!

Our packages are also portable, as long as `latestGit` is made available somewhere.

This doesn't *quite* solve the problem of using experimental versions of `foo` from within `bar`, however we're free to define *extra* packages, like `foo-unstable`, which use the local directory as their source. If we make `foo` a *parameter* of the `bar` package, we can override it with `nix-shell` to get a one-off development shell using `foo-unstable`, without breaking anything, without exposing our dodgy prototypes to the world, and without building a mountain of trivial git commits.
