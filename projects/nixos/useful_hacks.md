---
title: Useful Nix Hacks
packages: [ 'jq', 'nix-instantiate', 'timeout' ]
---

Here are a few helpful Nix expressions I've accumulated over the years, in case
they're useful to anyone else. I'll assume the following stuff is in context:

```{pipe="tee preamble.nix"}
with builtins;
with import <nixpkgs> {};
with lib;
```

Since some of these things will reference others, let's assume they're all
wrapped up in a big `rec {...}` expression.

<!-- Write each expression individually, as is should appear on the page -->

```{pipe="cat > def_filesIn.nix"}
nixFilesIn = dir: mapAttrs (name: _: import (dir + "/${name}"))
                           (filterAttrs (name: _: hasSuffix ".nix" name)
                                        (readDir dir));
```

```{pipe="cat > def_sanitiseName.nix"}
sanitiseName = stringAsChars (c: if elem c (lowerChars ++ upperChars)
                                    then c
                                    else "");
```

```{pipe="cat > def_withDeps.nix"}
withDeps = deps: drv: overrideDerivation drv (old: {
  extraDeps = (old.extraDeps or []) ++ deps;
});
```

```{pipe="cat > def_fetchGitHashless.nix"}
fetchGitHashless = args: stdenv.lib.overrideDerivation
  # Use a dummy hash, to appease fetchgit's assertions
  (fetchgit (args // { sha256 = hashString "sha256" args.url; }))

  # Remove the hash-checking
  (old: {
    outputHash     = null;
    outputHashAlgo = null;
    outputHashMode = null;
    sha256         = null;
  });
```

```{pipe="cat > def_fetchLatestGit.nix"}
# Get the commit ID for the given ref in the given repo
latestGitCommit = { url, ref ? "HEAD" }:
  runCommand "repo-${sanitiseName ref}-${sanitiseName url}"
    {
      # Avoids caching. This is a cheap operation and needs to be up-to-date
      version = toString currentTime;

      # Required for SSL
      GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

      buildInputs = [ git gnused ];
    }
    ''
      REV=$(git ls-remote "${url}" "${ref}") || exit 1

      printf '"%s"' $(echo "$REV"        |
                      head -n1           |
                      sed -e 's/\s.*//g' ) > "$out"
    '';

fetchLatestGit = { url, ref ? "HEAD" }@args:
  with { rev = import (latestGitCommit { inherit url ref; }); };
  fetchGitHashless (removeAttrs (args // { inherit rev; }) [ "ref" ]);
```

```{pipe="cat > def_latestGit.nix"}
latestGit = { url, ref ? "HEAD" }@args:
  with rec {
    # Make a unique variable name for this repo/ref combo
    key    = "${hashString "sha256" url}_${hashString "sha256" ref}";

    # Look it up
    envRev = getEnv "nix_git_rev_${key}";

    # Fetch the latest revision, if needed
    newRev = import (latestGitCommit { inherit url ref; });

    # If the environment contains a revision, use it; otherwise fetch one
    rev = if envRev == "" then newRev else envRev;
  };
  fetchGitHashless (removeAttrs (args // { inherit rev; }) [ "ref" ]);
```

```{pipe="cat > def_dirsToAttrs.nix"}
dirsToAttrs = dir: mapAttrs (n: v: if v == "regular"
                                      then dir + "/${n}"
                                      else dirsToAttrs (dir + "/${n}"))
                            (readDir dir);
```

```{pipe="cat > def_attrsToDirs.nix"}
isPath  = x: typeOf x == "path" || (isString x && hasPrefix "/" x);

toPaths = prefix: val: if isPath val || isDerivation val
                          then [{ name  = prefix;
                                  value = val; }]
                          else concatMap (n: toPaths (if prefix == ""
                                                         then n
                                                         else prefix + "/" + n)
                                                     (getAttr n val))
                                         (attrNames val);

toCmds = attrs: map (entry: with {
                              n = escapeShellArg entry.name;
                              v = escapeShellArg entry.value;
                            };
                            ''
                              mkdir -p "$(dirname "$out"/${n})"
                              ln -s ${v} "$out"/${n}
                            '')
                    (toPaths "" attrs);

attrsToDirs = attrs: runCommand "merged" {}
  (''mkdir -p "$out"'' + concatStringsSep "\n" (toCmds attrs));
```

```{pipe="cat > def_withNix.nix"}
withNix = attrs: attrs // {
  buildInputs = (attrs.buildInputs or []) ++ [ nix ];
  NIX_PATH    = if getEnv "NIX_PATH" == ""
                   then "nixpkgs=${toString <nixpkgs>}"
                   else getEnv "NIX_PATH";
  NIX_REMOTE  = if getEnv "NIX_REMOTE" == ""
                   then "daemon"
                   else getEnv "NIX_REMOTE";
};
```

```{pipe="cat > def_wrap.nix"}
wrap = { paths ? [], vars ? {}, file ? null, script ? null, name ? "wrap" }:
  assert file != null || script != null ||
         abort "wrap needs 'file' or 'script' argument";
  with rec {
    set  = n: v: "--set ${escapeShellArg (escapeShellArg n)} " +
                   "'\"'${escapeShellArg (escapeShellArg v)}'\"'";
    args = (map (p: "--prefix PATH : ${p}/bin") paths) ++
           (attrValues (mapAttrs set vars));
  };
  runCommand name
    {
      f           = if file == null then writeScript name script else file;
      buildInputs = [ makeWrapper ];
    }
    ''
      makeWrapper "$f" "$out" ${toString args}
    '';
```

```{pipe="cat > def_pipeToNix.nix"}
pipeToNix = attrsToDirs {
    bin = {
      pipeToNix = wrap {
        name   = "pipeToNix";
        vars   = removeAttrs (withNix {}) [ "buildInputs" ];
        script = ''
          #!/usr/bin/env bash
          set -e

          # If an argument is given, it's used as the file name (which Nix will
          # prefix with a content hash).

          NAME="piped"
          [[ -z "$1" ]] || NAME="$1"

          SCRATCH=$(mktemp -d)
          trap "rm -rf $SCRATCH" EXIT

          F="$SCRATCH/$NAME"
          cat > "$F"

          nix-store --add "$F"
        '';
      };
    };
  };
```

<!-- Combine together so we can use Nix to generate real outputs -->

```{pipe="sh > result.nix"}
cat preamble.nix
printf '\nrec {\n'

for F in def*.nix
do
  cat "$F"
  printf '\n\n'
done

printf '\n}\n'
```

<!-- Simple script to evaluate expressions -->

```{pipe="cat > eval && chmod +x eval"}
#!/usr/bin/env bash
set -e

# Tell withTimeout to abort if we use this much RAM
export MAX_KB=1000000

# $1 is the expression we show
printf '> %s\n\n' "$1"

# We always use these imports, but we don't show them (for brevity)
PREAMBLE='with builtins;
          with import <nixpkgs> {};
          with lib;
          with import ./result.nix;'

# Evaluate the actual expression: we insert PREFIX and SUFFIX, if given, to
# allow any required fiddling that we don't want to show; for example, calling
# 'toString', or forcing a derivation to be built, etc.
RESULT=$(withTimeout nix-instantiate --show-trace --read-write-mode --eval \
                                     -E "$PREAMBLE $PREFIX $1 $SUFFIX")

# Show some debug/progress info when rendering; set QUIET if you want to use the
# stderr on the page (e.g. using 2>&1 to show real error messages)
[[ -n "$QUIET" ]] || echo "RESULT: $RESULT" 1>&2

# Set UNWRAP if you want to unquote strings
if [[ -n "$UNWRAP" ]]
then
  RESULT2=$(echo "$RESULT" | jq -r '.')
else
  RESULT2="$RESULT"
fi

# Set FORMAT to have your JSON laid out nicely
if [[ -n "$FORMAT" ]]
then
  echo "$RESULT2" | jq '.'
else
  echo "$RESULT2"
fi
```

### Importing Directories ###

Nix provides the `readDir` primitive to get the contents of a directory. We can
use this to import all files ending in `.nix`, for example:

```{pipe="cat def_filesIn.nix"}
```

This lets us do things like:

```{pipe="sh"}
mkdir modules
echo 'Not a Nix file'                     > modules/notANixFile.txt
echo '"string from foo.nix"'              > modules/foo.nix
echo '["list" "from" "bar.nix"]'          > modules/bar.nix
echo '{ attrs = { from = "baz.nix"; }; }' > modules/baz.nix

FORMAT=1 UNWRAP=1 PREFIX='toJSON (' SUFFIX=')' ./eval 'nixFilesIn ./modules'
```

This can be useful for making modular configurations, without needing to specify
all of the required imports individually (in fact, that's how I define these
things in [my nix-config repo](/git/nix-config)). It can also be used for other
sorts of file, or even recursively on sub-directories...

### Converting Between Attrsets and Directories ###

If we have a directory full of files, we might want to access them in Nix,
whilst maintaining their directory hierarchy. We can represent files using their
paths, directories using attribute sets and hierarchies by nesting:

```{pipe="cat def_dirsToAttrs.nix"}
```

We can use these attribute sets however we like, e.g. `import`ing the files,
putting them in derivations, etc. If we give a path value, the files will be
added to the Nix store (this is good for reproducibility):

```{pipe="sh"}
FORMAT=1 UNWRAP=1 ./eval 'toJSON (dirsToAttrs ./modules)'
```

We might prefer the hierarchy to be preserved in the store, which we can do by
coercing our path to a string:

```{pipe="sh"}
FORMAT=1 UNWRAP=1 ./eval 'toJSON (dirsToAttrs "${./modules}")'
```

If we convert with `toString`, the path will be used as-is (this is good if we
have relative paths, symlinks, etc.):

```{pipe="sh"}
FORMAT=1 UNWRAP=1 ./eval 'toJSON (dirsToAttrs (toString ./modules))'
```

Likewise, we might have a bunch of derivations or paths neatly arranged in Nix
attribute sets which we'd like to write to disk, without wanting to write a
bunch of boilerplate. We can do this as follows:

```{pipe="cat def_attrsToDirs.nix"}
```

This makes it easy to structure the output of a derivation without having to
resort to bash scripts:

```{pipe="sh"}
export PREFIX='toString ('
export SUFFIX=')'
export UNWRAP=1
./eval 'attrsToDirs { bin = { myProg = writeScript "myProg" "echo hello"; }; }'
```

### Sanitising Names ###

If we have an arbitrary string, we can't just use it as a Nix derivation name
since it may contain forbidden characters. We can strip them out like this:

```{pipe="cat def_sanitiseName.nix"}
```

Now we can do, for example:

```{pipe="sh"}
UNWRAP=1 ./eval 'toFile (sanitiseName "a/b/c/d") "foo"'
```

Of course, we could just hard-code a particular name when we're generating a
derivation (like `"merged"` above), but that makes it harder to see what's going
on during the build process (e.g. if we see 20 different builds called `merged`,
it's hard to tell what's succeeded and what's left to go).

### Adding Extra Dependencies ###

Sometimes we might have a Nix package which builds, but we don't want it to; for
example if we want to check a bunch of tests and abort if they fail. Nix
packages tend to run their tests in an attribute called `testPhase`, which is
fine when we're defining packages from scratch, but can be awkward if we're
trying to add extra tests to some existing derivation: not all derivations
follow the `testPhase` convention, and appending to bash scripts isn't a
particularly simple or composable approach in any case (what if additions have
already been added? What if we need extra `buildInputs`?)

What we can do instead is make our extra tests into one or more derivations
(which may or may not depend on the original package), then append them as extra
build dependencies of the package:

```{pipe="cat def_withDeps.nix"}
```

```{pipe="sh"}
export PREFIX='with { x ='
export SUFFIX='; }; assert forceBuilds [ x ]; toString x'
PASS='runCommand "passingTest" {} '\'\''echo pass > "$out"'\'\'
FAIL='runCommand "failingTest" {} "exit 1"'

UNWRAP=1 ./eval "withDeps [ ($PASS) ] hello" > extraDepPass

 QUIET=1 ./eval "withDeps [ ($FAIL) ] hello" > extraDepFail 2>&1 || true
```

This way, we can make a new package which is equivalent to the original when our
tests pass:

```{pipe="cat extraDepPass"}
```

But which breaks when our tests don't pass:

```{pipe="cat extraDepFail"}
```

### Hashless Git Fetching ###

Nix's fixed output derivations are really useful, for ensuring that inputs are
as expected and for more extensive caching. Unfortunately they're not as useful
in "dynamic" situations, e.g. where we fetch or calculate which git revision to
use.

We can work around this by overriding the hash-checking mechanism of `fetchgit`:

```{pipe="cat def_fetchGitHashless.nix"}
```

With this, we can specify a git repo without needing a hash:

```{pipe="sh"}
# We call the git derivation 'x', force it to be built (to ensure hash checking
# is skipped), then spit out its store path
export PREFIX='with { x = '
export SUFFIX='; }; assert forceBuilds [ x ]; toString x'

UNWRAP=1 ./eval 'fetchGitHashless {
    url = "http://chriswarbo.net/git/chriswarbo-net.git";
    rev = "7a5788e";
  }'
```

### Fetch Latest Git ###

Sometimes we want to use the latest version of a git repo, rather than keeping a
hard-coded revision/checksum pair in our source. I don't recommend this for
third-party libraries, but it can be useful for integration testing and for
projects which have components spread across several repos.

We can do this using a derivation which fetches the repo's `HEAD` (or a branch,
etc.), imports the result and passes it to `fetchGitHashless`:

```{pipe="cat def_fetchLatestGit.nix"}
```

Now we only have to provide a URL (and optionally a branch) and our repos will
stay up to date, e.g.:

```{pipe="sh"}
export PREFIX='toString ('
export SUFFIX=')'
UNWRAP=1 ./eval 'import (fetchLatestGit {
           url = "http://chriswarbo.net/git/turtleviewer.git";
         }) {}'
```

Notice that we `import` the result of `latestGitCommit`; this prevents the
dependencies of that derivation (notably `currentTime`) from becoming
dependencies of the resulting repo, and hence causing everything to keep
rebuilding. Importing ensures that only a change in the commit ID will trigger a
rebuild. The downside is that we blur the distinction between evaluation and
building, which can slow down any Nix commands which evaluate these expressions.

### Cached Latest Git ###

When Nix evaluates an expression, the value of `builtins.currentTime` remains
constant for the whole time Nix is running; this ensures values are consistent,
and means we can avoid the cache (like in `latestGitCommit`) without having our
sub-expressions re-evaluated over and over. This doesn't prevent our expressions
being re-evaluated on *different* Nix runs though; which can be slow if we have
a shell script that runs a bunch of separate Nix commands.

We can prevent such slowdowns by augmenting `fetchLatestGit` to check for an
environment variable first, using `builtins.getEnv`; if we set that environment
variable in our shell script, we can stop Nix checking it over and over:

```{pipe="cat def_latestGit.nix"}
```

Thanks to laziness, we will never check the repo if we find a revision in the
environment. This uses one environment variable per repo/ref combination (taking
the hash of each, to ensure no funny characters appear in the name); we could
also use one big environment variable, e.g. containing a JSON array of repos,
refs and commits. The advantage of multiple variables is that we can append new
commits to the environment trivially; the advantages of one big variable are
that it's trivial to see all of the available repos, refs and commits (rather
than e.g. scanning through the environment), and we don't need to do any hashing
since all of the variable data appears in the value rather than the name.

### Use Nix in Builders ###

We might want to use Nix commands from within a builder, for example to evaluate
a dynamically-generated expression, to add a file to the store or to invoke some
other build. This tends to fail due to missing environment variables. The
following will augment a given attribute set to contain the needed config:

```{pipe="cat def_withNix.nix"}
```

With this, we can say things like:

```{pipe="sh"}
FUNC='runCommand "foo" (withNix { myVar = "hello"; })'
 STR=$(printf "''\n    %s\n    %s\n  ''" 'echo "$myVar" > myFile' \
                                         'nix-store --add myFile > "$out"')
EXPR="$FUNC $STR"
export PREFIX='toString ('
export SUFFIX=')'
UNWRAP=1 ./eval "$EXPR"
```

This uses `nix-store` to add a file to the Nix store, and puts the resulting
path into its output. On its own that's a pretty useless thing to do, but it may
be useful to run such Nix commands as part of a more complicated build
process. For example, we've seen that we can blur the eval/build distinction by
using `import` on a derivation, which triggers a build and makes the result
available to the evaluator. `withNix` goes the other way: it allows evaluation
to be performed during a build, which may be required if the expression we want
isn't available at eval time.

An example of this is projects which use Nix as their build system: if we want
to include such a project in our Nix config, there are two ways to do it:

 - Fetch the source using a derivation and use `import` to bring all of its
   expressions into scope. This has the downside that "fetch" might be a very
   expensive process, e.g. downloading, compiling, generating files, etc. which
   we might want to avoid during "normal" evaluations.
 - Using `withNix` we can write a simple build script which calls `nix-build`
   (or whatever the project uses) in its source directory, just as if it were
   using `make`, or `sbt`, or any other build tool.

Which of these approaches is preferable depends on the situation. In any case,
it's advisable to avoid nesting Nix commands inside each other too much, as it
can be difficult to control and/or override things as they get wrapped up.

Also note that we can vary the values we use for the environment variables; for
example, if we want to override the nixpkgs collection that's used by the build,
we can just pass some other value for `NIX_PATH`. One thing to keep in mind is
that embedding a path directly into a string, like `"nixpkgs=${myPath}"` or
`"nixpkgs=" + myPath`, will cause that path to be added to the Nix store; this
may or may not be what you want (e.g. it might break symlinks and relative
paths). To use the path as-is you should pass it to `builtins.toString`, e.g.
`"nixpkgs=${toString myPath}"`.

Another trick you might like to use if you're overriding `nixpkgs` is to avoid
causing an infinite loop when you want to reference the "real" `nixpkgs`. For
example, say we want to force a particular package to be available, we might
say:

```
with rec {
  myNixpkgs = attrsToDirs {
    "default.nix" = writeScript "default.nix" ''
      args: (import <nixpkgs> args) // {
        foo = runCommand "foo" '''
          mkdir -p "$out/bin"
          printf '#!/usr/bin/env bash\necho "foo"' > "$out/bin/foo"
        ''';
      }
    '';
  };
};
runCommand "bar"
  (withNix {
    # Override NIX_PATH to use our augmented copy
    NIX_PATH = "nixpkgs=$(myNixPkgs)";
  })
  ''
    nix-shell -p foo --run foo 1>&2
    echo "done" > "$out"
  ''
```

The idea is that the builder for `bar` will use `myNixpkgs` as its `<nixpkgs>`
path, and hence `nix-shell` will find the `foo` package we define in there. The
problem is that pretty much all Nix definitions, including those for `myNixpkgs`
and `foo`, use something from `<nixpkgs>` somewhere; hence if we try to use
`myNixpkgs` as `<nixpkgs>`, it will end up `import`ing itself, which will end up
`import`ing itself, and so on forever.

To work around this, we can give the "real" `nixpkgs` a different name, then use
that to break the cycle. I tend to call it `<real>`:

```
with rec {
  myNixpkgs = attrsToDirs {
    "default.nix" = writeScript "default.nix" ''
      with tryEval <real>;
      with rec {
        # Will be <nixpkgs> at the 'top level', and <real> in recursive calls
        path = if success then <real> else <nixpkgs>;
      };
      args: (import path args) // {
        foo = runCommand "foo" '''
          mkdir -p "$out/bin"
          printf '#!/usr/bin/env bash\necho "foo"' > "$out/bin/foo"
        ''';
      }
    '';
  };
};
runCommand "bar"
  (withNix {
    # Override NIX_PATH to use our augmented copy
    NIX_PATH = "nixpkgs=${myNixPkgs}:real=${toString <nixpkgs>}";
  })
  ''
    nix-shell -p foo --run foo 1>&2
    echo "done" > "$out"
  ''
```

### Wrapping Binaries ###

`nixpkgs` provides a useful script called `makeWrapper` which allows a program
or script to be "wrapped" such that it's run in a particular environment; for
example, using a `PATH` which contains its dependencies. Since `makeWrapper` is
a Bash function it's a little horrible to use. We can make a nicer interface
inside Nix (and since I often use `makeWrapper` in conjunction with
`writeScript`, we can combine both):

```{pipe="cat def_wrap.nix"}
```

Here's an example of a script being wrapped in its dependencies and some
variables:

```{pipe="cat > wrapScript.nix"}
wrap {
    name    = "wrapped-script";
    paths   = [ bash jq ];
    vars    = { VAR1 = toJSON { someKey = "someValue"; }; };
    script  = ''
      #!/usr/bin/env bash
      jq --arg var "$VAR1" '. + $var1.someKey'
    '';
  }
```

```{pipe="sh"}
UNWRAP=1 PREFIX='toString (' SUFFIX=')' ./eval "$(cat wrapScript.nix)"
```

Alternatively, we can pass a file instead of a script:

```{pipe="cat > wrapFile.nix"}
wrap {
  name  = "wrapped-file";
  paths = [ python ];
  vars  = { foo = "bar"; };
  file  = ./script.py;
}
```

```{pipe="sh"}
echo 'print "hello"' > script.py
UNWRAP=1 PREFIX='toString (' SUFFIX=')' ./eval "$(cat wrapFile.nix)"
```

### Piping Data to the Nix Store ###

The `nix-store --add` command is really useful for adding data to the Nix store,
and also for making better use of it as a cache (since it chooses a filename
based on a hash of the content, we can use it to spot duplicate data).

The downside is that our data must be written to files first; we can't just
stream it in. Often Bash's process substitution would work around this, but not
in this case (it causes `/dev/fd/...` symlinks to be added rather than the
content).

The following `pipeToNix` command will write its stdin to a temporary file, then
add it to the Nix store:

```{pipe="cat def_pipeToNix.nix"}
```

With this in `PATH`, we can say things like:

```{pipe="cat > pipeToNix.sh"}
echo "foo" | pipeToNix myFilename
```

```
printf '$ %s\n' "$(cat pipeToNix.sh)"
chmod +x pipeToNix.sh
./pipeToNix.sh
```

Note how we get back the Nix store path, which we can use elsewhere in a script,
for example.

### Inheriting Function Arguments ###

Nix functions have two sorts of arguments: variables, like `x: ...`; and named
argument sets, like `{ foo, bar }: ...`. If we use the latter, those names can
be found by passing the function to `builtins.functionArgs`. This is used by
common utility functions like `callPackage` to figure out which arguments to
pass in.

Unfortunately, this introspection ability is lost if we wrap up a function
somehow, for example via composition: `f: g: x: f (g x)`. If we apply this to a
`g` function with named arguments, the result will still accept those same
arguments, but Nix will only see it as taking some variable `x`. We can work
around this by generating Nix code which wraps a function in one with named
arguments, i.e. `withArgs [ "x" "y" ] func` to get the equivalent of
`{x, y}: func { inherit x y; }`:

```
with builtins;
with import <nixpkgs> {};
with lib;

args: f:
  with rec {

    # Build a string "a,b,c" for the arguments "args"
    arglist = concatStringsSep "," args;

    # Strip any dependencies off our string, so it can be embedded
    arglistF = unsafeDiscardStringContext arglist;

    # Write an eta-expansion of "f", which accepts the arguments "args"
    content = "f: args@{${arglistF}}: f args";

    eta = import (toFile "withArgs.nix" content) f;
  };
  eta
```

This lets us give one function some specific named arguments. To give one
function the same named arguments as another, e.g. `withArgsOf f g`, we can use:

```
f: with { fArgs = functionArgs f; };
   withArgs (filter (n: !fArgs."${n}") (attrNames fArgs))
```

For the common case of function composition, we can maintain our named arguments
by using the following:

```
f: g: withArgsOf g (args: f (g args))
```

### Multiple Nixpkgs Versions ###

Sometimes a package is broken in the version of `nixpkgs` we're using, but a fix
exists upstream; other times, a package may be broken by an upgrade. Thankfully
Nix lets pick and choose where to get our packages from. Here's a way to define
multiple `nixpkgs` repos, which minimises boilerplate if we want to add another:

```
with builtins;
with {
  pkgs = import <nixpkgs> {};

  version = { rev, sha256 }:
    with {
      name = replaceStrings ["."] [""] rev;
      repo = pkgs.fetchFromGitHub {
        inherit rev sha256;
        owner = "NixOS";
        repo  = "nixpkgs";
      };
    };
    {
      "repo${name}" = repo;

      # Explicitly pass an empty config, to avoid infinite loops
      "nixpkgs${name}" = import repo { config = {}; };
    };
};

foldl' (x: y: x // y) {} [
  (version {
    rev    = "16.03";
    sha256 = "0m2b5ignccc5i5cyydhgcgbyl8bqip4dz32gw0c6761pd4kgw56v";
  })

  (version {
    rev    = "16.09";
    sha256 = "0m2b5ignccc5i5cyydhgcgbyl8bqip4dz32gw0c6761pd4kgw56v";
  })

  (version {
    rev    = "17.03";
    sha256 = "1fw9ryrz1qzbaxnjqqf91yxk1pb9hgci0z0pzw53f675almmv9q2";
  })

  {
    # We can also use codenames like "stable" (assuming that these definitions
    # are being added to our Nix overrides, and hence available in pkgs)
    stableRepo = pkgs.repo1603;
    stable     = pkgs.nixpkgs1603;

    # Unmodified package set
    origPkgs = pkgs;
  }
]
```

### Checking for Broken Derivations ###

If a derivation is broken, Nix will abort whatever it's doing. This can be
frustrating, since we sometimes *want* derivations to fail. For example, we
might be overriding something from `nixpkgs` because it's broken; but we don't
want to maintain our patches forever: we only want our version to be built if
the one in `nixpkgs` is broken. We can express that using an `isBroken` function
which builds successfully if a given derivation doesn't, and vice versa:

```
with builtins;
with import <nixpkgs> {};
with lib;

# Wrap the buildCommand in a script which checks whether it fails. Note that
# we do this in a crude way by replacing the attribute, rather than using some
# override function; this is to ensure that buildCommand contains everything
# needed (e.g. phases, etc.)
drv:
  with rec {
    orig = if drv ? buildCommand
              then writeScript "buildCommand-${drv.name}" drv.buildCommand
              else writeScript "builder-${drv.name}" ''
                #!${bash}/bin/bash
                "${drv.builder}" ${toString (drv.args or [])}
              '';
    newBuildScript = writeScript "isBroken-${drv.name}-script" ''
      if "${orig}"
      then
        echo "Derivation '${drv.name}' should have failed; didn't" 1>&2
        exit 1
      fi
      echo "${drv.name} is broken" > "$out"
    '';
  };
  # As per https://github.com/NixOS/nixpkgs/issues/4017
  lib.overrideDerivation drv (old: {
    name    = "isBroken-${drv.name}";
    builder = "${bash}/bin/bash";
    args    = [ "-e" newBuildScript ];
  })

```

This way, we can add `isBroken nixpkgs.foo` as a build dependency of our
overridden `foo` package; as soon as the `nixpkgs` version starts to build
properly, our override will break and we can investigate.

### NPM Packages ###

I have, on occasion, found programs which only seem to be distributed via npm.
Whilst Nix does have some associated tools to convert npm packages to Nix, they
seem to be used *outside* of Nix itself. I've found the following Nix function
useful for calling out to these tools automatically:

```
with builtins;
with import <nixpkgs> {};
with lib;
repo:
  with rec {
    converted = runCommand "convert-npm"
      {
        inherit repo;
        buildInputs = [ nodePackages.node2nix ];
      }
      ''
        cp -r "$repo" "$out"
        chmod +w -R "$out"
        cd "$out"
        node2nix
      '';

    generatedPackages = callPackage "${converted}" {};
  };
  generatedPackages.package;

```

### Hackage DB ###

Haskell has a package repository called Hackage, which is used by the Cabal tool
to figure out which versions of each dependency to use in a project (such that
all dependency constraints are satisfied, transitively).

Unfortunately, this isn't very reproducible. To begin with, the `cabal update`
command (the recommended way to fetch the list of available packages) only ever
gets the latest list, which cause Cabal to find a different solution to some
constraints.

Here we use a project called `all-cabal-files`, which tracks the contents of
Hackage in a git repository, to emulate the behaviour of `cabal update`, but in
a deterministic way:

```
with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  all-cabal-files = fetchFromGitHub {
    owner  = "commercialhaskell";
    repo   = "all-cabal-files";
    rev    = "c008e28";
    sha256 = "0kfcc7dw6sahgkv130r144pfjsxwzq8h479fw866nf875frvpblz";
  };

  # 00-index.tar, used by Cabal to check versions, dependencies, etc. This takes
  # a while to build, so we keep it in a standalone derivation to reduce the
  # chance that it'll need to be rebuilt (e.g. due to a dodgy test)
  repoCache = stdenv.mkDerivation {
    name = "hackage-00-index.tar";
    src  = all-cabal-files;

    unpackPhase = "true";  # Without this, src will be copied, which takes ages
    buildPhase  = ''
      BASE="$PWD"

      tar cf 00-index.tar -T /dev/null

      echo "Adding package dirs to 00-index.tar" 1>&2
      pushd "$src"
        for F in *
        do
          echo "Adding $F" 1>&2
          tar rf "$BASE/00-index.tar" "$F"
        done
      popd
    '';

    installPhase = ''
      cp -r 00-index.tar "$out"
    '';
  };

  # Command to install repo cache into ~/.cabal
  makeCabalConfigCmd = writeScript "makeCabalConfig" ''
    #!/usr/bin/env bash
    [[ -e "$HOME" ]] || {
      echo "makeCabalConfig requires HOME to put config into" 1>&2
      exit 1
    }
    DIR="$HOME/.cabal/packages/hackage.haskell.org"
    mkdir -p "$DIR"
    TARGET=$(readlink -f "$REPOCACHE")
    cp "$TARGET" "$DIR/00-index.tar"
  '';
};

stdenv.mkDerivation {
  name        = "stable-cabal-config";
  src         = makeCabalConfigCmd;
  buildInputs = [ cabal-install ghc makeWrapper ];
  REPOCACHE   = repoCache;
  unpackPhase = "true";

  doCheck     = true;
  checkPhase  = ''
    echo "Making config" 1>&2
    export HOME="$PWD"
    "$src"

    echo "Testing non-sandboxed install" 1>&2
    cabal install list-extras

    echo "Testing install into a sandbox" 1>&2
    cabal sandbox init
    cabal install list-extras
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    ln -s "$REPOCACHE" "$out/00-index.tar"
    makeWrapper "$src" "$out/bin/makeCabalConfig" \
      --set REPOCACHE "$out/00-index.tar"
  '';
}
```

We can use this to generate a canonical Hackage DB via the following:

```
runCommand "stable-hackage-db" { buildInputs = [ stableHackage ]; } ''
  mkdir -p "$out"
  HOME="$out" makeCabalConfig
''
```

### Resolving with Tinc ###

Tinc is a program which uses Cabal to resolve Haskell package dependencies, but
does so in a deterministic way. We can call out to tinc from Nix to resolve the
dependencies of our Haskell packages, given a few hacks:

```
with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  defHPkg = haskellPackages;

  withTincDeps =
    { extras ? [], haskellPackages, includeExtras, nixpkgs, tincified }: args:
      with rec {
        resolver = withTincPackages {
          inherit extras haskellPackages nixpkgs tincified;
        };

        pkg = resolver.callPackage "${tincified}/package.nix" args;
      };
      if includeExtras
         then resolver.ghcWithPackages (h: [ pkg ] ++ map (n: h."${n}") extras)
         else pkg;

  withTincPackages =
    # Augments a given Haskell package set using definitions taken from a
    # "tincified" Haskell package, i.e. one where the dependencies have been solved
    # by Cabal and turned into a set of (mutually-recursive) Nix expressions by tinc
    # and cabal2nix.
    #
    # We also allow "extra" packages to be supplied, since we can't easily use the
    # "add source" facility of Cabal/tinc.
    #
    # Since the generated definitions in <tincified>/tinc.nix reference each other
    # directly, the callPackage function we pass in only has to resolve the extras.
    # We assume the extras are present in the given haskellPackages, so we use its
    # callPackage function.
    { lib }: with lib;

    { extras ? [], haskellPackages, nixpkgs ? import <nixpkgs> {}, tincified }:
    with rec {
      # Loads the package definitions generated by tinc
      deps = import "${tincified}/tinc.nix" { inherit haskellPackages nixpkgs; };

      # We pass a callPackage function to resolve any "extra" dependencies which
      # weren't given definitions by tinc (usually, those which aren't on Hackage)
      newPkgs = deps.packages { inherit (haskellPackages) callPackage; };

      # We extract the "extra" packages from the given haskellPackages, so they can
      # be included in our new overrides.
      extraPkgs = genAttrs extras (name: haskellPackages."${name}");
    };
    # Combine the given extras with those generated by tinc
    haskellPackages.override { overrides = self: super: extraPkgs // newPkgs; };

  yq = pythonPackages.buildPythonPackage {
    name = "yq";
    src  = fetchurl {
      url    = https://pypi.python.org/packages/14/1b/5efddd608b7df9849be50aca4a0d8602b75fb2929223a44e241d7290d6ea/yq-2.1.1.tar.gz;
      sha256 = "0g7rbmfn7k4rz77iqg29kp24fjpl678gy1g17hx435sdwjns00pd";
    };
    propagatedBuildInputs = with pythonPackages; [
      jq
      pyyaml
    ];
  };
};
{
  # Where to find cached tinc/cabal data. If global, we'll use it in place and
  # potentially update/overwrite it; otherwise we'll use a copy. We try to use a
  # global cache by default, since it's faster; unless we're on Hydra.
  cache           ? if getEnv "NIX_REMOTE" == ""
                       then { global = false; path = stableHackageDb;     }
                       else { global = true;  path = "/tmp/tincify-home"; },

  # Names of extra dependencies to include in the resulting Haskell package set;
  # useful for things which are in your Nix haskellPackages but not in Hackage.
  extras          ? [],

  # Whether to include the given 'extras' in the result using ghcWithPackages.
  includeExtras   ? false,

  # The Hackage DB for Cabal to use, if cache.global is true:
  #  - stableHackageDb is built from a fixed revision of all-cabal-files. This
  #    means it's a constant, deterministic value which Nix caches nicely.
  #  - hackageDb runs 'cabal update' to get the latest versions. This doesn't
  #    cache well, which causes a lot of extraneous rebuilding.
  hackageContents ? stableHackageDb,

  # Haskell package set to use as a base. 'extras' names should appear in here.
  haskellPackages ? defHPkg,

  # Name to use for the resulting package
  name            ? "pkg",

  # A nixpkgs set, used for non-haskell dependencies (e.g. zlib)
  nixpkgs         ? import <nixpkgs> {},

  # We allow other attributes, so Haskell package derivations can be passed in
  # (giving us 'name' and 'src')
  ... }@args:
  with rec {
    # The Haskell package source to run tinc against. Seems to behave funny when
    # specified in the arguments above, so we inherit it here instead.
    inherit (args) src;

    # By default, tinc runs Cabal in a Nix shell with the following available:
    #
    #   haskellPackages.ghcWithPackages (p: [ p.cabal-install ])'
    #
    # This can be fiddled a bit using TINC_NIX_RESOLVER, but overall we're
    # stuck using a Haskell package set that's available globally with a
    # particular attribute path. We don't want that; we want to use the
    # Haskell package set we've been given (haskellPackages), which might not
    # be available globally.
    #
    # To make tinc use this version, we rely on the fact it's only using
    # cabal-install, as above. We build that derivation, using our Haskell
    # package set, write it to disk, then set NIX_PATH such that tinc's
    # nix-shell invocation uses the derivation we built. Phew!
    env = runCommand "tinc-env"
      {
        expr = writeScript "force-tinc-env.nix" ''
          _:
            import <real> {} // {
            haskellPackages = {
              ghcWithPackages = _:
                ${ghcPackageEnv haskellPackages [ "cabal-install" ]};
            };
          }
        '';
      }
      ''
        mkdir -p "$out/pkgs/build-support/fetchurl"
        cp "$expr" "$out/default.nix"
        cp "${<nixpkgs/pkgs/build-support/fetchurl/mirrors.nix>}" \
           "$out/pkgs/build-support/fetchurl/mirrors.nix"
      '';

    tincified = runCommand "tinc-of-${name}"
      (newNixpkgsEnv env (withNix {
        inherit hackageContents;

        src = unpack src;

        buildInputs = [
          cabal2nix
          (haskellPackages.ghcWithPackages (h: [ h.ghc h.cabal-install ]))
          haskellTinc
          yq
          jq
        ];

        TINC_USE_NIX = "yes";

        # Should we share an impure cache with prior/subsequent calls?
        GLOBALCACHE = if cache.global then "true" else "false";

        # Where to find cached data; when global this should be a
        # string like "/tmp/foo". Non-global might be e.g. a path, or a
        # derivation.
        CACHEPATH = assert cache.global -> isString cache.path ||
                    abort ''Global cache path should be a string, to
                            prevent Nix copying it to the store.'';
                    cache.path;

        # Otherwise cabal2nix dies for accented characters
        LANG           = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";

        addSources = writeScript "tinc.json" (toJSON {
          dependencies = map (name: {
                               inherit name;
                               path = unpack haskellPackages."${name}".src;
                             })
                             extras;
        });
      }))
      ''
        function allow {
          # Allows subsequent users to read/write our cached values
          # Note that we ignore errors because we may not own some of
          # the existing files.
          chmod 777 -R "$HOME" 2>/dev/null || true
        }

        if $GLOBALCACHE
        then
          # Use the cache in-place
          export HOME="$CACHEPATH"

          # (Re-)Initialise the cache's Hackage contents
          cp -r "$hackageContents"/.cabal "$HOME"/
        else
          # Use a mutable copy of the given cache
          cp -r "$CACHEPATH" ./cache
          export HOME="$PWD/cache"
          allow
        fi

        [[ -d "$HOME" ]] || {
          echo "Cache dir '$HOME' not found" 1>&2
          exit 1
        }

        cp -r "$src" ./src
        chmod +w -R ./src

        pushd ./src
          if ${if extras != [] then "true" else "false"}
          then
            echo "Adding extra sources" 1>&2
            if [[ -f tinc.yaml ]]
            then
              echo "Merging dependencies into tinc.yaml"
              mv tinc.yaml tinc.yaml.orig
              yq --yaml-output '. * $deps' --argfile deps "$addSources" \
                 < tinc.yaml.orig > tinc.yaml
            else
              yq --yaml-output '$deps' --argfile deps "$addSources" > tinc.yaml
            fi
          fi

          tinc
        popd

        allow
        cp -r ./src "$out"
      '';
  };
  withTincDeps {
    inherit extras haskellPackages includeExtras nixpkgs tincified;
  }
```

This is quite complicated, but it basically lets us use our deterministic
Hackage database from within `tinc`, which will resolve dependencies of a given
Haskell package. `tinc` converts these dependencies into Nix expressions, which
we import and turn into dependencies of the given package.

Note that this also requires tweaks to `tinc` itself, which can be found in [my
fork](/git/tinc).

The result is that we can say, e.g. `tincified haskellPackages.pandoc {}` and
get a derivation for Pandoc with all of its dependencies satisfied by solving
against a particular Hackage snapshot.

This is much easier than attempting to choose only those package versions which
are mutually compatible (like, for example, the Stackage project tries to do; as
well as the Haskell package set offered by `nixpkgs`).