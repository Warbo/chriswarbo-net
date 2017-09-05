---
title: Scripting with Nix
---

[NPM](https://www.npmjs.com/) was originally a rudimentary package manager for
Javascript and node.js, but these days it seems to be getting used as a general
way of fetching dependencies for things like shell scripts.

I've not played with node.js since the very early days, and never really used
NPM. However, I do see the need for modular, composable shell scripts.

## Nix for Scripting

Personally, I've been using Nix in a similar way, since it also has nice
features like caching, laziness, splicing into indented strings, dependency
management, etc. For example, if you have a Nix expression stored in
`my-script.nix` you can use the following (e.g. in `my-script.sh`) to invoke it:

```
nix-instantiate --read-write-mode --show-trace --eval -E 'import ./my-script.nix'
```

The `--eval` tells Nix to evaluate an expression, rather than build a
package. `-E` is the expression to evaluate (in this case, importing our
"script" file). `--read-write-mode` allows the script to add things to the Nix
store (which is normally read-only). `--show-trace` will help us to debug, by
showing a backtrace if a runtime error occurs.

Note that Nix makes a distinction between "evaluation time" and "build time"
(similar to the compile/run time distinction in many other languages). The above
command will *evaluate* the contents of `my-script.nix`, which should be a Nix
expression, but most of the time that's *not* our actual script (after all, Nix
is a pure language, and hence unsuitable for many scripting applications).

Instead, we usually use Nix expressions to compose a "derivation" (basically a
package), into which we can put all sorts of scripts.

If `my-script.nix` evaluates to a derivation, we can use this command instead of
the above, to *build* it:

```
nix-build --show-trace -E 'import ./my-script.nix'
```

This creates a symlink called `result`, so if our derivation is a script we can
then do `./result` to run it.

A cleaner way would be to use the `--no-out-link` option to prevent cluttering
up the working directory with symlinks, capture the stdout using e.g.
`SCRIPT=$(...)`, and running the resulting string using `"$SCRIPT"` (since it
will contain the Nix store path to the result).

By the way, if you want to evaluate Nix expressions interactively, the
`nix-repl` command is much easier to use than `nix-instantiate` (although it
doesn't seem to offer a `--show-trace` option).

Also note that we can use
[nix-shell shebangs](/projects/nixos/nix_shell_shebangs.html) to fetch
dependencies of a script at run time; although I find that separate Nix files
are usually needed to work around the limitations of writing expressions in a
shebang line, and it can often be quite slow to have `nix-shell` invoked on each
run, when we could instead build a script with its dependencies once, and invoke
them as many times as we like with little overhead.

## Strings

The reason I like Nix's treatment of strings is that we can write "indented
strings", which means that long strings (such as scripts) can be embedded inside
other expressions quite naturally, without getting filled with whitespace. For
example, here's a Nix expression:

```
runCommand "foo"
           { buildInputs = [ python imagemagick ]; }
           ''
             I am an indented string
             I will be executed as a bash script, with the following
             dependencies available:
              - python
              - imagemagick

             Since these lines, and those sentences above beginning with "I",
             have the least indentation, they will appear flush to the left in
             the resulting file. The "list" above will hence be indented by 1
             space, rather than the 14 spaces which appear here.
           ''
```

Secondly, splicing allows Nix expressions to be embedded inside strings. A
splice begins with `${` and ends with `}`. The expression should either evaluate
to a string, which is inserted as-is, or a "derivation" (e.g. a package), which
gets "instantiated" (i.e. installed) and its installation directory is inserted
into the resulting string. Splices can be nested too.

For example, instead of giving `python` as a dependency in the `buildInputs`, we
could splice the full path into a string, e.g.

```
''
  "${python}/bin/python" my_script.py
''
```

Although this is probably a bad idea, since there may be transitive
dependencies, etc. missing when the script gets executed.

Splices aren't just a simple hack for resolving variable names; they drop us
into a complete Nix expression context, where we can write arbitrarily
complicated expressions, including other strings, containing other splices, etc.

## Decomposing

If we want to build up a result incrementally, with each step getting cached, we
can use `runCommand`, and write the results to "$out". For example:

```
with import <nixpkgs> {};
with builtins;
with rec {

  # Takes a script and runs it with jq available (Nix functions are curried)
  runJq = runCommand "jq-cmd" { buildInputs = [ jq ]; };

  step1 = runJq ''
    echo "I am step 1" 1>&2
    echo '[{"name": "foo"}, {"name": "bar"}]' | jq 'map(.name)' > "$out"
  '';
  step2 = runJq ''
    echo "I am step 2" 1>&2
    I won't be executed, because Nix is lazy and nothing calls me
  '';
  step3 = runJq ''
    echo "I am step 3" 1>&2
    jq 'length' < "${step1}" > "$out"
  '';
};

import step3
```

When evaluated, this gives the following:

```
$ ./go.sh
building path(s) ‘/nix/store/5ks08zbvmgzbhg9kr0k4g75nf2ymsqsr-jq-cmd’
I am step 1
building path(s) ‘/nix/store/v1svcqq6cmi4xc9650qz9w2x177w4pfr-jq-cmd’
I am step 3
2
$ ./go.sh
2
```

The results are cached, and will be re-used as long as the commands aren't
edited, and their dependencies don't change (e.g. if we update nixpkgs and a
newer version of jq is available, they'll be re-run with that version).

In this case, each "step" represents the data, which is common in lazy
languages. Alternatively, we can use `writeScript` to write more 'traditional'
process-oriented scripts:

```
with import <nixpkgs> {};
with builtins;
with rec {

  # Takes a script and runs it with jq available (Nix functions are curried)
  runJq = runCommand "jq-cmd" { buildInputs = [ jq ]; };

  step1 = writeScript "step-1" ''
    echo "I am step 1" 1>&2
    echo '[{"name": "foo"}, {"name": "bar"}]' | jq 'map(.name)'
  '';

  step2 = writeScript "step-2" ''
    echo "I am step 2" 1>&2
    I won't be executed, because Nix is lazy and nothing calls me
  '';

  step3 = writeScript "step-3" ''
    echo "I am step 3" 1>&2
    "${step1}" | jq 'length'
  '';
};

import (runJq ''"${step3}" > "$out"'')
```

Of course, we need something to invoke these scripts, which is why I used
`runJq` in the final expression. When run, we get:

```
$ ./go.sh
building path(s) ‘/nix/store/fnw68cmkib5fkmhls4fkdhx0vb2cyka8-step-1’
building path(s) ‘/nix/store/1kiwa6m11d0apxfjbwpqq3vl6jbv3sdx-step-3’
building path(s) ‘/nix/store/9hv1jcrglyx8x6xa64pnds6vzcp35zl5-jq-cmd’
I am step 3
I am step 1
2
$ ./go.sh
2
```

This time the scripts are cached, but we execute them both together in a normal
pipe. The overall result of the "runJq" call is still cached though. This is how
you'd run non-bash scripts too: by using `writeFile` to save your code to disk,
and `runCommand` to invoke it with a bash one-liner. For example, if we want
`step4` to use Haskell we might do the following:

```
  runJq = runCommand "jq-cmd" { buildInputs = [ jq ghc ]; };

  ...

  hsScript = script: writeScript "hs-cmd" ''
    runhaskell "${writeScript "hs-script" script}"
  '';

  ...

  step4 = hsScript ''
    doTimes :: (Show a) => a -> String -> String
    doTimes str n = show (replicate (read n) str)

    hello = "hello world"

    main = interact (doTimes hello)
  '';
};

import (runJq ''"${step3}" | "${step4}" > "$out"'')
```

This reads the length given by jq, and writes out a list of that many "hello
world"s:

```
$ ./go.sh
building path(s) ‘/nix/store/2d7wrd78dk1ilj84adnyq8ddgzy6m2rr-hs-cmd’
building path(s) ‘/nix/store/haqcwssfbzbj5s4ampv322qbpll1gw1h-jq-cmd’
I am step 3
I am step 1
"[\"hello world\",\"hello world\"]"
```

Unfortunately, this can end up separating the code from its dependencies,
i.e. we needed to give `ghc` as a dependency to whichever script invokes `step4`
(via `runJq`), rather than being able to add it in `hsScript`. If we used the
original data-oriented approach, this wouldn't be an issue.

To mitigate this, [my Nix config](/git/nix-config) provides a `wrap` function
([documented here](/projects/nixos/useful_hacks.html)) which lets us write
scripts which "bake in" their dependencies:

```
myScript = wrap {
  name   = "my-script";
  paths  = [ bash ghc jq python ];
  vars   = { someEnvVar = "Arbitrary content"; };
  script = ''
    #!/usr/bin/env bash
    ...do something here with "$someEnvVar" set, and a PATH containing bash,
    ghc, jq and python...
  '';
};
```

Since I often use `wrap` to make scripts which I then expose via `bin/`, I've
also made a function `mkBin` which takes the same arguments as `wrap`, but
instead of just building a script, it will put that script into a `bin/`
subdirectory of its output, with the filename taken from `name` (e.g.
`bin/my-script` in this case).

Note that we can also write our scripts in standalone files, and reference them
from Nix by giving the path to the file, e.g. `./myPythonScript.py`. Since these
are just regular files, we can use syntax highlighting, etc. when editing them.
We also avoid having to care about escaping, but note that we can't use string
interpolation this way.

We also end up exposing the "raw" script (i.e. without its dependencies). If you
want to target non-Nix users, that can be useful; otherwise, you might want to
make it clear that it's an internal detail which shouldn't be run as-is (e.g.
you could put it in a subdirectory called `internal` or `src` or something).

## Coupling

It's also pretty easy to transfer data between the Nix language and the
processes we're invoking, using `readFile`, `readDir`, `fromJSON` and `toJSON`
inside a splice; although Nix doesn't support floats, so you might need to turn
them into strings first. This is useful for doing tricky transformations on
small amounts of data, which may be error-prone in bash, but where invoking a
full-blown language like Haskell or Python would be overkill. It can also be
useful for things like assertions.

Note that my preferred way of accessing the result of a derivation from within
Nix is to use `import` rather than `readFile`. One advantage is that we're
guaranteed that Nix will build the result as needed, rather than saying 'no such
file' (e.g. if we'ren ot in read/write mode).

Another is that Nix will not propagate dependencies of that derivation into the
value that it reads; for example if we have a derivation `foo` which depends on
Python and results in a file containing the string `"hello"`, then `import foo`
will give the string `hello` *without* tracking the Python dependency. This can
be important if we have a fast-moving dependency which might cause lots of
rebuilding, but doesn't actually affect the contents of its output: we can
generate some intermediate value using this dependency, which will get rebuilt a
lot, but by importing that value for use in the subsequent steps, those steps
won't need to be rebuilt as long as that intermediate value doesn't change.

Of course, the tricky part is writing data into Nix files such that they can be
imported in the first place. Nix's syntax is quite simple, but it can sometimes
help to use `fromJSON`, etc. to make life easier.
