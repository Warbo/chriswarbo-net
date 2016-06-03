---
title: Scripting with Nix
---
I've not played with node.js since the very early days, and never really used NPM. However, I do see the need for modular, composable shell scripts.

Personally, I've been using Nix in a similar way, since it also has nice features like caching, laziness, splicing into indented strings, dependency management, etc. For example, if you have a Nix expression stored in "my-script.nix" you can use the following (e.g. in "my-script.sh") to invoke it:

    nix-instantiate --read-write-mode --show-trace --eval -E 'import ./my-script.nix'

The `--eval` tells Nix to evaluate an expression, rather than build a package. `-E` is the expression to evaluate (in this case, importing our "script" file). `--read-write-mode` allows the script to add things to the Nix store. `--show-trace` is to aid debugging.


Well, this is basically two features I condensed into one phrase. I didn't mean context-sensitive splicing (e.g. splicing together Python code, whilst adhering to the off-side rule).

Firstly, indented strings mean that long strings (such as scripts) can be embedded inside other expressions quite naturally. For example:

```
runCommand "foo"
           { buildInputs = [ python imagemagick ]; }
           ''
             I am an indented string
             I will be executed as a bash script, with the following dependencies available:
              - python
              - imagemagick

             Since these lines, and those above beginning with "I", have the least indentation,
             they will appear flush to the left. The "list" above will hence be indented by 1
             space.
           ''
```

Secondly, splicing allows Nix expressions to be embedded inside strings. A splice begins with "${" and ends with "}". The expression should either evaluate to a string, which is inserted as-is, or a "derivation" (e.g. a package), which gets "instantiated" (i.e. installed) and its installation directory is inserted into the resulting string. Splices can be nested too.

For example, instead of giving "python" as a dependency in the buildInputs, we could splice the full path into a string, e.g.

```
''
  "${python}/bin/python" my_script.py
''
```


Although this is probably a bad idea, since there may be transitive dependencies, etc. missing when the script gets executed.

If we want to build up a result incrementally, with each step getting cached, we can use "runCommand", and write the results to "$out". For example:

```
with import <nixpkgs> {};
with builtins;

let

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

in readFile step3
```

When run, this gives the following:

```
$ ./go.sh
building path(s) ‘/nix/store/5ks08zbvmgzbhg9kr0k4g75nf2ymsqsr-jq-cmd’
I am step 1
building path(s) ‘/nix/store/v1svcqq6cmi4xc9650qz9w2x177w4pfr-jq-cmd’
I am step 3
"2\n"
$ ./go.sh
"2\n"
```

The results are cached, and will be re-used as long as the commands aren't edited, and their dependencies don't change (e.g. if a newer version of jq is available, they'll be re-run with that version).

In this case, each "step" represents the data, which is common in lazy languages. Alternatively, we can use "writeScript" to write more 'traditional' process-oriented scripts:

```
with import <nixpkgs> {};
with builtins;

let

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

in readFile (runJq ''
     "${step3}" > "$out"
   '')
```

Of course, we need something to invoke these scripts, which is why I used "runJq" in the final expression. When run, we get:

```
$ ./go.sh
building path(s) ‘/nix/store/fnw68cmkib5fkmhls4fkdhx0vb2cyka8-step-1’
building path(s) ‘/nix/store/1kiwa6m11d0apxfjbwpqq3vl6jbv3sdx-step-3’
building path(s) ‘/nix/store/9hv1jcrglyx8x6xa64pnds6vzcp35zl5-jq-cmd’
I am step 3
I am step 1
"2\n"
$ ./go.sh
"2\n"
```

This time the scripts are cached, but we execute them both together in a normal pipe. The overall result of the "runJq" call is still cached though. This is how you'd run non-bash scripts too: by using "writeFile" to save your code to disk, and "runCommand" to invoke it with a bash one-liner. For example, if we want "step4" to use Haskell we might do the following:

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

in readFile (runJq ''
     "${step3}" | "${step4}" > "$out"
   '')
```

This reads the length given by jq, and writes out a list of that many "hello world"s:

```
$ ./go.sh
building path(s) ‘/nix/store/2d7wrd78dk1ilj84adnyq8ddgzy6m2rr-hs-cmd’
building path(s) ‘/nix/store/haqcwssfbzbj5s4ampv322qbpll1gw1h-jq-cmd’
I am step 3
I am step 1
"[\"hello world\",\"hello world\"]"
```

Unfortunately, this can end up separating the code from its dependencies, i.e. we needed to give "ghc" as a dependency to whichever script invokes "step4" (via "runJq"), rather than being able to add it in "hsScript". If we used the original data-oriented approach, this wouldn't be an issue.

It's also pretty easy to transfer data between the Nix language and the processes we're invoking, using "readFile" and "builtins.fromJSON", or "builtins.toJSON" inside a splice; although Nix doesn't support floats, so you might need to turn them into strings first. This is useful for doing tricky transformations on small amounts of data, which may be error-prone in bash, but where invoking a full-blown language like Haskell or Python would be overkill. It can also be useful for things like assertions.
