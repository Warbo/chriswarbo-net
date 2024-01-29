---
title: Nix from the bottom up
---

[Click here for more of my Nix/NixOS pages](./.)

This page describes what [Nix](https://nixos.org/manual/nix/stable/introduction)
fundamentally *is*, at its core; what it is actually doing when it runs; and why
it makes the choices that it does. This won't tell you what commands you need to
run, or which of the Nixpkgs override functions you should call to swap out that
broken dependency you've found; but hopefully it will give you a clearer picture
of what's happening, what's possible, and maybe steer you in the right
direction.

To begin, I like to say that the *point* of Nix is the following:

**GET SPECIFIED OUTPUTS**

The key, of course, is to understand what I mean by these words. That's what the
rest of this page is about!

## What is an output? ##

This is pretty easy: an "output" is just a file or folder. Nix puts all outputs
in the "Nix store", which is usually the folder `/nix/store`. For example, my
phone has an output with the path
`/nix/store/sqm5miynrjc5sw0zbnkvr9281xp043iw-firefox-120.0`: it is a folder
which happens to contain a copy of the Firefox browser (containing an executable
at `bin/firefox`, an icon image at `share/icons/hicolor/64x64/apps/firefox.png`,
etc.).

## How do we specify an output? ##

There are two ways to specify an output: using its hash, or using a derivation.

### Outputs specified by hash ###

These use two pieces of information: a name (for human readability), and a hash
of the file/folder contents. These are combined into the output's path, using
the pattern `<store>/<hash>-<name>`; e.g.
`/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh`. This is a neat
trick: paths are the standard way to refer to files/folders, so putting hashes
in output paths ensures every reference specifies the exact content that it's
referring to!

There are two downsides to this approach:

 - There is no feasible way to invert a hash, so there's no way for Nix to
   create such outputs if they are not found.
 - The only way to know a hash is to calculate it from existing data (or have
   someone else do that and tell you the result), so this approach cannot
   specify *new* outputs that don't yet exist.

This approach to specifying outputs is mostly useful as a cache/database of
things we already have; when we want a "content address" for files and folders.
For example, if we're going to compile multiple copies of some source code then
giving identical versions the same path avoids redundant compilation; whilst
those with differing content should be kept at distinct paths so we can compile
each individually.

### Outputs specified by derivation ###

A "derivation" is a text file in the Nix store, following the same
`<store>/<hash>-<name>` pattern as the previous approach, but whose name always
ends in `.drv`. These files must contain certain fields, including a "system"
(such as `x86_64-linux`), the path of an executable (like `/bin/sh`), a list of
string arguments to give to that executable (say,
`["-c", "echo RUNNING >&2 && echo $message > $out"]`) and a key/value map of
strings to use as environment variables (e.g. `("message", "hello")`). Together
these fields are essentially specifying an `exec` syscall; and that's exactly
how Nix will use them, to *create* outputs that don't already exist. This
overcomes the first problem identified with the hash-only approach.

Derivation files must specify at least one output, with each having a name and a
path. Like with the previous approach, these paths have the form
`<store>/<hash>-<name>`, although this time the hash is calculated from the
fields of the derivation file: that way, we don't have to know the *contents* of
an output ahead of time, only *a command to create it*; hence overcoming the
second problem.

Derivations may optionally specify "inputs", which are just references to other
outputs, specified either by a path (which contains a hash), or as a named
output of some other derivation (specified by the path to its `.drv` file, which
again contains a hash). This way, `.drv` files can refer to each other to form a
directed acyclic graph; and since each path contains a hash, these dependencies
form a Merkle tree which completely specifies the entire dependency graph of
each derivation. This is similar to how the ID of a git commit depends on its
content, and that content includes the commit IDs of its parents; hence giving
every git commit a tamper-proof specification of its entire history.

## How do we get an output? ##

Now we know how outputs are specified, it should be pretty clear how Nix can
get them (I already said, it can run the specified executable!). The algorithm
Nix follows is sketched below, and is known as "building" or "realising" an
output.

All outputs have a filesystem path, either specified directly (when we're
relying on the hash that occurs in the path) or written in a specified `.drv`
file. The first thing Nix will do is check if the specified output's path
already exists on the filesystem: if it does, no further action is needed and
we're finished!

If the specified output *doesn't* already exist on our system, Nix can query
*other* systems to see if they have it. These are called "binary caches" or
"remote stores" (depending on how they're set up), and are usually part of our
system config (e.g. a default Nix installation will use `cache.nixos.org` as a
binary cache). If one of these returns a hit for the specified output path, Nix
will copy its contents to the local filesystem, and is then finished. This lets
us decouple *specification* from *implementation*: for example, the Nixpkgs
project tends to specify derivations with large dependency graphs, with inputs
being as detailed and fine-grained as the particular patches to apply to the
source of the GCC compiler that is used to build the shell that is used to run
the tests of the library that... and so on! However, we don't need to *run* all
of those steps, since Nix will just fetch the output we asked for from a cache!

If there were no cache hits, and the output is only specified by its hash, then
Nix must abort at this point, since there's no way to re-create the desired
file/folder given only its hash.

The interesting case happens when outputs of a *derivation* are not found, since
we must run the derivation's command. Before that, Nix must get all of the
derivation's inputs (each of which, remember, is some other output): that's
right, this procedure is recursive!

Once all of the inputs exist, Nix will run the derivation's executable with the
given arguments and environment (this usually happens in a sandbox, depending on
the system configuration, but *that's not important*; the idea of Nix is
**orthogonal** to the idea of containers, although they complement each other
well)!

When the executable has finished, Nix will check whether all of the output paths
now exist on the filesystem: if not, it aborts with an error message. Otherwise,
Nix is now finished.

**Note:** We do not need to trust remote systems when copying outputs specified
by hash (since Nix will verify the hash after copying). However, copying the
output of a derivation requires some trust (either in the remote system itself,
or a key that's signed its contents). This is because verifying the contents
would require us to run the command ourselves, and compare the result; yet the
entire point of a cache is to avoid having to run things ourselves!

## Examples ##

### Specifying with a hash ###

We can use `nix-store --add` to put an existing file/folder into our Nix store.
This returns its path, which we can use to access it like any other file:

```
$ echo 'hello' > greeting.txt
$ nix-store --add greeting.txt
/nix/store/5cil4z0s59ii1splw7bhxf230bfdxfq5-greeting.txt
$ rm greeting.txt
$ cat /nix/store/5cil4z0s59ii1splw7bhxf230bfdxfq5-greeting.txt
hello
```

Since the path contains a hash of the content, adding the same content (with the
same name `greeting.txt`) will always give the same path; yet different content
will give an utterly different path:

```
$ echo 'goodbye' > greeting.txt
$ nix-store --add greeting.txt
/nix/store/q4x6d9w3x7wx1d2rx18n28sfbss4b9nw-greeting.txt
$ cat /nix/store/q4x6d9w3x7wx1d2rx18n28sfbss4b9nw-greeting.txt
goodbye
$ echo 'hello' > greeting.txt
$ nix-store --add greeting.txt
/nix/store/5cil4z0s59ii1splw7bhxf230bfdxfq5-greeting.txt
$ cat /nix/store/5cil4z0s59ii1splw7bhxf230bfdxfq5-greeting.txt
hello
```

### Specifying a derivation ###

Derivations are a great idea, but they're pretty tedious to write by hand
(especially calculating all the required hashes). Instead, Nix comes with a
simple scripting language (the "Nix expression language") for generating `.drv`
files for us. Here's a Nix expression for a simple derivation, which I'll save
in a file called `example.nix` (note that this isn't an output or `.drv` file,
so it doesn't need to live in the Nix store):

```nix
derivation {
  name = "myName";
  system = "aarch64-linux";
  builder = "/bin/sh";
  args = [ "-c" "echo RUNNING >&2 && echo $message > $out" ];
  message = "hello";
}
```

This expression is calling a built-in function called `derivation` with a single
argument: a "set" (think JSON object) containing a bunch of "attributes" (keys).
Note that we're not specifying any output names, so the `derivation` function
will default to using a single output called `out`, which will also appear as an
environment variable (hence why our Bash snippet is writing to `$out`). If
you're not on an `aarch64-linux` machine, you can replace that string with the
expression `builtins.currentSystem` instead!

We can evaluate this Nix expression to produce a `.drv` file by using the
`nix-instantiate` command:

```
$ nix-instantiate example.nix
warning: you did not specify '--add-root'; the result might be removed by the
garbage collector
/nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv
$ cat /nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv
Derive([("out","/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName","","")],[],
[],"aarch64-linux","/bin/sh",["-c","echo RUNNING >&2 && echo $message > $out"],
[("builder","/bin/sh"),("message","hello"),("name","myName"),("out",
"/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName"),("system","aarch64-linux")
])
```

Unfortunately Nix was created before the ubiquity of JSON, so its `.drv` format
may look unfamiliar. Thankfully the `nix derivation show` command will translate
it for us (albeit with a warning message, which we can ignore!):

```
$ nix derivation show /nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv
warning: The interpretation of store paths arguments ending in `.drv` recently
changed. If this command is now failing try again with
'/nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv^*'
{
  "/nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv": {
    "args": [
      "-c",
      "echo RUNNING >&2 && echo $message > $out"
    ],
    "builder": "/bin/sh",
    "env": {
      "builder": "/bin/sh",
      "message": "hello",
      "name": "myName",
      "out": "/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName",
      "system": "aarch64-linux"
    },
    "inputDrvs": {},
    "inputSrcs": [],
    "name": "myName",
    "outputs": {
      "out": {
        "path": "/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName"
      }
    },
    "system": "aarch64-linux"
  }
}
```

Hopefully you can see how each part of a derivation I described above appears in
this file (although we've not specified any inputs, for simplicity). We can
"realise" the outputs of this derivation using the `nix-store --realise`
command:

```
$ nix-store --realise /nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv
this derivation will be built:
  /nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv
building '/nix/store/i762zk23lrfsz8fjfd4lbjh48073hmlh-myName.drv'...
RUNNING
warning: you did not specify '--add-root'; the result might be removed by the
garbage collector
/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName
$ cat /nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName
hello
```

Notice that the text `RUNNING` appears, which shows that the `/bin/sh` command
was indeed executed; and sure enough the output contains the text `hello` which
we specified for the `message` environment variable.

Whilst `nix-instantiate` and `nix-store --realise` are useful to make it clear
what's going on (generating a `.drv` then getting its outputs), in practice we
can just use the simpler `nix-build` command:

```
$ nix-build example.nix
/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName
```

Note that this time we did not get a `RUNNING` message, or anything mentioning
"building" or "garbage collectors". That's because the output already exists, so
Nix did not need to run anything!

We can make life *even easier* by renaming our file to `default.nix`: that way,
we can run `nix-build` without having to specify any filename!

```
$ mv example.nix default.nix
$ nix-build
/nix/store/zcgax4c4wfvby6p06dwjl8cc4dvkvypr-myName
```

#### Writing a `.drv` file manually ####

We can remove a couple more abstraction layers from the above example, and avoid
the Nix expression language entirely, if we don't mind some trial-and-error to
calculate the hashes.

Since `.drv` files act like outputs specified by hash, we can create them
"manually" using `nix-store --add`. For example, we can create a new `.drv` file
by copying the contents of the above example (and changing the commands, to
ensure it won't get the same hash!); we don't know what the output path should
be yet, so I've used all-zeroes for its hash:

```sh
$ echo 'Derive([("out","/nix/store/00000000000000000000000000000000-myName","","")],[],[],"aarch64-linux","/bin/sh",["-c","echo ABC > $out"],[("out","/nix/store/00000000000000000000000000000000-myName")])' > myName.drv
$ nix-store --add myName.drv
error: derivation '/nix/store/97ip2v92rqjs83s2xxf23wq011ln9kjq-myName.drv' has incorrect output '/nix/store/00000000000000000000000000000000-myName', should be '/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName'
```

Nix has spotted that this `.drv` file is invalid, due to the dummy hash we used;
and thankfully has provided the correct value (based on a hash of the other
fields). We can copy/paste that path into our file contents, to get a working
Nix derivation:

```sh
$ echo 'Derive([("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName","","")],[],[],"aarch64-linux","/bin/sh",["-c","echo ABC > $out"],[("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName")])' > myName.drv
$ nix-store --add myName.drv
/nix/store/mcjp1bawqh0my5pma8lbpm4mdpldw9sd-myName.drv
$ nix-store --realise /nix/store/mcjp1bawqh0my5pma8lbpm4mdpldw9sd-myName.drv
this derivation will be built:
  /nix/store/mcjp1bawqh0my5pma8lbpm4mdpldw9sd-myName.drv
building '/nix/store/mcjp1bawqh0my5pma8lbpm4mdpldw9sd-myName.drv'...
warning: you did not specify '--add-root'; the result might be removed by the garbage collector
/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName
$ cat /nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName
ABC
```

This is about as low-level as we can go with Nix; and whilst I don't advise such
commands for day-to-day use, it's at least clear that *in principle* we could
do all of this ourselves, and perhaps make some different choices (e.g. like the
[Guix project](https://guix.gnu.org)).

#### Demystifying the `derivation` function ####

We just jumped down two steps of abstraction, from using the Nix language's
`derivation` function to writing `.drv` files manually. It can also be
enlightening to make a derivation *with* the Nix language, but *without* the
`derivation` function; to demystify what's going on.

We'll start in the `nix repl`, which is useful for experimenting. First note
that, in the Nix language, a derivation is just an attribute set (~JSON object)
with an attribute called `type` set to the string `"derivation"`:

```nix
$ nix repl
Welcome to Nix 2.17.1. Type :? for help.

nix-repl>  { foo = "some ordinary attribute set"; }
{ foo = "some ordinary attribute set"; }

nix-repl> { type = "derivation"; }
«derivation ???»
```

Of course, this is missing all of the required fields of a derivation; but we
can figure out what they need to be by using the `:b` (build) command of
`nix repl`, and following the error messages:

```nix
nix-repl> :b { type = "derivation"; }
error: derivation name missing

nix-repl> :b { type = "derivation"; name = "foo"; }
error: expression did not evaluate to a valid derivation (no 'drvPath' attribute)

nix-repl> :b { type = "derivation"; name = "foo"; drvPath = "bar"; }
error:
       … while evaluating the 'drvPath' attribute of a derivation

         at «string»:1:38:

            1| { type = "derivation"; name = "foo"; drvPath = "bar"; }
             |                                      ^

       error: path 'bar' is not in the Nix store

nix-repl> :b {
  type = "derivation";
  name = "foo";
  drvPath = "/nix/store/00000000000000000000000000000000-foo.drv";
}
error: expression evaluated to invalid derivation '/nix/store/00000000000000000000000000000000-foo.drv'
```

This `invalid derivation` error is due to that path not existing. We can fix
this by using the `builtins.toFile` function to create a file in the Nix store:
it acts in a similar way to the `nix-store --add` command, returning the path of
the file:

```nix
nix-repl> :b {
  type = "derivation";
  name = "foo";
  drvPath = builtins.toFile "foo.drv" "";
}
error:
       … while calling the 'toFile' builtin

         at «string»:1:48:

            1| { type = "derivation"; name = "foo"; drvPath = builtins.toFile "foo.drv" ""; }
             |                                                ^

       error: error parsing derivation '/nix/store/2pzx8pi92qfrbnm3wmcpmvv0cs1517fv-foo.drv': error: expected string 'Derive(['
```

We can see that Nix is now attempting to parse our `.drv` file, so we'll need to
write its contents like we did in the "manual" approach (again, changing the
command to ensure we get a different hash). Note that we can write strings using
`''two apostrophes''`, to avoid having to `\`-escape all the quotation marks:

```nix
nix-repl> :b {
  type = "derivation";
  name = "foo";
  drvPath = builtins.toFile "foo.drv" ''Derive([("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName","","")],[],[],"aarch64-linux","/bin/sh",["-c","echo 'I AM FOO' > $out"],[("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName")])'';
}
error:
       … while calling the 'toFile' builtin

         at «string»:4:13:

            3|   name = "foo";
            4|   drvPath = builtins.toFile "foo.drv" ''Derive([("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName","","")],[],[],"aarch64-linux","/bin/sh",["-c","echo 'I AM FOO' > $out"],[("out","/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName")])'';
             |             ^
            5| }

       error: derivation '/nix/store/q43b7y9pcni5s8243zi83ljccw45xba7-foo.drv' has incorrect output '/nix/store/r853x2wai95pp9c711j10srq10xryn8a-myName', should be '/nix/store/znfqqq66463n8wkh3qlp5c2pfz91dhbs-foo'
```

Just like the "manual" example, Nix is telling us that the output path has the
wrong hash (and name, in this case!). Like before, it gives us the correct
value, which we can copy into our string:

```nix
nix-repl> :b {
  type = "derivation";
  name = "foo";
  drvPath = builtins.toFile "foo.drv" ''Derive([("out","/nix/store/znfqqq66463n8wkh3qlp5c2pfz91dhbs-foo","","")],[],[],"aarch64-linux","/bin/sh",["-c","echo 'I AM FOO' > $out"],[("out","/nix/store/znfqqq66463n8wkh3qlp5c2pfz91dhbs-foo")])'';
}

This derivation produced the following outputs:
  out -> /nix/store/znfqqq66463n8wkh3qlp5c2pfz91dhbs-foo

nix-repl> :q
$ cat /nix/store/znfqqq66463n8wkh3qlp5c2pfz91dhbs-foo
I AM FOO
```

Hence the `derivation` function isn't doing anything special: it's just using
its arguments to write an appropriate string to an appropriately named `.drv`
file, and returning an attribute set with the appropriate attributes filled in.
In principle we could have even calculated the hashes ourselves (using the
`builtins.hashString` and `builtins.hashFile` functions), but that's a bit
too tedious, even for me!

## Going forward ##

I've described the most basic, fundamental workings of Nix, which hopefully
gives you a solid understanding to build upon. However, not only is there much
more to the Nix tool itself, but there's now an entire "ecosystem" built around
it, such as Nixpkgs, NixOS, NixOps, Cachix, etc. New features are pushing what's
possible, and simplifying what's useful.

If you want to start playing around, I highly recommend using the `nix repl`
command, looking through
[the Nix manual](https://nixos.org/manual/nix/stable/language/), and following
some of the excellent documentation and resources people have put online!
