---
title: Nixlang glossary
---

**For a primer on what Nix is, what a "derivation" is, and how to use Nix
*without* Nixlang, see [Nix from the bottom up](bottom_up.html)**

Nix commands can be given "expressions" which *calculate* derivations, using a
simple (but Turing-complete) programming language called the Nix Expression
Language. This language is often just called "Nix", but that can cause ambiguity
so I either call it the "Nix Expression Language", or abbreviate it to
"Nixlang". Nix commands can read Nixlang expressions either written straight on
the commandline, or in "`.nix` files".

Modern eyes would see Nixlang as an extension of JSON; but for historical
reasons it has slightly different syntax and terminology. This page gives a
quick glossary, to help understand its syntax and semantics. Note that it is
incomplete, and biased towards my coding style. I may add to it occasionally,
but for a more thorough reference, see
[its chapter in the Nix manual](https://nixos.org/manual/nix/stable/language).

### Architecture ###

Most Nixlang code is written in functions, and those functions tend to be
organised in (nested) attribute sets. The phrase "repository" can refer to a
directory of `.nix` files (usually tracked by git) which define such attrsets,
although that's just a convention; Nix has no built-in concept of "repositories"
in the same way as, say, APT or Maven.

The most important repository of Nixlang code is called
[Nixpkgs](https://github.com/nixos/nixpkgs), which defines many thousands of
derivations, as well as functions for creating new ones. Most Nix projects use
at least *some* of Nixpkgs. A default installation of Nix will include a copy of
Nixpkgs (using a Nix feature called "channels").

#### Personal recommendations ####

I recommend *avoiding* channels: they lead to under-specified derivations, harm
reproducibility, and can lead to the sort of "works on my machine" nonsense that
Nix is supposed to avoid.

Instead, external references (like Nixpkgs, or other git repos, etc.) should
always be specified by their exact, cryptographic hash (whether as a git commit
ID, or a URL with accompanying SHA256, etc.). This practice is sometimes
referred to as "pinning" (borrowing terminology from e.g. `apt-get`, although
it's not a dedicated feature in Nix; just a way of using it). Some people use
tools like [`niv`](https://nix.dev/guides/recipes/dependency-management.html) to
manage these references; but I find it easier to avoid such indirection and just
write plain `.nix` files. If you want to automate their updates, you can use
tools like
[`update-nix-fetchgit`](https://hackage.haskell.org/package/update-nix-fetchgit).

I'd recommend projects only put a few `.nix` files at their top-level, e.g.
`default.nix` and maybe `shell.nix`; any more extensive definitions can be kept
out of the way in a `nix/` folder.

If you're using Nix inside an organisation, or across many of your personal
projects, I'd suggest making a git repo (or a directory inside your monorepo, if
you do that) for storing generally-useful Nix code. I tend to give this a name
like "nix-helpers". Individual projects can then "pin" a particular revision of
that nix-helpers repo, say in a file like `nix/nix-helpers.nix`. I especially
like to use such a nix-helpers repo to "pin" some chosen revision of common
external dependencies (like Nixpkgs, or even internal libraries); that way, many
projects will only need to "pin" a single dependency (the nix-helpers repo), and
it's easier to maintain consistency across projects (compared to e.g. the "lock
files" found in legacy systems).

### Functions ###

Functions always have exactly one argument, which is separated from their return
value by writing a `:`, like `myArg: myArg + myArg`{.nix}. Function calls use a
space, like `myFunction 123`{.nix} (which will return `246`{.nix}).

Functions are also referred to as "lambdas". When generating error messages, Nix
will refer to a function using the name of the variable that was used to access
it (if any); functions which weren't accessed via a variable will be called
`anonymous lambda`. For example:

```nix
nix-repl> with { double = myArg: myArg + myArg; }; double (1 / 0)
error:
       … from call site

         at «string»:1:42:

            1| with { double = myArg: myArg + myArg; }; double (1 / 0)
             |                                          ^

       … while calling 'double'

         at «string»:1:17:

            1| with { double = myArg: myArg + myArg; }; double (1 / 0)
             |                 ^

       error: division by zero

       at «none»:0: (source not available)

nix-repl> (myArg: myArg + myArg) (1 / 0)
(myArg: myArg + myArg) (1 / 0)
error:
       … from call site

         at «string»:1:1:

            1| (myArg: myArg + myArg) (1 / 0)
             | ^

       … while calling anonymous lambda

         at «string»:1:2:

            1| (myArg: myArg + myArg) (1 / 0)
             |  ^

       error: division by zero

       at «none»:0: (source not available)
```

Multi-argument functions can be simulated either by **currying**, like
`x: y: x + y`; or **uncurrying**, like `args: args.x + args.y`{.nix}. Uncurried
functions can be written with a nicer syntax, like `{x, y}: x + y`{.nix}, but
keep in mind that this is still *one* argument (an attrset containing two
attributes). This syntax also allows *default arguments* to be specified, e.g.
`{x, y ? 42}: x + y`{.nix}. In this example, if there is no `y`{.nix} attribute
then it defaults to using `y = 42;`; e.g.
`({x, y ? 42}: x + y) { x = 2; }`{.nix} returns `44`{.nix}. Such argument sets
are automatically recursive (like an attrset with the `rec` keyword), for
example:

```nix
{ name
, value
, format   ? "json"
, filename ? "${name}.${format}"
, content  ? (if format == "string" then value else builtins.toJSON value)
}:
builtins.toFile filename content
```

Any attrset defining the names `__functor` and `__functionArgs` can be called
as if it were a function; with the advantage that we can also put other
attributes in them too.

#### Personal recommendations ####

Functions with default arguments are an incredibly simple and effective way to
specify derivations that are easily overridable (just call the function with
some different arguments). You can call it "dependency injection", if it makes
you feel better. For example, many of my projects have a `default.nix` file that
looks something like:

```nix
{ name        ? "my-project"
, src         ? ./.
, nix-helpers ? import ./nix/nix-helpers.nix
, nixpkgs     ? nix-helpers.nixpkgs
, dependency1 ? nix-helpers.pinned.dependency1
}:
nixpkgs.someHelperFunction {
  inherit dependency1 name src;
}
```

Commands like `nix-build` will automatically call such functions, giving them an
empty attrset `{}`{.nix} so all the default values will be used; yet we're free
to can swap
out all of the pieces if we

### Attribute sets ###

"Attribute sets", AKA "attrsets" or "sets" are basically JSON objects. They
associate names (AKA attributes) with values, like
`{ x = 42; y = true; "1.2" = {}; }`{.nix}. Notice the `=` (rather than JSON's
`:`) and the trailing `;` characters. Names can be looked-up like `foo.x`{.nix}
(or `foo."1.2"`{.nix} for awkward names).

The `inherit` keyword lets us append variables to an attrset, or copy attributes
from one attrset to another, as long as their name stays the same. For example
`{ inherit x y z; }`{.nix} is the same as `{ x = x; y = y; z = z; }`{.nix}
and `{ inherit (foo) a b; }`{.nix} is the same as
`{ a = foo.a; b = foo.b; }`{.nix} (note that the parentheses are *required* when
doing this!)

`builtins`{.nix} is a variable bound by default to an attrset of useful
functions and constants, like `builtins.fetchGit`{.nix} and
`builtins.storeDir`{.nix}.

Nix will treat any attrset containing `type = "derivation";`{.nix} as a
derivation; although more attributes must also be present, like `name`,
`drvPath`, etc. for it to actually *work* as a derivation!

The keyword `with` lets us reference a set's attributes as variables, e.g.
`with { x = 2; }; x + x`{.nix} evaluates to `4`{.nix}. There is an annoying
quirk in Nix's semantics, which might catch you out: variables bound by `with`
do not shadow variables bound by function arguments. For example:
`(x: with { x = 123; }; x + x) 7`{.nix} evaluates to `14`{.nix}, even though
the binding `x = 123;`{.nix} is "closer" to the sum.

The keyword `rec` allows the attributes of a set to reference *each other* as
variables (similar to `let` versus `letrec` in Lisp/Scheme). For example
`(rec { x = 2; y = x + x; }).y`{.nix} evaluates to `4`{.nix}. Note that using
`inherit` inside such `rec`ursive attrsets will skip the self-reference; i.e. in
`rec { inherit x; }`{.nix} the value of the `x` attribute will come from the
surrounding scope, unlike `rec { x = x; }`{.nix} (which is a circular reference
and hence an error).

#### Personal recommendations ####

There is an alternative to `with`, which is to use the `let` and `in` keywords.
However, [I dislike
them](https://github.com/NixOS/nix/issues/1361#issuecomment-390420439).

When using `with`, it's best to explicitly state which names we're binding. For
example, `with { inherit (foo) python }; python.numpy`{.nix} is preferable to
`with foo; python.numpy`{.nix}, since in the latter it's ambiguous where the
`python`{.nix} variable is coming from; or indeed whether it's bound at all! If
`foo`{.nix} happens to contain an attribute called `python`{.nix}, then both of
these expressions are equivalent to `foo.python.numpy`{.nix}; yet if `foo`{.nix}
does not contain a `python`{.nix} attribute, the former will give
`error: attribute 'python' missing` whilst the latter's behaviour depends
entirely on whatever the surrounding code happens to be doing.

### Paths ###

Paths are distinct from strings. They must be written with at least one `/`,
e.g. `./foo.txt`{.nix} is the path to an entry `foo.txt` in the current
directory (AKA `.`; with `..` being the parent directory); whilst
`foo.txt`{.nix} is not a path (since it's not got a `/`), and is instead
referencing the `txt` attribute of a variable called `foo`.

All paths are absolute, so `./foo.txt`{.nix} will evaluate to, say,
`/home/me/foo.txt` (depending on what the current directory is). If our Nix
expression is written directly on a commandline, like
`nix-build -E './foo.txt'`{.sh} then the current directory is the same as our
shell's. If the expression instead appears in a `.nix` file then it is *always*
resolved relative to the directory containing that file (regardless of what
command we're running, or whether it's being imported by some other file in a
different directory, etc.). Note that the latter may behave unexpectedly when
using symbolic links!

There are two ways to convert a path to a string, which behave differently:

 - `builtins.toString ./foo.txt`{.nix} will return a string of that path,
   e.g. like `"/home/me/foo.txt"`{.nix}
 - Splicing a path into a string (see Strings below) will *add it to the Nix
   store* and return its Nix store path. This is like using the
   `nix-store --add`{.sh} command. For example `"It is ${./foo.txt}"`{.nix}
   evaluates to something like
   `"It is /nix/store/iazrg11viq5kxydw8n52bza0phzixk0f-foo.txt"`{.nix} (with the
   hash depending on that file/directory's contents; or Nix will abort with
   `error: getting status of '/home/me/foo.txt': No such file or directory`
   if it doesn't exist!)

We can append a string to a path without having to convert it. For example,
`./foo.txt`{.nix} is equivalent to `./. + "/foo.txt"`{.nix} (note the leading
`/` on the latter); which is useful for building paths out of variables, e.g.
`./json-files + ("/" + myString + ".json")`{.nix}. We can use this to convert a
string-containing-a-path into a path, by appending it to `/.`, e.g.
`"/home/me"`{.nix} is a string value, whilst `/. + "/home/me"`{.nix} is a path
value.

An attrset containing an attribute called `outPath` can be treated like a path,
at least when converting to a string. This makes it simpler to treat derivations
(which, remember, are represented as attrsets) as if they were paths; since it's
common for a derivation's output path to be stored in the `outPath` attribute.
For example:

 - `builtins.toString { outPath = "hi"; }`{.nix} evaluates to `"hi"`{.nix}
 - `builtins.toString { outPath = /etc/profile; }`{.nix} evaluates to
   `"/etc/profile"`{.nix}
 - `"${{ outPath = "ho"; }}"`{.nix} evaluates to `"hi"`{.nix}
 - `"${{ outPath = /etc/profile; }}"`{.nix} evaluates to
   `"/nix/store/bjkf9xibayibcf26dm00h4045anka6hh-profile"`{.nix}

#### Personal recommendations ####

Always be aware of whether a value is a path or a string-containing-a-path. When
we have a path value, keep in mind whether it will be pointing into the Nix
store, or to some arbitrary filesystem location. When using paths in strings,
take a second to consider whether it's better to appear verbatim (e.g. by using
`builtins.toString`{.nix}) or whether it's better to have it added to the Nix
store (e.g. by splicing it into place).

Nix can treat paths as if they're strings, but take care when relying on such
implicit conversion; which usually acts like `builtins.toString`. This can cause
random filesystem paths to appear, when we might have expected their contents to
be added to the Nix store instead.

### Strings ###

Strings can either be written in `"double quotes"`{.nix} or
`''two apostrophes''`{.nix}. The latter is convenient if our string contains
lots of double quotes, since it reduces the amount of \\-escaping needed.

Nix supports "string interpolation" (AKA splicing or quasiquoting), to insert
the contents of one string inside another; e.g.
`"This is ${builtins.currentSystem}"`{.nix} evaluates (on my PinePhone) to
the value `"This is aarch64-linux"`{.nix}. This syntax only applies to string
literals, so e.g. `"This is $" + "{builtins.currentSystem}"`{.nix} stays
verbatim, as if we'd escaped it like
`"This is \${builtins.currentSystem}"`{.nix}. Keep this in mind if you're
writing Nix string literals containing code, like Bash commands, which may need
to escape their own `${}` syntax!

Nixlang strings have a "context", which keeps track of outputs that are
referenced by that string (for example, paths that have been spliced into it).
When we specify a derivation (at least, using the normal helper functions;
rather than "manually" as in [Nix from the bottom up](bottom_up.html)!), the
context of the provided strings will be added as inputs to the derivation. This
is convenient to keep track of, and avoid having to repeat, the various
dependencies that our build commands may have. String context can be inspected
with the `builtins.getContext`{.nix} function; but keep in mind that it doesn't
always correspond to the exact string contents. For example:

 - Splicing a path into a string appends it to the Nix store *and* puts it in
   the resulting string's context, e.g.
   `builtins.getContext "${/etc/profile}"`{.nix} will contain an attribute like
   `"/nix/store/bjkf9xibayibcf26dm00h4045anka6hh-profile"`{.nix}
 - Context accumulates when strings are appended/spliced, e.g.
   `builtins.getContext ("${/etc/profile}" + "${/etc/bashrc}")`{.nix}
   will contain two attributes (like
   `"/nix/store/5n87wr3p63g647zgqqdxj6ip0yc3f1is-bashrc"`{.nix} and
   `"/nix/store/bjkf9xibayibcf26dm00h4045anka6hh-profile"`{.nix})
 - Taking a substring doesn't affect the context, so an expression like
    returns a
   string containing `hello` (since we're appending a substring of length `0`,
   AKA an empty string) but with a *context* containing that `profile` path!

#### Personal recommendations ####

We can make an *empty* string, which nevertheless has context, by first making a
string with the desired context, then taking a zero-length substring of it. We
can hence add context to an arbitrary string, by appending to such a zero-length
substring. For example `"hi" + builtins.substring 0 0 "${/etc/profile}"`{.nix}
evaluates to the string `"hi"`, whose context contains an attribute like
`"/nix/store/bjkf9xibayibcf26dm00h4045anka6hh-profile"`{.nix}.

When building up strings programatically, there are lots of great helpers in
Nixpkgs's `lib` attribute; e.g. the `lib.concatMapStringsSep`{.nix} function.

### Imports ###

The built-in function called `import` can be given a path, a
string-containing-a-path, or a derivation (relying on the `outPath`{.nix} trick
mentioned in the Paths section above). In the latter case, Nix will *build* the
derivation before attempting to import that path. This feature, called "`import`
from derivation", is incredibly powerful, since the expression that we import
can be generated using arbitrary commands. For example, we can write a
derivation that runs some given data through an off-the-shelf dependency solver,
and outputs a `.nix` file based on the result: passing that derivation to
`import` will cause the solver to run, and give us a value containing those
results (which we can, say, use to specify some version numbers in another
derivation)

### Fetchers ###

Nixlang provides several built-in "fetchers", which define outputs resulting
from HTTP downloads, Git checkouts, etc.; and Nixpkgs defines helper functions
to fetch from even more places. It can sometimes be confusing which one to use.
Here are some of the most common:

 - The `fetchurl` helper from Nixpkgs. This is basically the same as
   `builtins.fetchurl`{.nix}, but it's been around longer. It's also a pain to
   use since we first need to fetch Nixpkgs (causing a "bootstrapping" problem)!
   Consider it deprecated.
 - The `fetchgit` helper from Nixpkgs is likewise deprecated now that we have
   `builtins.fetchGit`{.nix}.
 - `builtins.fetchGit`{.nix} will fetch a specific commit of a Git repo (or
   `HEAD` if none is specified; though I recommend against this). This is
   *usually* what you want, for source code and such, since the result is a
   directory containing the repo contents. There are a couple of annoyances with
   `fetchGit`, caused by the way Git works: firstly it has to fetch a bunch of
   metadata, to find the requested commit; and secondly, if that commit is too
   far back in the history of the specified branch/ref, `fetchGit` may fail to
   find it. These are fine for "ordinary" repos, like that of an application or
   library; but they can cause problems for repos which build up many commits,
   like Nixpkgs.
 - `builtins.fetchTarball`{.nix} is useful to download *and extract* a file
   over HTTP; outputting a *directory* of the file's contents. Again, this is
   *usually* what you want, when fetching something like source code.
 - `builtins.fetchurl`{.nix} will *only* download a file; it won't try to
   extract it. This is useful for downloading things other than archives; e.g. a
   raw JSON file, or something.
 - Nixpkgs also provides a handy `fetchFromGitHub` function. It's just a
   wrapper around `fetchTarball`, but is nicer to use since we can give it
   arguments like `owner` and `rev`, and it takes care of constructing the
   corresponding GitHub URL.
