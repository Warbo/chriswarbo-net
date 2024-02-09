---
title: Renaming files inside tarballs
---

I recently needed to alter the contents of a `.tar.gz` file (a "tarball"), to
remove the first component of every path, e.g. turning an entry like
`foo/bar/baz.txt` into just `bar/baz.txt`. It took me an extra-ordinary
amount of searching to come across a solution; with most forum/QA responses
stating that it can't be done without either extracting all the files, or
resorting to overkill solutions like FUSE filesystems or Perl scripts.

I did come across a couple of tools which claim to do this, but couldn't get
them to work:

 - [tarcust](https://tracker.debian.org/pkg/tarcust), although its source code
   seems to have disappeared.
 - [tardy](https://tardy.sourceforge.net), although I couldn't compile it due to
   its dependency on `libexplain`, which is marked as broken in Nixpkgs.

Eventually I discovered this can be done using
[`bsdtar`](https://man.freebsd.org/cgi/man.cgi?query=bsdtar&format=html), which
is provided by `libarchive` in Nixpkgs.

To create a new file with *the contents of* an existing file (rather than just
including that file itself), just prefix the filename argument with an `@`.

For example, let's make a file called `foo`, and create an `original.tar.gz`
file containing it:

```sh
$ echo hello > foo
$ tar czf original.tar.gz foo
```

Let's list the contents of that tarball:

```sh
$ tar tf original.tar.gz
foo
```

Great, now let's delete the file `foo` to avoid any confusion:

```sh
$ rm foo
$ ls
original.tar.gz
```

Now let's use `bsdtar` to create a new `.tar.gz` file, using its `-s` argument
to transform the paths (for this example, we'll replace all `o` letters with the
digit `0`). If we give it `original.tar.gz` as an argument, we'll just get a
tarball-in-a-tarball:

```sh
$ bsdtar -s '/o/0/g' -c -z -f wrapped.tar.gz original.tar.gz
$ tar tf wrapped.tar.gz
0riginal.tar.gz
```

If we instead give it `@original.tar.gz`, it will use the contents of that
archive instead (i.e. the entry `foo`):

```sh
$ bsdtar -s '/o/0/g' -c -z -f unwrapped.tar.gz '@original.tar.gz'
$ tar tf unwrapped.tar.gz
f00
```

### Removing prefixes ###

Whilst the `-s` argument is very powerful (giving us `sed`-like regular
expressions), my use-case of removing the leading directory from paths is common
enough to have its own option: `--strip-components 1`
