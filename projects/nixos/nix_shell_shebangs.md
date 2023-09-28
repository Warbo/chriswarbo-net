---
title: Nix Shell Shebangs
packages: [ 'python3' ]
---

# Shebangs #

```{pipe="cat > shebang.sh"}
#! /bin/sh

echo "hello world!"
```

```{pipe="cat > shebang.py"}
#! /usr/bin/env python3

print("hello world!")
```

```{pipe="sh"}
chmod +x shebang.*
(source "$stdenv/setup" && patchShebangs .)
```

A "shebang", written as `#!`, is a special code at the start of a file, which is used to tell the OS how to execute that file. For example, the top line of this script tells the OS to run it using the `/bin/sh` program:

```{.bash pipe="cat shebang.sh"}
```

If we run this script, we get:

```{pipe="./shebang.sh"}
```

Here's an example which runs Python:

```{.python pipe="cat shebang.py"}
```

Which outputs:

```{pipe="./shebang.py"}
```

Notice that in the second case we don't call the `python3` command directly; instead we call the `env` command, which looks up the `python3` command in our `$PATH` variable (in my case, `type -a python3`{.unwrap pipe="sh | pandoc -t json"}, which I'd rather not hard-code!).

# `nix-shell` #

Since the Nix package manager can install programs "non-destructively" (ie. without disrupting existing software), we can install 'disposable' software; ie. install `foo`, run it, then remove it, without any other software noticing. To do this, we use [`nix-shell`](http://nixos.org/nix/manual/#sec-nix-shell).

We tell `nix-shell` what software to install by providing either:

 - A list of packages with the `-p` flag, eg. `nix-shell -p python3 python3Packages.prettytable`{.bash}
 - A Nix expression with the `-E` flag, eg. `nix-shell -E 'with import <nixpkgs> {}; stdenv.mkDerivation { name = "foo"; buildInputs = [ python3 python3Packages.prettytable ]; }'`{.bash}
 - A Nix expression in a file, passed as an argument, eg. `echo 'with import <nixpkgs> {}; stdenv.mkDerivation { name = "foo"; buildInputs = [ python3 python3Packages.prettytable ]; }' > pkg.nix; nix-shell pkg.nix`{.bash}
 - A Nix expression in a file called `shell.nix`, eg. `echo 'with import <nixpkgs> {}; stdenv.mkDerivation { name = "foo"; buildInputs = [ python3 python3Packages.prettytable ]; }' > shell.nix; nix-shell`{.bash}
 - A Nix expression in a file called `default.nix`, eg. `echo 'with import <nixpkgs> {}; stdenv.mkDerivation { name = "foo"; buildInputs = [ python3 python3Packages.prettytable ]; }' > default.nix; nix-shell`{.bash}

Inside this environment we can do a few things:

 - Launch an interactive shell, which is the default behaviour: `nix-shell`{.bash}
 - Launch an (interactive or non-interactive) command, using the `--command` or `--run` flags, eg. `nix-shell --command python3`{.bash} or `nix-shell --run python3`{.bash}

For example, we might need to run a Python script, but we either:

 - Don't have Python installed
 - Don't *want* to install Python permanently
 - Don't know if Python is installed (eg. if our script is to be used by others)

If we *do* know that Nix is available, we can do something like the following:

```bash
#! /bin/sh
nix-shell -p python3 --run python3 << EOF
print("Hello world!")
EOF
```

Running this script will invoke `/bin/sh` as an interpreter (thanks to the shebang), then it will use `nix-shell` to temporarily install the `python3` package. With the `python3` package available, it will run the `python3` command, and pipe in the script `print("Hello world")`{.python} to its stdin. Once that Python script has finished, the `python3` interpreter will exit, `nix-shell` will exit, and the environment it created will be available for *garbage collection*.

Notice a few things about our throw-away Python environment:

 - If Python is *already* installed, `nix-shell` will re-use it
 - When `nix-shell` exits, it doesn't uninstall Python (since that would be a bad idea if the user already had Python beforehand!)
 - Until we run the garbage collector, the environment is still available on disk. This makes it quick to re-use the same shell over and over, eg. without having to keep downloading and/or rebuilding our software.

# `nix-shell` shebangs #

There are two obvious flies in our ointment: firstly, `nix-shell` must be installed, which we can't really get around; secondly, our Python code was written as a string in a shell script.

As of Nix 1.9 we can solve this second issue, by writing a regular Python script but using `nix-shell` as our shebang:

```python
#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

print("Hello world!")
```

This shebang has a few parts:

 - The first line tells the OS to run `/usr/bin/env`. This will look for `nix-shell` in our `$PATH` and run it, so we don't have to hard-code the path.
 - By using `/usr/bin/env`, we lose the ability to pass arguments to the `nix-shell` command. To work around this, `nix-shell` will look at that second line to get its options.
 - We use the special flag `-i` to tell `nix-shell` which interpreter to use (similar to `--run` or `--command`, but it passes the filename, rather than using stdin)

By using `nix-shell` in our shebangs like this, we're going one step further than `/usr/bin/env`:

 - When we use a shebang like `#! /usr/bin/python3`, we're assuming that Python is installed and available at a particular location
 - When we use a shebang like `#! /usr/bin/env python3`, we're assuming Python is installed, but we make no assumption about its location. We *do* add a dependency on `env`, but hope that it's widely available.
 - When we use a shebang like the `nix-shell` one above, we're making no assumption about Python being installed at all. We add another dependency, on `nix-shell`, but hopefully that will become more widespread over time; after all, *by design* it shouldn't interfere with existing software (apart from taking up disk space!)
