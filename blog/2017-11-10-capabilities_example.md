---
title: Capabilities Example
---

A nice introductory example of capabilities (as found in 'object capability
systems') vs access control lists, is to compare the following commands:

    cp some/source/file some/destination/file

    cat < some/source/file > some/destination/file

The first one is putting a lot of trust in the `cp` command: we need to give it
read/write access to the file system, and we hope that it will only access those
paths we asked it to, and that it will access them in the right ways (reading
from one, writing to the other).

In the second command, the `cat` program doesn't need any access to the
filesystem, and the only thing we need to trust is that it will send through the
data unchanged. That trust seems pretty much required though, since it's the
reason to use `cat` in the first place.

A similar example is how Haskell programs tend to implement most of their logic
using pure functions, and limit potentially-dangerous IO actions to the
periphery. For example, I've written many programs which look like this:

    main :: IO ()
    main = interact someFunction

    someFunction :: String -> String
    someFunction input = ...

Here the `main` function has type `IO ()`, meaning that it can perform arbitrary
effects. It calls the `interact` function, which reads from stdin and writes to
stdout, transforming one into the other using the given function (here called
`someFunction`). The nice part is: `someFunction` is just a pure function from
`String`s to `String`s, it doesn't care where these `String`s come from or go
to. The only "capability" available to `someFunction` (and hence to any other
functions that it calls) is to affect the contents of its output `String`
(assuming we're using the "Safe Haskell" option in GHC).

Compared to the above `cp`/`cat` example, we can think of `main` as acting like
the shell, and `interact` as acting like `< /dev/stdin > /dev/stdout`.
