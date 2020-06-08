---
title: Haskell Strings
---

This post is taken from
[my comment on Hacker News](https://news.ycombinator.com/item?id=23456519), in
response to [a blog post about Haskell's string
types](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings).

I agree with that post, although the arguments it uses aren't the strongest.
In particular:

The main reason given for `String` being slow is that it's immutable and hence
leads to many duplicate Strings being created. In fact, the main reason
`String`s are slow is that their linked-list structure has a lot of overhead,
and may be scattered around in memory. For example, the `String` `"abc"` will be
represented something like this:

    +---+---+   +---+---+   +---+---+
    | * | *---->| * | *---->| * | *---->[]
    +-|-+---+   +-|-+---+   +-|-+---+
      |           |           |
      V           V           V
     'a'         'b'         'c'

Those arrows are pointers, which can point to locations arbitrarily far
away. From this we can see that just *storing* a (fully evaluated) `String` is
expensive, since each character requires a pair of pointers (in addition to the
unique characters themselves). Processing the contents of this `String` is also
slow, since we need to dereference two pointers for each character (one to get
the `Char`, one to get the tail of the `String`). Since the data may not be
contiguous in memory, this can make poor use of the CPU caches, which might
otherwise speed up these dereferences.

Compare this to a C-style string, which would look like this in memory:

    +---+---+---+---+
    | a | b | c | 0 |
    +---+---+---+---+

This "packed" representation requires no long-distance dereferencing, the memory
is contiguous so it will make good use of the cache, and we can process the
characters directly without having to chase pointers.

That article describes the `ByteString` type as a "list of `Word8` objects", but
that's a bit misleading. Firstly we often say "list" to mean a chain-of-pointers
like the first diagram above, e.g. that's exactly what Haskell's built-in list
type is, and Haskell's `String` is such a list. `ByteStrings` store their
`Word8` objects more like the second diagram, so it would be better to call them
an "array of `Word8` objects". Secondly, `ByteStrings` use a clever
three-layered structure to speed up common operations:

 - The raw `Word8` characters are stored in arrays, like the C-style string
   above (but without the `NUL` byte at the end)

 - These arrays are pointed to by values called "chunks". These are "fat
   pointers", i.e. they also store a length and offset value.

 - A `ByteString` itself is a list of chunks (like the first `String` diagram,
   but instead of values like `'a'`, `'b'` and `'c'` the values are chunks).

This makes `ByteString` *really* fast, for three reasons:

 - Some operations can be performed by just fiddling at the list-of-chunks
   level. For example, to append `X` and `Y`, the result is just a list of `X`'s
   chunks followed by `Y`'s chunks; no need to touch the underlying chunks or
   arrays.

 - Some operations can be performed by just fiddling the length and/or offset of
   a chunk. For example, incrementing a chunk's offset will chop characters off
   the start (they're still stored in memory, but they will be ignored);
   decrementing a chunk's length will chop characters off the end.

 - The same underlying `Word8` arrays can be pointed to by many different chunks
   in many different `ByteStrings`; i.e. we rarely need to copy the actual
   characters around.

As an example, let's say we read the `ByteString` `"[\"foo\", \"bar\"]"`
(without the backslashes), we parse it as JSON to get a list of `ByteString`s
`["foo", "bar"]`, then we concatenate that list to get the single `ByteString`
`"foobar"`, here's how it might look in memory:

                         BS--+---+   BS--+---+
    "foobar":            | * | *---->| * | *---->[]
                         +-|-+---+   +-|-+---+
                           |           |
                      +----+           +----------------+
                      |                                 |
                      |  List+---+      List+---+       |
    ["foo", "bar"]:   |  | * | *------->| * | *---->[]  |
                      |  +-|-+---+      +-|-+---+       |
                      |    |              |             |
                      |    |              |             |
                      |    |              |             |
                      |    V              V             |
                      | BS--+---+       BS--+---+       |
    "foo" and "bar":  | | * | *---->[]  | * | *---->[]  |
                      | +-|-+---+       +-|-+---+       |
                      |   |               |             |
                      |   |               +---+         |
                      |   |                   |         |
                      V   V                   V         V
                     Chunk---+---+           Chunk---+---+
    (chunks)         | 3 | 2 | * |           | 3 | 9 | * |
                     +---+---+-|-+           +---+---+-|-+
                               |                       |
           BS--+---+           |                       |
    Input: | * | *---->[]      |                       |
           +-|-+---+           |                       |
             |                 |                       |
             V                 |                       |
             Chunk+---+---+    |                       |
    (chunk)  | 14 | 0 | * |    |                       |
             +----+---+-|-+    |                       |
                        |      |                       |
                        |      |                       |
                        |      |                       |
                        V      V                       V
            Array---+---+---+---+---+---+---+---+---+---+---+---+---+
    (array) | [ | " | f | o | o | " | , |   | " | b | a | r | " | ] |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+---+


(Note that the exact position where arrows "land" doesn't matter; they're
pointing to the entire datastructure they "hit")

Here we can see that there's only one copy of the underlying text; that the
substrings `foo` and `bar` are simply chunks with length 3, offset by
appropriate amounts; and that the resulting `foobar` `ByteString` is just a list
of these two chunks.

This approach of "store things once, then do all processing with indices" can be
*very* fast (even faster than C in some cases
https://chrisdone.com/posts/fast-haskell-c-parsing-xml ). Whilst we can
obviously write this sort of algorithm in any language, re-using arrays and
chunks like this relies on them being immutable, which is more idiomatic in
Haskell. In particular:

 - Languages which tend to use mutation aren't well suited to this, since
   mutating one value can have unforseen consequences to those which are
   re-using the same components. Copying is safer and more predictable in the
   face of mutation.

 - Languages which favour some built-in interface rather than high-level
   functions may need to copy values in order to comply with these
   interfaces. In particular, C's array syntax will work for arrays and chunks,
   but not for the overall `ByteString` structure (a list of chunks).

 - If we want `NUL`-terminated arrays, we'll need to make copies when truncating
   strings, to avoid the `NUL` byte overwriting the truncated part.

 - Re-using values can make it hard to keep track of which parts can be
   freed. Garbage collection (and other approaches, like linear types, borrow
   checking, etc.) can make this easier.

The difference between lazy and strict `ByteStrings` is just whether the overall
list-of-chunks is lazy (chunks generated on-demand, useful for streaming) or
strict (chunks are generated up-front, closer to using one big array, hence
potentially faster and more predictable).

The `Text` type is just a `ByteString` paired with a particular encoding method,
e.g. `UTF8`.

The linked article also talks about fusion, claiming that's why `Text` (and
hence `ByteString`) is faster, or avoids intermediate allocations. Fusion is
great, but care must be taken if we're going to rely on it; e.g. very minor
changes to an expression (say, to make debugging easier) can stop things from
fusing, greatly changing the compiled code's speed and memory usage.

On the subject of fusion, it's worth noting that Haskell's built-in list type is
also subject to fusion (and hence so is `String`, since that's just a list of
`Char` values). List fusion often resulting in *zero* allocation; e.g.
generating a list of `Char`s, then counting them, might compile down to a single
loop with a single counter variable, and no `Char`s/lists/`String`s in sight!
Again, it can be tricky to ensure that this happens. One approach is to test for
it with something like https://hackage.haskell.org/package/inspection-testing
