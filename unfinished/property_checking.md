---
title: Effective Property Checking
---

Most of the testing I do is integration testing, using property checkers like
QuickCheck. It's remarkable how well this exposes problems I would never have
thought to write explicitly (as a unit test, for example).

It's easy to think of property checking as a minor generalisation of unit tests,
where we replace hard-coded values/actions with free variables, e.g. turning a
test like:

``` haskell
let key   = "hello"
    value = "world"
    db    = addValue key  value  newDB
    found = lookup (keyQuery key) db
 in assertEqual found  [value]
```

Into something like:

```haskell
test key value =
  let db    = addValue key value newDB
      found = lookup (keyQuery key) db
   in assertEqual found [value]
```

This is certainly better, since we've removed some irrelevant constraints from
the test (the particular choice of `key` and `value`). However, we can go so
much further when we think about all of the *implicit* actions/values that are
involved; or which irrelevant constraints are implicitly restricting our tests.

In this example we're using `newDB`, presumably to avoid collision with an
existing key, which would cause multiple matches to be returned. However, that
constraint is unnecessarily restrictive; we only care about the *presence* of
`value` in what's `found`, so we might as well avoid hard-coding the value. This
gives us a stronger test that may uncover more errors:

```haskell
test key value initialDB =
  let db    = addValue key value initialDB
      found = lookup (keyQuery key) db
      -- We only care that `found` contains `value`
   in assertTrue (contains value found)
```

Many people would stop here, since those are all of the *explicit*
constraints. However, there are still more unnecessary constraints that are
*implicit*. Firstly, our query is overly restricted; as long as our `key` lookup
is in there somewhere, it shouldn't matter what else we lookup:

```
test key value initialDB preQ postQ =
  -- preQ and postQ are lists of queries to add on to ours
  let db = addValue key value initialDB

      -- Disjunction (OR) should only ever add to the result
      query = disjunctionQuery (append preQ [keyQuery key] postQ)
      found = lookup query db
   in assertTrue (contains value found)
```

Next, we're not allowing any modifications to the DB in between `addValue` and
`lookup`. Adding to the DB shouldn't affect our test, so we should check that
(taking in a list of key/value pairs `extras`):

```haskell
test key value initialDB preQ postQ extra =
  -- Take a list of key/value pairs `extra`
  let db = addValue key value initialDB

      -- Combine each `extra` with the DB using `addValue`
      modified = reduce addValue db extra
      query    = disjunctionQuery (append preQ [keyQuery key] postQ)
      found    = lookup query modified
   in assertTrue (contains value found)
```

Removing values shouldn't alter our result either, unless they match our key;
likewise the interleaving of additions and removals shouldn't matter:

```haskell
test key value initialDB preQ postQ extras removals =
  -- Rather than having one list of additions and one of removals, they are both
  -- lists-of-lists. This allows arbitrary interleaving.
  let db       = addValue key value initialDB
      chunks   = interleave extras
                            -- Remove `key` from any removal chunk
                            (map (filter (notEqual key)) removals)

      -- Apply the adds/removes one "chunk" at a time; `mode` toggles between
      -- add and remove; `first` discards the final mode
      modified = first (reduce (\chunk (mode, db) ->(
                                   not mode
                                 , reduce (if mode
                                              then addValue
                                              else removeKey)
                                          db
                                          chunk
                                 ))
                               db
                               chunks)
      query    = disjunctionQuery (append preQ [keyQuery key] postQ)
      found    = lookup query modified
   in assertTrue (contains value found)
```

This test is far stronger than the unit test we began with, since it generalises
a lot of things we might have missed if we didn't think carefully. This is more
likely to find problems caused by weird sequences and interleavings of actions,
which we probably wouldn't think to test in isolation.

In this case I think the interleaving of arbitrarily-many rounds of insertion
and deletion isn't really worth the extra complexity. I would either pull the
interleaving logic out into a library function (if it's sufficiently
general/useful), or otherwise I'd just do a couple of rounds each (so we have
adds sandwiched between removes, and vice versa), to get a nice compromise like:

```haskell
test key value initialDB preQ postQ extra1 extra2 remove1 remove2 =
  let db    = addValue key value initialDB

      db2   = reduce addValue  db  extra1
      db3   = reduce removeKey db2 (filter (notEqual key) remove1)
      db4   = reduce addValue  db3 extra2
      db5   = reduce removeKey db4 (filter (notEqual key) remove2)

      query = disjunctionQuery (append preQ [keyQuery key] postQ)
      found = lookup query modified
   in assertTrue (contains value found)
```

I certainly make heavy use of the idiom of embedding the feature under test
within a bunch of irrelevant values, like sticking our query between the `preQ`
and `postQ` parameters. The same pattern comes up whenever we're free to perform
a *sequence* of actions which ostensibly shouldn't impact our result.

Another nice trick, which is obvious in hindsight but not necessarily easy to
think up, is making dynamic choices using numbers modulo the possibilities. For
example, we might have a Web app and want to ensure that no sequence of clicks
can result in some behaviour. The question is, how might we generate those
clicks? Pages are presumably generated dynamically, and links from one page to
another depend on layers of indirection like routing, so how on earth might we
generate a valid sequence of clicks, e.g.
`["profile", "about", "contact", "email"]`, if we don't know that clicking on
`"profile"` will take us to a page with an `"about"` link, and so on?

The easy answer is that we simply generate a list of random numbers: to pick a
link we just count all those on the page, take the next random number modulo
that count, and use that as the index for which link to click. For example, we
might generate a list like `[308, 1006, 248264, 7]`; if the first page contains
100 links then `mod 308 100 = 8` so we click the 8th link; if that happens to
take us to a page with 250 links, then `mod 1006 250 == 6` so we click the 6th
link, and so on. This gives uniform weighting to each link, and nicely avoids
false positives (e.g. passing the test despite broken paths, if we forgot to add
new links to hard-coded tests) and false negatives (e.g. tests which fail, but
only because the hard-coded paths they're trying to test don't exist anymore).
