---
title: Effective Property Checking
packages: [ "ghcWithQuickCheck" ]
---

```{pipe="cat > test.hs"}
import Test.QuickCheck
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck

```

Most of the testing I do is integration testing, using property checkers like
QuickCheck. It's remarkable how well this exposes problems I would never have
thought to write explicitly (as a unit test, for example). In this post I'll
be using Haskell, but will stick to a subset that's hopefully widely
understandable. We'll take an example of a unit test for a (hypothetical)
key/value data store, see how we can turn it into a property test, how we might
improve the test, and some of the useful techniques and different alternatives
that we can choose along the way.

## From Unit to Property ###

Property checking is a generalisation of unit testing, where our assertions can
contain free variables. Every unit test is hence *already* a property, albeit a
trivial one; the interesting part is how we might generalise such tests to take
advantage of some free variables.

The most obvious generalisation is to replace each hard-coded value with a free
variable. For example, here is a typical unit test for checking that we can look
up a value that's been inserted into a key/value database:

``` haskell
let key   = "hello"
    value = "world"
    db    = addValue key value newDB
    found = lookup (keyQuery key) db
 in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

This code defines a single value: that of `assert (...)`, given all of the
bindings specified by the equations. We're assuming that `newDB` is an empty
key/value database, and that the following functions are defined:

 - `addValue` inserts a given value into a given database at a given key.
 - `keyQuery` produces a "query" value, for looking up values associated with a
   given key.
 - `lookup` runs a given query on a given database.
 - `contains` tests whether a particular value exists in a collection (e.g. an
   element of a list).
 - `assert` tests a given boolean.

</details>

We can get a stronger test by replacing these arbitrary, hard-coded strings with
free variables. This way, we're stating that *all* strings should work
(including, say, special characters, control characters, unicode code points,
etc.), not *just* those we happened to pick:

```haskell
test key value =
  let db    = addValue key value newDB
      found = lookup (keyQuery key) db
   in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

We're now defining a function called `test`, which takes two arguments (`key`
and `value`); these represent our free variables. In principle, we can think of
this function as being a universal statement about all possible values of the
arguments; in practice, property checkers tend to try a whole bunch of
*particular* values for the arguments, to see if any counterexample can be
found.

</details>

This version is certainly better, since we've removed some irrelevant
constraints from the test (the particular choice of `key` and `value`). However,
we can go so much further when we think about all of the *implicit*
actions/values that are involved; or which irrelevant constraints are implicitly
restricting our tests.

In this example we're using `newDB`, presumably to avoid collision with an
existing key, which would cause multiple matches to be returned. However, that
constraint is unnecessarily restrictive; we only care whether or not `found`
contains `value`; we don't care whether or not it `contain`s anything else, so
we might as well generalise `newDB` to a free variable as well, to get a
stronger test that may uncover more errors:

```haskell
test key value initialDB =
  let db    = addValue key value initialDB
      found = lookup (keyQuery key) db
   in assert (contains value found)
```

## Beyond Abstracting Values ##

Many people would stop here, since all of the *explicit* constraints have been
removed (i.e. all of our hard-coded values are now replaced by free variables).
Its simplicity is certainly nice, so I might keep it around as a way to document
the system's behaviour. However it still contains more unnecessary constraints
which are *implicit*, which artificially limit the sorts of bugs it can find.

Firstly, our query is overly restricted: our `value` should be found by all
queries *containing* our `key`, not just queries for *only* our `key`, so we
can introduce a new variable for extensions to our query (we stick to
disjunction, AKA "OR", since there's no way it could accidentally filter out the
`key` lookup we care about):

```haskell
test key value initialDB extraQs =
  let db    = addValue key value initialDB
      query = reduce orQuery (keyQuery key) extraQs
      found = lookup query db
   in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

The `extraQs` argument is a list of queries which we'll combine with our
`key` lookup.

We're assuming that a call like `orQuery q1 q2` produces a query that's the
disjunction of the given queries (`q1` and `q2`, in this case). In other words,
we will get results from both (if some value satisfies both queries, it would
only appear once in the result).

The `reduce` function, also known as "`fold`", uses a given function (in this
case `orQuery`) to combine together the elements of a list (`extraQs`). It also
takes an "initial value", which in our case might as well be `keyQuery key`;
this is a separate argument to ensure we can always return *something*, even if
the list is empty.

</details>

This is stronger than before, since it will exercise more of the query building
and execution logic. Yet we've introduced an asymmetry: we allow extensions to
our `keyQuery`, but we don't allow our `keyQuery` to extend anything else! We
can fix this by demoting our `keyQuery` from being the initial value of our
`reduce` call, to being treated like any other element of `extraQs`. The
initial value can then be any query we like, so we can introduce *another* free
variable. We'll call this new variable `preQ` to indicate that it comes "before"
our `keyQuery`, and rename our `extraQs` variable to `postQs` for symmetry:

```haskell
test key value initialDB preQ postQs =
  let db    = addValue key value initialDB
      query = reduce orQuery preQ (cons (keyQuery key) postQs)
      found = lookup query db
   in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

The `cons` function puts an extra element on to the start of a list. The weird
name comes from Lisp!

</details>

This is a pattern I run into *a lot* when property testing: generalising a value
by sandwiching it between two free variables and reducing (AKA "folding") them
all together. Note that we could have used a list instead of a single `preQ`
value, and appended them all together before reducing. That would actually be
redundant, since `preQ` already represents *any* possible query, including
whatever intermediate value we would get from reducing a list of queries
together.

If we want to avoid repeating ourselves when generating such queries, or simply
want to de-clutter this test, we might choose to abstract out the details into
a reusable "query generator", like this:

```haskell
test key value initialDB queryWith =
  let db = addValue key value initialDB

      -- Generalise our `keyQuery` using `queryWith`
      query = queryWith (keyQuery key)
      found = lookup query db
   in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

The `queryWith` argument will contain a function which inserts its argument into
an arbitrary `OR` query. If we use the same idea as before, we might get:

```haskell
genQueryWith preQ postQs =
  let f q = reduce orQuery preQ (cons q postQs)
   in f
```

Here the `genQueryWith` function returns another function `f`, and it's those
`f` values which can be used for the `queryWith` argument of our `test`
function.

Note that `genQueryWith` still takes `preQ` and `postQs` as arguments, rather
than generating them internally somehow. That way, `genQueryWith`, and the
resulting `queryWith` functions, all remain pure. We might instead choose to
generate the queries inside `genQueryWith`; the details of which vary depending
on the property checker being used. Still, it is important that it takes place
outside of the resulting `queryWith` function, i.e. at the level of the `let`
rather than the `reduce`; otherwise calling `queryWith` would be impure, which
would make our property impure and hence hard to reproduce.

</details>

This is another common situation: the tradeoff between complicating our tests,
or complicating our data generators. If a specialised data generator would have
some relevant semantic meaning (in this case "generate queries whose results are
a superset of another"), it's probably worth defining it that way; even if it's
a local definition for one test. If it's useful for other tests too, pull it out
into a standalone generator.

## Generalising Actions Too ##

So far we've generalised all of the *values* in our test: `key`, `value`,
`initialDB` and `query`. We can go further by generalising our *actions*.

The "actions" in this test are `addValue` and `lookup`, although the distinction
between actions and values is blurry. In fact, we can use this to our advantage
by thinking of actions *as* values, then generalising those values like we did
before.

In this test, the important sequencing is that `addValue` occurs before `lookup`,
which is enforced via the data dependency `db`. We can make this dependency more
direct by inlining the value of `db`, as a stepping stone to our generalisation:

```haskell
test key value initialDB queryWith =
  let query = queryWith (keyQuery key)

      -- Inline the definition of `db`
      found = lookup query (addValue key value initialDB)
   in assert (contains value found)
```

Functional programmers will recognise that this is the [*composition*](
https://en.wikipedia.org/wiki/Function_composition_(computer_science)) of two
functions (our two "actions"), so let's go ahead and expose that pattern (note
that I'll use ["left to right" composition](
https://davesquared.net/2012/07/left-to-right-composition.html), since it works
out nicer in this case than the more common "right to left"):

```haskell
test key value initialDB queryWith =
  let query = queryWith (keyQuery key)

      -- Combine our two "actions" into one, using left-to-right composition
      action = compose (addValue key value) (lookup query)

      -- Apply our combined action to turn the input into the output in one go
      found  = action initialDB
   in assert (contains value found)
```

These rearrangements haven't changed the semantics of the test, but they've
exposed a violation of the ["zero, one, infinity" rule](
https://en.wikipedia.org/wiki/Zero_one_infinity_rule): our overall `action` is
made out of *two* parts (`addValue key value` and `lookup query`); yet there's
no reason we can't have more! To make this more obvious, we can put our actions
in a list and `reduce` them together using composition:

```haskell
test key value initialDB queryWith =
  let query = queryWith (keyQuery key)

      -- Chain together all (one) actions in the list, ending with `lookup`
      action = reduce compose (lookup query) [addValue key value]

      found  = action initialDB
   in assert (contains value found)
```

<details class="odd"><summary>Code notes</summary>

Notice that the actions have different types: the `addValue` action turns one
database into another (it is a "database endomorphism"), whilst the `lookup`
action turns a database into a query result. Our choice of left-to-right
composition helps us handle this in a few ways:

 - We can use the `lookup` action as our initial value, rather than composing it
   on separately.
 - Everything plugs together easily, without the need for wrapper functions.
 - We don't need to introduce some dummy action (e.g. an identity function) for
   the initial value.
 - Our list doesn't need to handle different element types (i.e. it is
   "homogeneous")
 - The list can be easily extended with more transformations.

</details>

If we squint, this reduction of a list of actions looks a bit like the situation
we had with our `query`. We can generalise it in the same way, by introducing a
free variable containing irrelevant actions to perform after `addValue`. Note
that there's no point adding actions *before* `addValue`, since the `initialDB`
variable already accounts for any possible effect they might have (similar to
`preQ` representing any query).

We need to determine what counts as an "irrelevant" action in this situation.
Since I've made up this database API for the example, I don't want to get too
bogged-down with inventing possible operations, so I'll stick to adding and
removing.

We justified the generalisation from `newDB` to `initialDB` by claiming that
existing values for `key` shouldn't prevent our `value` from being found, so by
the same logic any additions made *after* our `value` shouldn't make a
difference either (regardless of what key they use).

We can check this by introducing a free variable `extra`, containing a list of
key/value pairs. Note that we can use `uncurry` to apply a function to a pair of
arguments at once:

```haskell
-- `extra` is a list of key/value pairs to add
test key value initialDB queryWith extra =
  let query = queryWith (keyQuery key)

      -- Actions to add all of the key/value pairs, including `key`/`value`
      adds = map (uncurry addValue) (append extra [(key, value)])

      -- Combine all additions into one action
      action = reduce compose (lookup query) adds

      found  = action initialDB
   in assert (contains value found)
```

Removing keys shouldn't alter our result either, *unless* they happen to match
`key`. We can check this by using introducing another free variable for a list
of keys, and use `filter` to avoid accidental matches:

```haskell
test key value initialDB queryWith extra removals =
  let query = queryWith (keyQuery key)
      adds  = map (uncurry addValue) (append extra [(key, value)])

      -- Apply `removeKey` to all `removals` unless they equal `key`
      removes = map removeKey (filter (notEqual key) removals)

      -- Compose all additions with all removals
      action = reduce compose (lookup query) (append removes adds)

      found  = action initialDB
   in assert (contains value found)
```

This test is much more general than before, but our naïve `append`ing of actions
has introduced an implicit constraint: all removals will take place after all
additions (or vice versa if we switch the arguments to `append`). Ideally we'd
prefer them to be arbitrarily interleaved, but there are a few different ways to
achieve this.

## Interleaving ##

Arbitrary interleaving is often desirable when property checking, to prevent
details of our setup from artificially constraining the test scenario. We want
the interleaving to be deterministic, so we can reproduce any failures, but we
also want "runs" of any length to be taken from either input at any point. For
example, given additions `[a1, a2, a3, ...]` and removals `[r1, r2, r3, ...]`,
we would like their interleaving to allow starting with *no* additions, like
`[r1, ...]`, as well as a single addition like `[a1, r1, ...]`, and *two*
additions `[a1, a2, r1, ...]`, three additions `[a1, a2, a3, r1, ...]` and so
on; and the same goes for the removals, after which we switch back to additions,
and so on until we've exhausted both lists.

### Lists of Lists ###

One way to achieve this, purely by construction, is by using *lists of lists* to
represent the (possibly empty) "runs". These runs can be interleaved one at a
time, then the resulting list-of-lists concatenated together, using the
following helper functions:

```haskell
interleave [] ys = ys
interleave xs ys = cons (head xs) (interleave ys (tail xs))

interleaveRuns xs ys = concat (interleave xs ys)
```

If we use this in our test, we get the following:

```haskell
test key value initialDB queryWith addRuns removeRuns =
  let query = queryWith (keyQuery key)

      -- Map twice, since we have a list of lists
      adds = map (map (uncurry addValue)) addRuns

      -- Apply `removeKey` to all `removals` unless they equal `key`
      removes = map (compose (map removeKey) (filter (notEqual key))) removeRuns

      -- Combine all actions together, ending with our main `addKey` action
      actions = append (concat (interleave adds removes)) [addKey key value]

      -- Compose all actions together, beginning with the `lookup`
      action = reduce compose (lookup query) actions

      found  = action initialDB
   in assert (contains value found)
```

This is perfectly generic, but has a few code smells:

 - We have an extra level of `map` in the definitions of `adds` and `removes`,
   which adds a little to our cognitive load.
 - In order to re-use the `filter` over and over for each sub-list, we need an
   extra `compose` (since we have no concrete value to apply it to anymore).
 - We need an extra call to `addKey`, for `key`/`value`, since we can't just
   append them to `addRuns` (if there are fewer `addRuns` than `removeRuns`, the
   actions for those excess removals will take place before `addKey`).

### Sum Types ###

An alternative approach is to use a single list for all of the actions'
parameters, and use a [sum type](https://en.wikipedia.org/wiki/Tagged_union) to
distinguish between them:

```haskell
test key value initialDB queryWith changes =
  let query = queryWith (keyQuery key)

      -- Append our `key`/`value` pair and discard any removals of `key`
      params = append (filter (notEqual (right key)) changes)
                      [left (key, value)]

      -- Turn parameters into actions, depending on their tag
      actions = map (either (uncurry addValue) removeKey) params

      -- Compose all actions together, beginning with the `lookup`
      action = reduce compose (lookup query) actions
      found  = action initialDB
   in assert (contains value found)
```

<details class="odd"><summary>Side note about sum types</summary>

Many programming languages don't support sum types, so briefly: they let us give
values a "tag", which subsequent code can branch on. If we only need two tags,
the de facto naming convention is to call one "left" and the other "right"; the
functions `left` and `right`  wrap a value with the corresponding tag. Above,
we're using `left` for the arguments intended for `addValue`, and `right` for
those destined for `removeKey`.

Languages with sum types usually provide a branching construct, but it's often
nicer to encapsulate the branching inside an elimination function. The de facto
name for eliminating two tags is `either`, where `either f g (left x) == f x`
and `either f g (right x) == g x`).

If our language doesn't have sum types, we can fake them by pairing with a
boolean, e.g.

```haskell
left  x = (True , x)

right x = (False, x)

either f g (tag, value) = if tag
                             then f value
                             else g value
```

We can get more tags using nesting, e.g. `left`, `compose right left` and
`compose right right` for three tags. However, that's pretty horrible. There's
no standard naming convention for generic sums with more than two tags, but
they're easy enough to define (or import). To fake them, it's usually cleanest
to tag with a string.

It's also possible to fake sum types using subclasses in object oriented
programming, but eww.

</details>

I prefer this to the lists-of-lists, since there is less redundancy and we're
being more direct about what we want. In particular:

 - We only need one free variable, `changes`, rather than two.
 - Interleaving of actions is implicit to the free variable, rather than needing
   any intervention from us.
 - We don't have any two-dimensional lists, and hence no `map (map ...)` or
   extra `compose` calls.
 - We can treat both actions uniformly, with one `map` call; although we need to
   sprinkle a few `either`, `left` and `right` calls around.
 - We can append `(key, value)` to the list (albeit wrapped in `left`), rather
   than needing an extra call to `addValue`.

### Permuting ###

Finally, we could instead stick with the original `append` of additions and
removals, then remove the bias by *permuting* the result. This isn't the same as
interleaving, since the order will change, but that doesn't matter in this case.
To remain deterministic (and hence reproducible), we need to tell the permuting
function which choices to make. An easy way to do this is with a non-empty list
of natural numbers (which I'll call `seeds`), which can easily be turned into
list indices using `mod`:

```{.haskell pipe="tee -a test.hs"}
-- Inserts value `x` into list `ys` at index `n` (modulo the list length)
insert (n, x) ys = let i           = mod n (length ys + 1)
                       (pre, post) = splitAt i ys
                    in concat [pre, [x], post]

mkPermuter seeds values = reduce insert [] (zip (cycle seeds) values)

```

Alternatively we could use a single list, whose elements are of a
 of either additions or
removals. This keeps the looping easy, but complicates how we call `addValue`
and `removeKey` (since we'd need to branch on the tag).

My preferred method is to randomly permute the resulting list of actions, which
also demonstrates a pattern I use a lot in property checking: when the data we
*want* is tricky to has preconditions which are tricky to satisfy *up-front*, it's often
simpler to generate it *on-demand*, using inputs make it dynamically rather than dynamically making
"dynamic" choices ()
reproducible way to do this is to pick elements using a list of indices using a list of indicestake a do this easily if we're given a list of arbitrary numbers, which we can use as
indices (modulo the list length):


```haskell
test key value initialDB queryWith extras removals =
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
`                               db
                               chunks)
      query    = queryWith (keyQuery key)
      found    = lookup query modified
   in assert (contains value found)
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
test key value initialDB queryWith extra1 extra2 remove1 remove2 =
  let db    = addValue key value initialDB

      db2   = reduce addValue  db  extra1
      db3   = reduce removeKey db2 (filter (notEqual key) remove1)
      db4   = reduce addValue  db3 extra2
      db5   = reduce removeKey db4 (filter (notEqual key) remove2)

      query = queryWith (keyQuery key)
      found = lookup query modified
   in assert (contains value found)
```

I certainly make heavy use of the idiom of embedding the feature under test
within a bunch of irrelevant values, like sticking our query between the `preQ`
and `postQs` parameters. The same pattern comes up whenever we're free to perform
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

```{pipe="cat >> test.hs"}

main = defaultMain $ testGroup "All tests" [
    composeTests
  ,  insertTests
  , permuteTests
  ]

composeTests = testGroup "Compose tests" [
      testProperty "Compose once" t1
    , testProperty "Compose many" t2
    ]
  where t1 x y = compose (const (x :: Int)) show (y :: String) === show x

        t2 ys  = let fs :: [(String -> String)]
                     fs = map (:) ys

                     want = 'X':ys

                  in reduce compose ('X':) fs "" === want

insertTests = testGroup "Insert tests" [
      testProperty "Insert start" t1
    , testProperty "Insert end"   t2
    , testProperty "Insert index" t3
    ]
  where t1   x xs = insert (0        , x) xs === ((x : xs :: String))
        t2   x xs = insert (length xs, x) xs === ((xs ++ [x]) :: String)
        t3 n x xs = let ys = insert (n, x) xs
                        i  = n `mod` length ys
                     in ys !! i === (x :: Char)

permuteTests = testGroup "Permute tests" [
      testProperty "Permute reversal" t1
    , testProperty "Permute identity" t2
    ]
  where t1 xs = mkPermuter [0]            xs === ((reverse xs) :: String)
        t2 xs = mkPermuter [0..length xs] xs === (xs :: String)

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x)

reduce :: (a -> b -> b) -> b -> [a] -> b
reduce f = foldl (flip f)

insert :: (Int, a) -> [a] -> [a]

```

```{pipe="sh > /dev/null"}
ghc test.hs || exit 1
./test      || exit 1
```
