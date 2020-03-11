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

<details class="odd">
<summary>Code notes</summary>

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

<details class="odd">
<summary>Code notes</summary>

We're now defining a function called `test`, which takes two arguments (`key`
and `value`); these represent our free variables. In principle, we can think of
this function as being a universal statement about all possible values of the
arguments; in practice, property checkers tend to try a whole bunch of
*particular* values for the arguments, to see if any counterexample can be
found.

</details>

This test also uses a value `newDB`, presumably to avoid collision with an
existing key, which would cause multiple matches to be returned. This is an
example of an unnecessarily restrictive constraint: we only care whether or not
`found` contains `value`; we don't care whether or not it contains anything
else, so we might as well generalise `newDB` to a free variable as well, to get
a stronger test that may uncover more errors:

```haskell
test key value initialDB =
  let db    = addValue key value initialDB
      found = lookup (keyQuery key) db
   in assert (contains value found)
```

This version is certainly better than we started with, since we've removed some
irrelevant constraints from the test (that the database starts off empty, and
the particular choice of `key` and `value`). However, we can go so much further
when we think about all of the *implicit* actions/values that are involved; or
which irrelevant constraints are implicitly restricting our tests.

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

<details class="odd">
<summary>Code notes</summary>

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

<details class="odd">
<summary>Code notes</summary>

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

      query = queryWith (keyQuery key)
      found = lookup query db
   in assert (contains value found)
```

<details class="odd">
<summary>Code notes</summary>

We've generalised `keyQuery key` using a new argument `queryWith`, which is a
function that inserts its argument into an arbitrary `OR` query. It might seem
strange to generate arbitrary functions as inputs to a test, but functions are
ordinary values like anything else; and we can use the same code as before to do
the query manipulation, e.g.:

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

<details class="odd">
<summary>Code notes</summary>

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
key/value pairs:

```haskell
-- `extra` is a list of key/value pairs to add
test key value initialDB queryWith extra =
  let query = queryWith (keyQuery key)

      -- List of actions to add all of the key/value pairs
      adds = map (uncurry addValue)
                 (append extra [(key, value)])

      -- Combine elements of adds into one action
      action = reduce compose (lookup query) adds

      -- Add our initial
      found = action initialDB
   in assert (contains value found)
```

<details class="odd">
<summary>Code notes</summary>

We can use `uncurry` to apply a function to a pair of arguments at once, such
that `uncurry f (x, y)` is the same as `f x y`.

Notice that our use of left-to-right composition requires the pair
`(key, value)` to come at the *end* of the list `adds`, in order for it to be
applied to the database *first*.

</details>

Removing keys shouldn't alter our result either, *unless* they happen to match
`key`. We can check this by using introducing another free variable for a list
of keys, and use `filter` to avoid accidental matches:

```haskell
test key value initialDB queryWith extra removals =
  let query = queryWith (keyQuery key)
      adds  = map (uncurry addValue)
                  (append extra [(key, value)])

      -- Apply `removeKey` to all `removals` unless they equal `key`
      removes = map removeKey (filter (notEqual key) removals)

      -- Compose all additions with all removals
      action = reduce compose (lookup query) (append removes adds)

      found = action initialDB
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

      -- Combine all actions together
      actions = concat (interleave adds removes)

      -- Compose all actions together, beginning with the `lookup`
      action = reduce compose (lookup query) actions

      -- Add our key before applying the other actions
      found = action (addKey key value initialDB)
   in assert (contains value found)
```

This is perfectly generic, but the extra verbosity has introduced a few code
smells:

 - We have an extra level of `map` in the definitions of `adds` and `removes`,
   which adds a little to our cognitive load.
 - In order to re-use the `filter` over and over for each sub-list, we need an
   extra `compose` (since we have no concrete value to apply it to anymore).
 - We need an extra call to `addKey`, for `key`/`value`, since it needs to come
   after everything in the `actions` list. Applying it directly to `initialDB`
   avoids the need for other complications; e.g. putting it on the end of
   `actions` would need an `append` call; prepending it to `adds` would require
   `reverse` on the `actions`; etc.

### Sum Types ###

An alternative approach is to use a single list for all of the actions'
parameters, and use a [sum type](https://en.wikipedia.org/wiki/Tagged_union) to
distinguish between them:

```haskell
test key value initialDB queryWith changes =
  let query = queryWith (keyQuery key)

      -- Discard any removals of `key`
      params = filter (notEqual (right key)) changes

      -- Turn parameters into actions, depending on their tag
      actions = map (either (uncurry addValue) removeKey) params

      -- Compose all actions together and apply them, as before
      action = reduce compose (lookup query) actions
      found  = action (addValue key value initialDB)
   in assert (contains value found)
```

<details class="odd">
<summary>Side note about sum types</summary>

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
 - We don't have any two-dimensional lists, ¯and hence no `map (map ...)` or
   extra `compose` calls.
 - We can treat both actions uniformly, with one `map` call; although we need to
   sprinkle a few `either`, `left` and `right` calls around.

### Parameterising Choices ###

Another possible approach is to have our `interleave` function take an arbitrary
number of elements each time. To remain deterministic, the choice of how many
elements to take needs to come from elsewhere, and be passed in as an extra
argument. We can call such a function an "interleaver", and provide a generator
which "seeds" an interleaver with arbitrary choices:

```haskell
interleaveN ns [] ys = ys
interleaveN ns xs ys = let (pre, post) = splitAt (head ns) xs
                        in append pre (interleaveN (tail ns) ys post)

mkInterleaver choices = interleaveN (cycle choices)
```

<details class="odd">
<summary>Code notes</summary>

The `cycle` function repeats a list over and over, so as long as `choices` is
non-empty, the list `ns` will never run out.

A call to `splitAt n l` returns a pair, containing the first `n` elements of the
list `l`, and the rest of the elements (if any).

</details>

A test taking such an "interleaver" as a parameter would look like this:

```haskell
test key value initialDB queryWith extra removals interleaver =
  let query   = queryWith (keyQuery key)
      adds    = map (uncurry addValue) extra
      removes = map removeKey (filter (notEqual key) removals)

      -- Use interleaver to avoid patterns when appending adds and removes
      actions = interleaver adds removes

      -- Compose and apply as before
      action = reduce compose (lookup query) actions
      found  = action (addValue key value initialDB)
   in assert (contains value found)
```

I don't think this is as nice as the sum-type approach, since we still have to
process the additions separately from the removals. Still, I think it is quite
reasonable, and it demonstrates another common pattern in property checking:
writing data generators can often be made easier by giving them a source of
"choices" to draw from when a decision needs to be made. These choices can
usually be a simple list of booleans or integers, which is trivial to plug in to
complete the generator.

### Permuting ###

Finally, we could use our original `append` of additions with removals, but use
a similar approach to the "interleaver" to *permute* the result. Rather than
ensuring the behaviour we want "by construction" (i.e. building our list of
`actions` such that additions and removals can occur in any order), we're
instead going to *impose* that behaviour after-the-fact. Note that this isn't
quite the same as interleaving, since elements may get rearranged as well, but
that doesn't matter in this ezmple.

To remain deterministic (and hence reproducible), we seed our "permuter" with
arbitrary choices, like we did for the "interleaver":

```{.haskell pipe="tee -a test.hs"}
-- Inserts value `x` into list `ys` at index `n` (modulo the list length)
insert (choice, x) ys = let i           = mod choice (length ys + 1)
                            (pre, post) = splitAt i ys
                         in concat [pre, [x], post]

mkPermuter choices values = reduce insert [] (zip (cycle choices) values)

```

<details class="odd">
<summary>Code notes</summary>

Like before, we use `cycle` to ensure we never run out of `choices`. Each
`choice` needs to be adjusted before we can use it, since arbitrary numbers
might not be valid indices into our lists. We use `length ys + 1` so that even
an empty list `ys` will still give us an index: `0`, in that case.

The `zip` function pairs up the elements of two lists, so
`zip [a, b, c] [x, y, z]` would give `[(a, x), (b, y), (c, z)]`. We use this to
pair up each `choice` with a list element, for use as the first argument of
`insert`.

Notice that our adjustment of `choice` is dynamic: each time `insert` is called
(as `reduce` works its way through the list produced by `zip`), the list `ys`
gets longer and longer. This causes the `mod` calculation to change, allowing
larger and larger indices. Trying to generate a list of arbitrary indices
up-front would be tricky, but relying on `mod` to cut them down once we know the
length is much easier.

</details>

We can use `mkPermuter` to generate values for a `permuter` argument, like this:

```haskell
test key value initialDB queryWith extras removals permuter =
  let query   = queryWith (keyQuery key)
      adds    = map (curry addValue) extras
      removes = filter (notEqual key)) removals

      -- Arbitrarily permute the list of adds and removals
      actions = permuter (append adds removes)

      -- Compose and apply as before
      action = reduce compose (lookup query) actions
      found  = lookup query (addValue key value initialDB)
   in assert (contains value found)
```

This looks about as reasonable as the "interleaver", but both require extra
definitions that the sum-type implementation doesn't. The "permuter" approach is
also less applicable to other situations, e.g. if we need to ensure that some
values occur before others, even if we don't care whether others occur in
between.

## Conclusion ##

Regardless of which approach we take, our resulting test is far stronger than
the unit test we began with, since it generalises a lot of things we might have
missed if we didn't think carefully. This is more likely to find problems caused
by weird sequences and interleavings of actions, which we probably wouldn't
think to test in isolation. Such sequences can also be useful for [finding
concurrency issues](
https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#stateful-testing)

I certainly make heavy use of the idiom of burying the required data/action
within a bunch of irrelevant values (like
`preQ`/`postQs`/`changes`/`extra`/etc.) The same pattern comes up whenever we're
free to perform a *sequence* of actions which ostensibly shouldn't impact our
result.

Another nice trick, which is obvious in hindsight but not necessarily easy to
think up, is making dynamic choices by taking an easily-generated "seed" and
altering it in context; like picking a list element using an arbitrary number
modulo how many possibilities there are.

As a more complex example, we might have a Web app and want to ensure that no
sequence of clicks can result in some unwanted behaviour. The question is, how
might we generate those clicks? Pages are presumably generated dynamically, and
links from one page to another depend on layers of indirection like routing, so
how on earth might we generate a valid sequence of clicks, like
`["profile", "about", "contact", "email"]`, if we don't know that clicking on
`"profile"` will take us to a page with an `"about"` link, and so on?

If we use our "parameterised choices" trick, we simply generate a list of random
numbers: to pick a link we just count all those on the page, take the next
random number modulo that count, and use that as the index for which link to
click. For example, we might generate a list like `[308, 1006, 248264, 7]`; if
the first page contains 100 links then `mod 308 100 = 8` so we click the 8th
link; if that happens to take us to a page with 250 links, then
`mod 1006 250 == 6` so we click the 6th link, and so on. This gives uniform
weighting to each link, and nicely avoids false positives (e.g. passing the test
despite broken paths, if we forgot to add new links to hard-coded tests) and
false negatives (e.g. tests which fail, but only because the hard-coded paths
they're trying to test don't exist anymore).

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
