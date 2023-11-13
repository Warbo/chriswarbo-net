---
title: Billion Dollar Mistake
---

Sometimes we need to indicate the lack of a result. For example, let's say we're
writing a `lookup` function for a `KeyValueMap[K, V]`, what should we do if
there is no entry for the given key?

## A One Dollar Mistake (My Preferred Approach) ##

Sometimes we have a value to return, sometimes we have no value to return. We
can represent both possibilities using a list[1]. Let's call this API our $1
mistake:

    function lookupOneDollar(m: KeyValueMap[K, V], key: K): List[V] {
      // Lookup/traversal logic goes here; I'll cheat by assuming we have some
      // sort of 'contains' function.
      return contains(m, key)? [m[key]] : [];
    }

Existing functions like `map` can handle both possibilities at once, e.g.
`map(areaCode, lookup(phoneBook, myCustomer))` will return a list that's
either empty, or contains a single area code.

If the function we pass to `map` may *also* return no value, then we'll produce
a nested list, with three distinct possibilities:

 - An empty list `[]` indicating that the first step had no value (e.g.
   `myCustomer` was not found in `phoneBook`)
 - A list containing an empty list `[[]]` indicating that the second step had no
   value (e.g. we found their entry but it had no `areaCode`)
 - A list containing a non-empty list, like `[["020"]]`, which indicates that
   both steps succeeded.

Sometimes we don't care *why* a value is missing, so we don't need the nesting.
In that case we can `flatten` (or `join`) the result: in the successful scenario
that will return a list of results (e.g. `["020"]`); whilst *both* of the
unsuccessful scenarios will return an empty list (e.g. `[]`). This combination
of `map` followed by `flatten` is common enough to get its own function, usually
called `flatMap`. Having a dedicated function also allows implementations to
skip the wrapping-then-flattening, for efficiency.

This is my currently-preferred solution to the problem of "representing a
missing value". Some languages let us (or libraries) define our own list-like
types, in which case we should[2] replace `List` (which may contain any number
of values) with some `ListOfAtMostOne` type (containing either zero or one
value). Many languages/libraries provide such a type, usually with a nicer name
like `Maybe` or `Option`.

<details class="odd">
  <summary>Boolean blindness</summary>

Note that the examples in this post suffer from "boolean blindness", since they
are performing unsafe operations like `m[key]` based on a boolean condition
like `contains(m, key)`. Real implementations should use a safer,
correct-by-construction approach; but the details would depend on our choice of
datastructure, and are hence out of scope for this post!

</details>

## A Thousand Dollar Mistake ##

A different approach is to return some "default" value of type `V`, if the entry
isn't found.

We want our `KeyValueMap` to be generic[3] (i.e. work the same for all types
`V`) so we can't hard-code a default, since we don't know what type `V` will
be. Instead, an API can take a default value as an *extra argument*, which I'll
call our "$1,000 mistake":

    function lookupThousandDollar(m: KeyValueMap[K, V], key: K, default: V): V {
      return contains(m, key)? m[key] : default;
    }

First of all, notice that this $1,000 API is less powerful than the $1 API
above, since the former can be built using the latter, e.g.

    function thousandOnOne(m: KeyValueMap[K, V], key: K, default: V): V {
      found = lookupOneDollar(m, k);
      return (found == [])? default : found[0];
    }

We *cannot* do the converse, since this $1,000 API does not retain enough
information to implement the $1 API. In particular, there are two situations
where `lookupThousandDollar(myMap, myKey, myDefault)` returns the value
`myDefault`: when `!contains(m, key)` *and* when `m[key] == myDefault`.  The $1
API needs to return different values in each case (`[]` and `[myDefault]`,
respectively), but the $1,000 API's response doesn't tell us which (we could
call `contains` directly, but then we wouldn't need this $1,000 API at all!).

This API works when there is a meaningful default value, e.g. if we're counting
a bunch of items and their `quantity` defaults to `1` (our counting doesn't care
whether `1` comes from a specified `quantity` or the default). Unfortunately,
there are many situations where we *do* care; and there is no appropriate value
we can use as a default. For example, looking up the position of a value in a
list should give us a natural number, but every natural number is already a
meaningful position: there are no values "left over" which we could use as a
default (to indicate absence).

A common solution to this problem is extending the space of values we're using,
e.g. representing positions using integers lets us represent absence with
negatives. However, that's [even worse](2) since it erodes safety elsewhere,
e.g. lookups would have to deal with negative positions, traversals would have
to deal with negative start/end points, etc.

## A Million Dollar Mistake ##

Another approach is to change the language so types can be "nullable". This
extends types with an extra value called `null`, and is sometimes written `?T`
to mean "`T` extended with the value `null`". If we think of types as sets, then
`?T = T ∪ {null}` (the union of `T` with the set `{null}`).

Using `?V` as our return type is a "$1,000,000 mistake": it lets us return
`null` for any given type, whilst remaining generic:

    function lookupMillionDollar(m: KeyValueMap[K, V], key: K): ?V {
      return contains(m, key)? m[key] : null;
    }

Unfortunately this is fraught with problems. Generic functions must work for
nullable types (the alternative is even worse!), so our value type `V` itself
could be nullable (e.g. `KeyValueMap[String, ?Int]`). That gives the same
problem as our $1,000 mistake: that we can't tell the difference between missing
values and those which just-so-happen to be `null`. Note that the result in such
cases would be "doubly nullable" (e.g. `??Int`), but such nesting is always
equivalent to the "singly nullable" type (the set union is idempotent, since
`{null, null} = {null}`); or, in other words, wrapping with multiple `?` symbols
doesn't matter since there's only one `null` symbol available to use as a value.

Even in a language with such a nullability feature, the original $1 API is
*still* more informative and expressive, since we can implement the $1,000,000
version as follows:

    function millionOnOne(m: KeyValueMap[K, V], key: K): ?V {
      found = lookupOneDollar(m, k);
      return (found == [])? null : found[0];
    }

The converse is still not possible, since this $1,000,000 API also throws away
the information needed to distinguish between missing values and values which
just so happen to be `null` (same as the $1,000 API).
the $1 no way to implement the $1 API using We also can't  Notice that we *cannot* implement this using the $1,000 API, since its `default`
argument has the same type as its return value (so we can't pass `null`, since
that type may not be nullable).

## The Billion Dollar Mistake ##

We've finally reached Tony Hoare's "billion dollar mistake", which is to make
*all* types nullable, with no way to opt-out. Such a "$1,000,000,000 API" might
look something like this:

    function lookupBillionDollar(m: KeyValueMap[K, V], key: K): V {
      return contains(m, key)? m[key] : null;
    }

Notice that we're allowed to return `null` for *any* type `V`. This may look
appealing at first glance, but consider all the different possibilities we now
have to handle (again, with no way to opt-out):

 - The map `m` could be `null`
 - The key `key` could be `null`
 - The result of `contains(m, key)` could be `null`
 - The value of `m[key]` could be `null`
 - If our language has first-class functions, then `contains` itself may also be
   `null`

The example above is quite unsafe, since it makes many assumptions about the
behaviour of `contains`, which aren't being checked by the type system (e.g.
that `contains` is not `null`, that it will accept `null` arguments, etc.).
Safer implementations would be more cumbersome, e.g.

    function lookupPotentialNulls(m: KeyValueMap[K, V], key: K): V {
      return (m == null)? null
                        : (contains == null)? null
                                            : contains(m, key)? m[key]
                                                              : null;
    }

What makes this approach so insidious is that we're forced to deal with all of
these `null` checks, even in situations *without* optional/missing values! Or,
more accurately, it turns *all* expressions into potentially-missing values!

Again, even in languages which have made this costly mistake, the "$1 API" is
*still* more expressive, as it's able to distinguish between cases which the
$1,000,000,000 API would all coalesce to an uninformative `null`.

## Conclusion ##

There is a difference between unavoidable complexity, and problems to be
worked-around. Many programmers seem to be unaware that

---

References:

[1] [How to replace failure by a list of successes](https://link.springer.com/chapter/10.1007%2F3-540-15975-4_33) ([PDF here](https://rkrishnan.org/files/wadler-1985.pdf))

[2] [Making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable)

[3] [Parametricity (and theorems for free)](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)
