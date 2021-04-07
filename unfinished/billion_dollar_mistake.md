---
title: Billion Dollar Mistake
---

Sometimes we need to indicate the lack of a result. For example, let's say we're writing a `lookup` function for a `KeyValueMap(K, V)`, what should we do if there is no entry for the given key?

## A Zero Dollar Mistake (My Preferred Approach) ##

Sometimes we have a value to return, sometimes we have no value to return. We can represent both possibilities using a list[1]:

    function lookup(m: KeyValueMap(K, V), key: K): List(V) {
      return contains(m, key)? [m[key]] : [];
    }

We can use functions like `map` to handle both possibilities at once, e.g. `map(areaCode, lookup(phoneBook, myCustomer))` will return a list that's either empty, or contains a single area code. If multiple

    function

This is my preferred solution. Some languages let us (or libraries) define our own list-like types, in which case we should[2] replace `List` (which may contain any number of values) with some `ListOfAtMostOne` type (containing either zero or one values). Many languages/libraries provide such a type, usually with a nicer name like `Maybe` or `Option`.

## A One Dollar Mistake ##

A different approach is to return some "default" value of type `V`, if the entry isn't found.

We want our `KeyValueMap` to be generic[3] (i.e. work the same for all types `V`) so we can't hard-code a default, since we don't know what type `V` will be. We *can* take a default value as an extra argument, which gives us something like:

    function lookup(m: KeyValueMap(K, V), key: K, default: V): V {
      return contains(m, key)? m[key] : default;
    }

This works, but everyone using it will need to pick a default value, which must be distinguishable from any entry in the map. For example, let's say we do `lookup(myMap, myKey, -1)` and it returns `-1`: does that mean `myKey` wasn't found, or that `myMap[myKey]` has the value `-1`? There's not enough information, so in general it's impossible to tell the difference (in fact, it's a good idea to make types as specific as possible[https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/], which would eliminate such "defaults").

## A Thousand Dollar Mistake ##

Another approach is to change the language so types can be "nullable". This extends types with an extra value called `null`, and is sometimes written `?T` to mean "`T` extended with the value `null`". If we think of types as sets, then `?T = T ∪ {null}` (the union of `T` with `{null}`).

We can use this for our `lookup` function, by returning a nullable type, and always using `null` as the default:

    function lookup(m: KeyValueMap(K, V), key: K): ?V {
      return contains(m, key)? m[key] : null;
    }

Unfortunately there are a couple of problems with `null`. Let's say we want a function which takes in a `Person` and returns their mother's `PhoneNumber`:

    function mothersPhoneNumber(p: Person): ?PhoneNumber {
      mother = lookup(motherMap, p);
      return isNull(mother)? null : lookup(phoneBook, mother);
    }

The first problem

## The Billion Dollar Mistake ##

Nullable

---

References:

[1] [How to replace failure by a list of successes](https://link.springer.com/chapter/10.1007%2F3-540-15975-4_33) ([PDF here](https://rkrishnan.org/files/wadler-1985.pdf))

[2] [Making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable)

[3] [Parametricity (and theorems for free)](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)
