Most of the testing I do is integration testing, using property checkers like QuickCheck. It's remarkable how well this exposes problems I would never have thought to write explicitly (as a unit test, for example).

It's easy to think of property checking as a minor generalisation of unit tests, where we replace hard-coded values/actions with free variables, e.g. turning a test like:

    let key   = "hello"
        value = "world"
        db    = addValue(key, value, newDB())
        found = lookup(keyQuery(key), db)
     in assertEqual(found, [value])

Into something like:

    function(key, value) {
      let db    = addValue(key, value, newDB())
          found = lookup(keyQuery(key), db)
       in assertEqual(found, [value])
    }

This is certainly better, since we've removed some irrelevant constraints from the test (the particular choice of key and value). However, we can go so much further gets even more interesting when we think about all of the *implicit* actions/values that are involved; or which irrelevant constraints are implicitly restricting our tests.

In this example we're using `newDB`, presumably to avoid collision with an existing key, which would cause multiple matches to be returned. However, that constraint is unnecessary; we might as well remove it, since we'll get a stronger test that may uncover more errors:

    function(key, value, initialDB) {
      let db    = addValue(key, value, initialDB)
          found = lookup(keyQuery(key), db)
          # We only care that `found` contains `value`
       in assertTrue(contains(value, found))
    }

Many people would stop here, since those are all of the *explicit* constraints. However, there are still more unnecessary constraints that are *implicit*. Firstly, our query is overly restricted; as long as our key lookup is in there somewhere, it shouldn't matter what else we lookup:

    function(key, value, initialDB, preQ, postQ) {
      # preQ and postQ are lists of queries to add on to ours
      let db    = addValue(key, value, initialDB)
          # Disjunction (OR) should only ever add to the result
          query = disjunctionQuery(append(preQ,
                                          [keyQuery(key)],
                                          postQ))
          found = lookup(query, db)
       in assertTrue(contains(value, found))
    }

Next, we're not allowing any modifications to the DB in between `addValue` and `lookup`. Adding to the DB shouldn't affect our test, so we should check that (taking in a list of key/value pairs `extras`):

    function(key, value, initialDB, preQ, postQ, extra) {
      # Take a list of key/value pairs `extra`
      let db       = addValue(key, value, initialDB)
          # Combine each `extra` with the DB using `addValue`
          modified = reduce(addValue, db, extra)
          query    = disjunctionQuery(append(preQ,
                                             [keyQuery(key)],
                                             postQ))
          found    = lookup(query, modified)
       in assertTrue(contains(value, found))
    }

Removing values shouldn't alter our result either, unless they match our key; likewise the interleaving of additions and removals shouldn't matter:

    function(key, value, initialDB, preQ, postQ, extras, removals) {
      # Rather than having one list of additions and one of removals, they
      # are both lists-of-lists. This allows arbitrary interleaving.
      let db       = addValue(key, value, initialDB)
          chunks   = interleave(extras,
                                # Remove `key` from any removal chunk
                                map(filter(notEqual(key)), removals))
          # Apply the adds and removes one "chunk" at a time; `mode` toggles
          # between add and remove; `first` discards the final mode
          modified = first(reduce(function(chunk, (mode, db)) {
                                    (not mode,
                                     reduce(if mode
                                               then addValue
                                               else removeKey,
                                            db,
                                            chunk)
                                  },
                                  db,
                                  chunks)
          query    = disjunctionQuery(append(preQ,
                                             [keyQuery(key)],
                                             postQ))
          found    = lookup(query, modified)
       in assertTrue(contains(value, found))
    }

This test is far stronger than the unit test we began with, since it generalises a lot of things we might have missed if we didn't think carefully. This is more likely to find problems caused by weird sequences and interleavings of actions, which we probably wouldn't think to test in isolation.

In this case I think the interleaving of arbitrarily-many rounds of insertion and deletion isn't really worth the extra complexity. I would either pull this out into a library function (if it's sufficiently general/useful), or otherwise I'd just do a couple of rounds each (so we have adds sandwiched between removes, and vice versa):


    function(key, value, initialDB,
             preQ, postQ,
             extra1, extra2,
             remove1, remove2) {
      let db    = addValue(key, value, initialDB)

          db2   = reduce(addValue,  db,  extra1)
          db3   = reduce(removeKey, db2, filter(notEqual(key), remove1))
          db4   = reduce(addValue,  db3, extra2)
          db5   = reduce(removeKey, db4, filter(notEqual(key), remove2))

          query = disjunctionQuery(append(preQ, [keyQuery(key)], postQ))
          found = lookup(query, modified)
       in assertTrue(contains(value, found))
    }

I certainly make heavy use of the `preQ`/`postQ` idiom of embedding the feature under test within a bunch of irrelevant values.
