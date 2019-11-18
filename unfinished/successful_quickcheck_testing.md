---
title: Successful Testing with QuickCheck (and Similar Property Checkers)
---

Unit testing is existential

Property checking is universal

QuickCheck uses random generators

SmallCheck enumerates, LazySmallCheck searches, etc.

I was inspired to write this post when optimising some data analysis code.
After waiting a couple of days for it to finish, I decided to revisit the code
to see if there were any "obvious" performance gains to be had. (Remember:
nothing is obvious when optimising; always measure!)

The code contains a global mapping from "identifiers" (implemented as `String`)
to sets of "symbols" (implemented as `[Data.Text.Text]`). For example:

    [ (id1, [s1, s3]    )
    , (id2, [s2, s3, s4])
    , (id3, [s3]        )
    ]

This mapping is always the same, and is read from a file at compile-time using
[TemplateHaskell](). At run-time we are given a set of symbols as input, for
example:

    [s3, s1, s2]

Our goal is to return any IDs from the mapping which correspond to (non-strict)
subsets of our input. In the above example we should return the set
`[id1, id3]`. This is because `id1` and `id3` map to subsets of `[s3, s1, s2]`
(they map to `[s1, s3]` and `[s3]`, respectively). We do not include `id2`
since it maps to `[s2, s3, s4]` which is not a subset of `[s3, s1, s2]`.

When I began this round of optimisation, the code looked something like the
following (where `globalMapping` is defined by some TemplateHaskell that I've
elided):

```
import qualified Data.List as L
import qualified Data.Text as T

type Symbol = T.Text
type ID     = String

-- | Lists which we assume are sorted. This is for documentation and to help us
--   follow what's going on; it is not checked or enforced!
newtype AscendingList a = AscendingList { unAsc :: [a] }

-- | Check whether the given list is a subset of the given AscendingList. This
--   is faster than comparing two unordered lists, since we can short-circuit
--   if a small element isn't found near the start of the AscendingList.
subset :: Ord a => [a] -> AscendingList a -> Bool
subset xs ays@(AscendingList ys) = case xs of
    []     -> True
    (x:xs) -> (x `isIn` ys) && (xs `subset` ays)
  where x `isIn` []     = False
        x `isIn` (y:ys) = case x `compare` y of
                            LT -> False  -- x < y implies all (x <) ys
                            EQ -> True
                            GT -> x `isIn` ys

idsFor :: [Symbol] -> [ID]
idsFor input = map fst (filter match globalMapping)
  where match x = snd x `subset` input
```

There are a few potential optimisations I can spot right away. First of all, we
can try combining the `map` and `filter` calls in `idsFor`: naively we might
think that this performs two list traversals, but Haskell's demand-driven
evaluation should avoid this by interleaving the processing; allowing elements
to be generated and discarded one at a time. Likewise, GHC is able to fuse list
operations into tight loops. Yet I'm unsure if how well this is optimising,
since each traversal uses a different projection function (`fst` and `snd`).
Time to do some measurement!



I immediately saw a potential improvement: the `subset` function relies on the
second argument being sorted; but we can perform more short-circuiting if the
first argument is also sorted. Since each `datum` will be `match`ed against
hundreds of potential subsets, it makes sense to sort them once and use an
optimised `subset` for all of those checks:

```haskell
-- | Check whether the first AscendingList is a subset of the second. This is
--   faster than comparing with (one or two) unordered lists, since we can
--   discard small elements as they're encountered, knowing they won't appear
--   further down the lists.
subset :: Ord a => AscendingList a -> AscendingList a -> Bool
subset (AscendingList l1) (AscendingList l2) = go l1 l2
  where go []     _          = True
        go _      []         = False
        go a@(x:xs) b@(y:ys) = case x `compare` y of
                                 LT -> False
                                 EQ -> go xs b
                                 GT -> go a  ys

getIDsFor :: [Symbol] -> [ID]
getIDsFor datum = map fst (filter match globalMapping)
  where sorted          = AscendingList (L.sort datum)
        match (_, syms) = syms `subset` sorted
```


```
-- We arrange the theorem dependencies into a trie to speed up subset finding.
-- The dependencies are sorted, then the first level of the trie contains a list
-- for each smallest-element, e.g. [(t1, [A, B]), (t2, [B, D]), (t3, [E]),
-- (t4, [E, F])] gives
--
--   +-- A ----- B --- t1
--   |
-- --+-- B ----- D --- t2
--   |
--   |       +-- t1
--   |       |
--   +-- E --+
--           |
--           +-- F --- t4

data Trie a b = TrieNode [a] (AscendingList (b, Trie a b))

zTrie :: Ord b => Trie a b
zTrie = TrieNode [] (AscendingList [])

addTrie :: forall a b. Ord b => a -> AscendingList b -> Trie a b -> Trie a b
addTrie tid (AscendingList deps) (TrieNode tids (AscendingList branches)) =
    case deps of
      []   -> TrieNode (tid:tids) (AscendingList branches)
      d:ds -> TrieNode      tids  (   go d ds [] branches)

  where go :: Ord b => b -> [b] -> [(b, Trie a b)] -> [(b, Trie a b)]
                    -> AscendingList (b, Trie a b)
        go d ds acc bss = case bss of
          []        -> fin ((d, addTrie tid (AscendingList ds) zTrie):acc)
          (x, b):bs -> let f elem trie rest = fin $ concat ([
                                         [(elem,
                                           addTrie tid (AscendingList ds) trie)]
                                       , rest
                                       , acc
                                       ]) ++ bs
                        in case d `compare` x of
                             LT -> f d zTrie [(x, b)]
                             EQ -> f x b     []
                             GT -> go d ds ((x, b):acc) bs

        fin = AscendingList . reverse

listToTrie :: forall a b. Ord b => [(a, [b])] -> Trie a b
listToTrie = go zTrie . map sortDeps
  where sortDeps :: Ord b => (a, [b]) -> (a, AscendingList b)
        sortDeps (t, deps) = (t, sortUniq deps)

        go :: Trie a b -> [(a, AscendingList b)] -> Trie a b
        go acc []              = acc
        go acc ((t, deps):tds) = go (addTrie t deps acc) tds

sortUniq :: forall a. Ord a => [a] -> AscendingList a
sortUniq = AscendingList . List.foldl' insertIfMissing []
  where insertIfMissing :: Ord a => [a] -> a -> [a]
        insertIfMissing []     x = [x]
        insertIfMissing (y:ys) x = case x `compare` y of
                                     LT -> x:y:ys
                                     EQ ->   y:ys
                                     GT ->   y:insertIfMissing ys x

theoremsToTrie :: [(TheoremID, [Name])] -> Trie TheoremID Name
theoremsToTrie = listToTrie

trieSubsets :: Ord b => AscendingList b -> Trie a b -> [a]
trieSubsets (AscendingList deps) t = go [] t deps
  where go acc (TrieNode ids branches) ds = go' (ids ++ acc) (unAsc branches) ds

        go' !acc []            _      = acc
        go' !acc _             []     = acc
        go' !acc l@((x, b):bs) (d:ds) = case d `compare` x of
                                          -- Ignore d and try again
                                          LT -> go' acc           l  ds

                                          -- Look in b; also look for ds in bs
                                          EQ -> go' (go acc b ds) bs ds

                                          -- Skip b and look in bs
                                          GT -> go' acc bs (d:ds)

```

```haskell
buildTrie = testProperty "Can build trie of theorem dependencies" go
  where go :: [Dep] -> Dep -> [Dep] -> Natural -> [Dep] -> Property
        go deps d ds n extra =
          let -- Prepend d to ensure the list is non-empty. We will use this as
              -- a set of dependencies for a theorem, then try looking it up.
              want = d:ds

              -- Pick a theorem ID to associate with want. We will assign IDs
              -- sequentially, so this can't be larger than the total number of
              -- deps sets (including want).
              wantID = fromIntegral n `mod` (length deps + 1)

              -- Insert want into deps such that it gets assigned to wantID
              deps' :: [Int]
              deps' = map fixDeps $
                        take wantID deps ++ [want] ++ drop wantID deps

              -- Assign IDs using zip and build a trie
              trie = Helper.listToTrie (zip [0..] deps')

              -- Our query should work for any superset of want, so we allow it
              -- to be extended with extra deps to check this.
              query = fixDeps (want ++ extra)

              -- Look up theorem IDs whose deps are subsets of our query
              got = Helper.trieSubsets trie query

              -- Querying for a superset of want should always find wantID
              foundWant = counterexample (show (("error" , "Didn't get wantID")
                                               ,("want"  , want               )
                                               ,("wantID", wantID             )
                                               ,("query" , query              )
                                               ,("got"   , got                )
                                               ,("deps'" , deps'              )
                                               ,("trie"  , trie               )
                                               ,("extra" , extra              )
                                               ))
                                         (property (wantID `elem` got))

              -- Only subsets of our query should be returned
              gotOnlySubsets []     = property True
              gotOnlySubsets (t:ts) = counterexample
                                        (show (("error" , "Result not in query")
                                              ,("query" , query                )
                                              ,("result", t                    )
                                              ,("got"   , got                  )
                                              ,("all"   , map (deps' !!) got   )
                                              ))
                                        (property (all (`elem` query)
                                                       (deps' !! t))) .&&.
                                      gotOnlySubsets ts

           in (property (wantID `elem` got)) .&&.
              (gotOnlySubsets got)

        -- Removes dupes and sorts, turning a list of names into valid deps
        fixDeps :: [Int] -> [Int]
        fixDeps = nub . sort
```
