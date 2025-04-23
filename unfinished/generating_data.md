Want something that captures the idea of Gen (QuickCheck, falsify, Hedgehog), as
well as FEAT, Smallcheck, Lazy Smallcheck, Leancheck, etc.

Alternative is almost there, but:

 - Associativity is hard.
 - falsify Gen is not Alternative

Unfolder tries to address this with its 'choose' that uses lists. Maybe NonEmpty
would make sense?

One way to implement Alternative for falsify Gen is to compose it with Maybe:

 - 'Gen (Maybe a)' can have 'empty = pure Nothing', but it requires a lot of
   wrapping and unwrapping of 'Just', and overlaps with the Alternative instance
   for Compose.
 - 'Maybe (Gen a)' can have 'empty = Nothing'. We can compose generators without
   running them. Works out of the box with instances for 'Compose Maybe Gen'.

An alternative encoding is 'Compose ((,) m) Gen', where 'Monoid m' and
'empty = (mempty, undefined)'. We can get a similar behaviour to
'Compose Maybe Gen' if m is booleans combined with OR. This isn't the nicest
encoding; might be cleaner to use 'Maybe (m, Gen a)', where 'Just (x, g)' is a
particular generator 'g' paired with some monoidal value 'x'; and 'Nothing' is
taken to mean the monoidal value mempty. Can we just compose three functors?
'Compose Maybe (Compose ((,) a) Gen)'?

Whichever way we encode it, that representation generalises in interesting ways.
For example, we can use numbers to calculate various quantities as we go:

 - 'pure x = Just (Product 1, x)' will count the number of possible combinations
   with 'f <*> x' multiplying them, whilst 'x <|> y' adds them.
 - 'pure x = Just (Sum 0, x)' counts the number of choices to make, with
   'f <*> x' having to choose 'f' and 'x', whilst each 'x <|> y' increments.
 - Having <|> take the min and <*> take the max gives the minimum depth (i.e.
   before we reach a leaf)

Maybe the parser/gen combo used in that PhD thesis?
