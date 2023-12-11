---
title: "A New Numeric Tower: Negatives And Inverses"
---

TODO

 - Parse unicode COMBINING OVERLINE
   - Allow negative indeterminates, GA units, etc. so long as they're completely
     overlined (i.e. `d₀` needs a combining character for the `d` and another
     for the `₀`)
 - Allow partially-negative literals
   - Interpret these as negative digits
 - Can we just parse by peeking for a COMBINING OVERLINE and, if coming up,
   consuming everything until we hit a char without a subsequent COMBINING
   OVERLINE?
   - Recursively parse every other character, and negate the result
   - Would need special-case to handle negative digits
 - Ambiguity for reciprocal of a negative, e.g. `3̅²̅` could mean
   `(= (- (expt 3 2)) 9̅)` or `(= (expt 3̅ 2̅) 1/9)`
   - The former would be the result of a simple every-other-character parser
   - The latter can be represented with `3̅⁻²`
