---
title: Negatives
---

 - If a situation is easily modelled using Nat then we should stick to that, and
not introduce negative numbers, fractions, complex numbers, etc. since they're
not needed.
    - Example: Alice has 2 apples, Bob gives Alice 3 apples, how many apples
does Alice have?
    - Alice can't have 'negative apples', and Bob can only increase their apple
count, so negatives aren't needed (and likewise for fractional apples,
imaginary apples, infinitesimal apples, transfinite cardinals of apples,
etc.)
    - Note that many programming languages get this wrong: e.g. having 'length'
return a Z, forcing callers to account for the negative case.
 - When a situation *does* make sense with negatives, we *should* use them.
    - For example: if Alice has 3 apples and gives 2 to Bob, how many apples
does Alice have left?
