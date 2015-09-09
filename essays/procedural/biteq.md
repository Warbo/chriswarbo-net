---
title: Bitwise Equality
---

We get the inverse of this image if we use `/=`{.haskell}, AKA XOR.

```{.unwrap pipe="./root/static/procedural/codeAndPic eq grey"}
f = bitWise $ zipWith (==)
```
