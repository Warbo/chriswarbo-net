---
title: Bitwise Equality
---

We get the inverse of this image if we use `/=`{.haskell}, AKA XOR.

```{pipe="sh"}
cp -ar root/data/procedural/* .
```

```{.unwrap pipe="./codeAndPic eq grey"}
f = bitWise $ zipWith (==)
```
