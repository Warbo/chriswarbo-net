---
title: Bitwise Equality
dependencies: [ 'static/procedural/codeAndPic' ,
                'static/procedural/greyCode'   ,
                'static/procedural/includePic' ,
                'static/procedural/Pic.hs'     ]
packages: [ 'imagemagick', 'file2img', 'ghc', 'wrapCode' ]
---

We get the inverse of this image if we use `/=`{.haskell}, AKA XOR.

```{.unwrap pipe="./root/static/procedural/codeAndPic eq grey"}
f = bitWise $ zipWith (==)
```
