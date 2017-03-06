---
title: Z Curve
packages: [ 'file2img', 'ghc', 'imagemagick', 'wrapCode' ]
dependencies: [ 'static/procedural/Pic.hs'     ,
                'static/procedural/greyCode'   ,
                'static/procedural/includePic' ,
                'static/procedural/colourCode' ]
---

<style type="text/css">
.x {
  background: #000;
  color: #FFF;
}

.y {
  background: #FFF;
  color: #000;
}
</style>

```{pipe="sh > /dev/null"}
ln -s ./root/static/procedural/Pic.hs Pic.hs
```

The Z curve is a line which zig-zags through a space, going through all points in a particular order. It's a nice, quick way to enumerate points in a way which sort-of preserves locality, or 'closeness', between points in the space and points on the resulting line. Its locality isn't as good as the Hilbert curve's, but the Z curve is embarassingly simple to understand:

Given a point with coordinates `(x, y)`, we can find out its position on the Z curve by writing `x` and `y` in binary, interleaving their bits, then reading back the resulting number.

For example, the point `(` `5`{.x} `, ` `10`{.y} `)` can be written in binary as `(` `0101`{.x} `, ` `1010`{.y} `)`. If we interleave these, we get `0`{.x} `1`{.y} `1`{.x} `0`{.y} `0`{.x} `1`{.y} `1`{.x} `0`{.y}, which converted to decimal is `102`.

To convert back to `(x, y)` coordinates we just read the even and odd bits, respectively.

The `interleave`{.haskell} function will interleave lists of `Bits`{.haskell} whilst the `outerleave`{.haskell} function will extract lists of `Bits`{.haskell} from a single list:

```{.haskell pipe="tee -a bw.hs | tee -a rgb.hs"}
interleave :: [Bits] -> Bits
interleave l = case l of
                    []        -> []  -- No lists to interleave
                    []    :_  -> []  -- We abort as soon as an empty list is spotted
                    (x:xs):ys -> x : interleave (ys ++ [xs]) -- Extract x, send xs to the end

outerleave :: Int -> Bits -> [Bits]
outerleave n xs = let (heads, xs') = splitAt    n xs          -- Take one bit for each list
                      tails        = outerleave n xs'         -- Recurse to get the rest
                      lists        = zipWith (:) heads tails  -- Prepend the heads to their tails
                   in if length xs < n
                         then replicate n []                  -- Base case
                         else lists

```

For example, we can colour our Z curve with a black-to-white gradient, then zig-zag it across a 2D space:

```{.haskell pipe="tee -a bw.hs"}
f x y = adjust . fromBits $ interleave [toBits y, toBits x]

```

Since the interleaved number has twice as many bits, we must `adjust`{.haskell} it to fit in the `import Pic; main = print (2 ^ scale)`{pipe="runhaskell"} range we're using:

```{.haskell pipe="tee -a bw.hs"}
adjust = (`div` (2 ^ scale))

```

This results in the following image:

```{.unwrap pipe="sh"}
./root/static/procedural/greyCode < bw.hs | runhaskell > bw.ppm
./root/static/procedural/includePic bw | wrapCode .unwrap | pandoc -t json
```

Notice that the gradient is quite smooth: large discontinuities are rare, common discontinuities are small. That's because the curve has good locality.

## Higher Dimensions ##

Since the Z curve works for any number of dimensions, we can zig-zag it through the 3D RGB colour space, where the `(x, y, z)` coordinates we interleave correspond to red, green and blue colour intensities respectively.

Like before, we interleave the `x` and `y` coordinates of our pixels to get their position on the Z curve:

```{.haskell pipe="tee -a rgb.hs"}
f x y = rgbCube $ interleave [toBits y, toBits x]

```

To get our red, green and blue values, we just 'outerleave' three numbers from this position:

```{.haskell pipe="tee -a rgb.hs"}
rgbCube xs = let [r, g, b] = outerleave 3 xs
              in (adjust r, adjust g, adjust b)

```

Again, we adjust the colours to fit in our range. This time it's not strictly necessary; since they're only using 2/3 of the allowed bits, all values will be in range, but they'll be quite dark so we scale them up:

```{.haskell pipe="tee -a rgb.hs"}
adjust = let cubeScale = floor $ fromIntegral (scale * 2) / 3
             ratio     = 2 ^ (scale - cubeScale)
          in (* ratio) . fromBits

```

This gives us the following 2D embedding of the RGB colour cube:

```{.unwrap pipe="sh"}
./root/static/procedural/colourCode < rgb.hs | runhaskell > rgb.ppm
./root/static/procedural/includePic rgb | wrapCode .unwrap | pandoc -t json
```

This isn't as smooth as the greyscale, since the problem is harder: we don't have the luxury of an *extra* dimension, like the greyscale example. Instead, we have to *remove* a dimension from the RGB cube. Many discontinuities follow the same pattern as the greyscale example, eg. the image being split into quadrants. The tartan-like banding is due to the cube's neighbours having to rearrange themselves in the constrained 2D space.
