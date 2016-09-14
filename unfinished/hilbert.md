---
title: Hilbert Curve
---

The [Hilbert curve](https://en.wikipedia.org/wiki/Hilbert_curve) is similar to the [Z curve](z.html), but has better locality: on average, points which are close together in space will appear closer together on a Hilbert curve through that space than on a Z curve.

The downside of the Hilbert curve is that it's slightly more complicated to construct than the Z curve.

```{.haskell pipe="tee -a hilbert.hs"}
type Bit  = Bool
type Bits = [Bit]

log2 :: Int -> Int
log2 n = if n < 2
            then 1
            else 1 + log2 (n `div` 2)

i2B dim 0 = take (log2 dim) (repeat False)

bAnd :: Bits -> Bits -> Bits
bAnd = zipWith (&&)

xy2d :: Int -> Bits -> Bits -> Bits
xy2d dim x1 y1 = loop (dim `div` 2) 0 x1 y1
  where
    loop s d x y =
      let rx = or (bAnd x s)
          ry = or (bAnd y s)
          (x', y') = rot s x y rx ry
       in if s <= 0
             then d
             else loop (s `div` 2)
                       (d + (s * s * ((3 * rx) ^ ry)))
                       x' y'

{-
d2xy dim d *x *y =
    int rx, ry, s, t=d;
    *x = *y = 0;
    for (s=1; s<dim; s*=2) {
        rx = 1 & (t/2);
        ry = 1 & (t ^ rx);
        rot(s, x, y, rx, ry);
        *x += s * rx;
        *y += s * ry;
        t /= 4;
    }
-}

rot dim x y rx ry =
    case (rx, ry) of
      (1, 0) -> (dim - 1 - y, dim - 1 - x)
      (_, 0) -> (          y,           x)
      (_, _) -> (          x,           y)
```

```{pipe="ghci"}
:load hilbert.hs
```
