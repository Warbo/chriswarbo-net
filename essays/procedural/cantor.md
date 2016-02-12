---
title: Cantor Tuples
---

<!-- Preamble -->

<!-- I haven't figured out how to escape backticks in inline code yet -->

```{pipe="tee replaceTicks > /dev/null"}
#!/usr/bin/env bash
sed 's/TICK/`/g'
```

<!-- Shorthand for appending code to our main Haskell file -->

```{pipe="cat > code"}
#!/usr/bin/env bash
tee -a     code.hs
echo "" >> code.hs
```

<!-- Run Haskell code in the context of code.hs
     ExistentialQuantification lets allTests be heterogeneous
  -->

```{pipe="cat > haskell"}
#!/usr/bin/env bash
nix-shell -E 'with import <nixpkgs> {}; runCommand "dummy" { buildInputs = [ (haskellPackages.ghcWithPackages (p: [p.QuickCheck p.ghc])) ]; } ""' --run "runhaskell -XExistentialQuantification"
```

```{pipe="cat > run"}
#!/usr/bin/env bash
code=$(cat)
(cat code.hs; echo ""; echo "$code") | ./haskell

```

```{pipe="cat > runMono"}
#!/usr/bin/env bash
code=$(cat)
(cat code.hs; echo ""; echo "$code") | ./root/static/procedural/monoCode | ./haskell
```

```{pipe="cat > runGrey"}
#!/usr/bin/env bash
code=$(cat)
(cat code.hs; echo ""; echo "$code") | ./root/static/procedural/greyCode | ./haskell
```

```{pipe="cat > runRgb"}
#!/usr/bin/env bash
code=$(cat)
(cat code.hs; echo ""; echo "$code") | ./root/static/procedural/colourCode | ./haskell
```

```{pipe="bash > /dev/null"}
chmod +x code replaceTicks runGrey run runMono runRgb haskell
ln -s ./root/static/procedural/Pic.hs Pic.hs
```

<!-- Preamble -->

```{pipe="./code > /dev/null"}
import           Prelude hiding (pi)
import           Pic
import           Data.List
import qualified Data.Map as DM
import           Test.QuickCheck

-- Unifies all Testable types into one
data T = forall a. Testable a => T a

instance Testable T where
  property (T x) = property x
```

<!-- Content -->

Cantor pairs (and triplets, or tuples in general) are way to enumerate multi-dimensional spaces, similar to the [Z curve](z.html). Unlike the Z curve, Cantor Tuples don't preserve locality very well.

The idea is very simple: we trace a curve back and forth across the space until we've covered the whole thing. Here are the definitions we'll be using below:

 - `type Dimensions = Int`{.haskell pipe="./code"}
    - How many `Dimensions`{.haskell} to work in
 - `type Range = Int`{.haskell pipe="./code"}
    - A bounded or unbounded `Range`{.haskell} of coordinates
 - `range n = if n == 0 then [0..] else [0..n]`{.haskell pipe="./code"}
    - A way to enumerate a `Range`{.haskell}
 - `type Point = [Int]`{.haskell pipe="./code"}
    - Storing *lists* of coordinates rather than tuples lets us use any number of `Dimensions`{.haskell}
 - `type Curve = [Point]`{.haskell pipe="./code"}
    - A `Curve`{.haskell} is a line traced through the space
 - `data Shape = Shape {sDim :: Dimensions, sRange :: Range, sPred :: Point -> Bool}`{.haskell pipe="./code"}
    - We define a `Shape`{.haskell} using a *predicate*, deciding whether a `Point`{.haskell} is in the `Shape`{.haskell} or not

```{pipe="./code > /dev/null"}
cPx = [[x, y] | n <- [0..10], x <- [0..n], y <- [0..n], x + y == n]

instance Show Shape where
  show s = "Shape {sDim " ++ show         (sDim   s) ++
              ", sRange " ++ show         (sRange s) ++
              ", sPred "  ++ show (filter (sPred  s) (take 10 cPx)) ++ "}"
```

## Axis-aligned `Curves`{.haskell} ##

The most obvious way to trace a `Curve`{.haskell} across a `Shape`{.haskell} is to start at `(0, 0)` (which for our purposes will be the *top* left of the images) and increase, say, the `x` coordinate to get `(1, 0)`, then `(2, 0)`, etc. until we exhaust the `Shape`{.haskell}'s `Range`{.haskell}, then jump to `(0, 1)` and do the same thing, and so on:

```{.haskell pipe="./code"}
aaCurve :: Range -> Dimensions -> Curve
aaCurve r 1 = [[x]  | x <- range r]
aaCurve r n = [x:xs | x <- range r, xs <- aaCurve r (n - 1)]

aaShape :: Shape -> Curve
aaShape (Shape d r p) = filter p (aaCurve r d)
```

If we use this to trace a black-to-white gradient across a square image, we get this:

```{pipe="./code > /dev/null"}
img    = Shape 2 dim (const True)

white  = dim

pixelsOf f shape = let curve  = f shape
                       count  = length curve
                       greys  = [(i * white) `div` count | i <- [0..]]
                    in DM.fromList $ zip curve greys

aaPixels = pixelsOf aaShape

boxPixels = aaPixels img

aaGrad x y = let this   = DM.lookup [x, y] boxPixels
              in maybe (error "Out of range") id this
```

```{pipe="./runGrey > aagrad.ppm"}
f = aaGrad
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic aagrad | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

### A Finite Example ###

Let's say we have a `circle`{.haskell} (where `dim = 2 ^ scale =`{.haskell} `main = print dim`{.haskell pipe="./run"} is the width and height of the following images):

```{.haskell pipe="./code"}
pythagoras' :: Int -> Int -> Int
pythagoras' a b = a ^ 2 + b ^ 2  -- There's no need to take the square root

circle :: Shape
circle = let centre = dim `div` 2
             p [x, y] = pythagoras' (x - centre) (y - centre) < (centre ^ 2)
          in Shape 2 (2 * centre) p
```

Here's what we get when we use an axis-aligned curve to trace a black-to-white gradient through the `circle`{.haskell}:

```{.haskell pipe="./code"}
circlePixels = aaPixels circle

drawCircle x y = let this = DM.lookup [x, y] circlePixels
                  in maybe 255 id this

```

```{pipe="./runGrey > circ.ppm"}
f = drawCircle
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic circ
```

<div class="togglable odd" style="cursor: pointer;">

#### Aside ####

<div class="toggled">

Another thing we can do with `circle`{.haskell} is to approximate pi:

 - `pi * r^2`{.haskell} is the area of any circle
    - `r = dim TICKdivTICK 2`{.haskell pipe="./replaceTicks"} in our example
 - `sRange s ^ sDim s`{.haskell} is the area of the bounding box of `s :: Shape`{.haskell}
    - `sRange circle = dim = 2 * r`{.haskell}
    - `sDim   circle = 2`{.haskell}
    - `boxArea = (2 * r)^2 = 4 * r^2`{.haskell} for `circle`{.haskell}
 - `circleArea / boxArea = (pi * r^2) / (4 * r^2) = pi / 4`{.haskell} follows from simple algebra
    - Therefore `pi = 4 * circleArea / boxArea`{.haskell pipe="./code"}
 - Since each `Point`{.haskell} is an uniform distance from its neighbours, counting how many are in a `Shape`{.haskell} is a measure of the `Shape`{.haskell}'s area
    - `aaArea     = length . aaShape`{.haskell pipe="./code"} gives us the area inside a `Shape`{.haskell}
    - `circleArea = fromIntegral $ aaArea circle`{.haskell pipe="./code"}
    - `boxArea    = fromIntegral $ aaArea (Shape (sDim circle) (sRange circle) (const True))`{.haskell pipe="./code"}
 - Plugging these values into our definition of pi gives `cat code.hs; echo ""; echo "main = print pi"`{.haskell pipe="bash | ./haskell"}
    - Increasing the radius decreases the error, since the sampling gives a less 'jagged' approximation of our circle

  </div>
</div>

<script src="/js/jquery.js"></script>
<script type="text/javascript">
$('.togglable').click(function() {
                        $('.toggled', $(this)).toggle();
                      })
               .click();
</script>

### An Unbounded Example ###

What happens if our `Range`{.haskell} is unbounded (ie. `range 0`{.haskell})? For example, we might define an infinitely-repeating pattern:

```{.haskell pipe="./code"}
checkerboard :: Shape
checkerboard = let f        = (`mod` 2) . (`div` 8)
                   g [x, y] = f x == f y
                in Shape 2 0 g
```

Obviously we can't draw the whole pattern, but we can draw a small section:

```{.haskell pipe="./code"}
drawBoard x y = sPred checkerboard [x, y]
```

```{pipe="./runMono > board.ppm"}
f = drawBoard
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic board
```

Since the area of `checkerboard`{.haskell} is infinite, so is the length of a `Curve`{.haskell} traced through it. However, an infinite `Curve`{.haskell} returned by `aaCurve` will *not* contain every `Point`{.haskell} in the `Shape`{.haskell}.

This is because we only add a `Point`{.haskell} from the second row once we've reached the end of the first row; since the first row never ends, we'll never add a `Point`{.haskell} from any other row!

## Diagonal Traces ##

Cantor's approach handles infinite patterns like `checkerboard`{.haskell} by tracing *diagonal* lines across the shape, from one coordinate axis to another:

 - Each `Point`{.haskell} on the `x` axis is the start of a diagonal line
 - To get from one `Point`{.haskell} in a line to the next, we decrement the `x` coordinate and increment the `y`
    - If we have more dimensions, we fix the first coordinate (ie. `x`) and recurse using the rest of the dimensions
 - Once the `x` coordinate hits `0`{.haskell}, we jump to the start of the next line

```{.haskell pipe="./code"}
cantorMax [_]    = True
cantorMax (x:xs) = sum xs == 0

cantorStart n 1 = [n]
cantorStart n d = 0 : cantorStart n (d-1)

cantorNext :: Point -> Point
cantorNext (x:xs) | cantorMax (x:xs) = xs ++ [1+x]
cantorNext (x:xs) | cantorMax xs     = (x + 1) : cantorStart (sum xs - 1) (length xs)
cantorNext (x:xs) | otherwise        = x : cantorNext xs

-- A full curve
cantorCurve :: Range -> Dimensions -> Curve
cantorCurve r d = takeWhile ((<= 2 * r) . sum) (iterate cantorNext (replicate d 0))

cantorShape :: Shape -> Curve
cantorShape (Shape d r p) = filter p (cantorCurve r d)
```

```{pipe="./code > /dev/null"}
-- Sanity checks
cantorNextIncrements =
  ("cantorNext [0, 0, .., 0, n+1] => [0, 0, .., 1, n]",
   T $ \n d -> let prefix = replicate d 0
                   init   = prefix ++ [0, abs n + 1]
                   out    = prefix ++ [1, abs n]
                in cantorNext init == out)

cantorNextBumpsUp =
  ("cantorNext bumps [n, 0, 0, ..] to [0, 0, .., n+1]",
   T $ \n d -> let suffix = replicate d 0
                   init   = abs n : suffix
                   total  = sum (cantorNext init)
                in total == abs n + 1)

cantorNextList =
  ("cantorNext in 1D gives [[0], [1], [2], ..]",
   T $ \n -> take (n + 1) (iterate cantorNext [0]) == [[x] | x <- [0..n]])

cantorNextMonotonic =
  ("cantorNext increases monotonically",
   T $ \n d -> let (x:xs) = iterate cantorNext (replicate (abs d + 1) 0)
                   bits   = zipWith (\a b -> sum a <= sum b) (x:xs) xs
                in all id (take n bits))

cantorNextLength =
  ("cantorNext doesn't alter size",
   T $ \l -> let len = length l
              in len == 0 || len == length (cantorNext l))

-- Start with 0,0,0,..,n, iterate cantorNext, takeWhile ((n ==) . sum) and compare
-- elements with a list comprehension: [[a, b, c, ..] | a <- [0..n], b <- [0..n], .., a + b + .. == n]
```

To see how this works, we can trace a black-to-white gradient across a square, following the curve given by `cantorCurve`{.haskell}. Compare it to the axis-aligned version above:

```{.haskell pipe="./code > /dev/null"}
-- This works for 2D; higher dimensions are more complicated

cantor2DTotal :: Int
cantor2DTotal = sum [1..2 * dim]

cantor2DFrac :: Float
cantor2DFrac = fromIntegral dim / fromIntegral cantor2DTotal

cantor2DPosToGrey :: Int -> Grey
cantor2DPosToGrey = round . (* cantor2DFrac) . fromIntegral

-- Get which line a point is in
cantor2DLine' a n = if n > a
                       then cantor2DLine' (a + 1) (n - a)
                       else (a, n)

cantor2DLine = cantor2DLine' 0

-- The index of a Point in a cantorCurve
cantorIndex2D :: Point -> Int
cantorIndex2D [x, y] = sum [1..x + y + 1] + y

cantorGrad x y = cantor2DPosToGrey (cantorIndex2D [x, y])
```

```{pipe="./runGrey > cantorgrad.ppm"}
f = cantorGrad
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic cantorgrad | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

We start in the top left corner and draw a *diagonal* line from the top edge to the left edge, then draw another next to it, and another next to it, and so on. Note that Cantor's method naturally draws a *triangle*; to make it trace a square, we've had to filter out those points which extend too far to the right or the bottom.

### A Finite Example ###

Let's revisit our `circle`{.haskell}. If we trace a gradient across it using Cantor's method, we get the following:

```{pipe="./code > /dev/null"}
cantorPixels = pixelsOf cantorShape

circleCantorPixels = cantorPixels circle

cantorCircle x y = let this = DM.lookup [x, y] circleCantorPixels
                    in maybe 255 id this
```

```{pipe="./runGrey > cantorcircle.ppm"}
f = cantorCircle
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic cantorcircle | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

### Unbounded Example ###

If we apply this to the checkerboard pattern, we're now guaranteed to reach any finite coordinate in finite time:

```{pipe="./code > /dev/null"}
checkerboardCantorPixels = cantorPixels (Shape (sDim checkerboard) (dim) (sPred checkerboard))

cantorCheckerboard x y = let this = DM.lookup [x, y] checkerboardCantorPixels
                          in maybe 255 id this
```

```{pipe="./runGrey > cantorcheckerboard.ppm"}
f = cantorCheckerboard
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic cantorcheckerboard | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

### Removing All Bounds ###

The checkerboard example is unbounded on the right and bottom, but *does* have a bound on the top and the left. Cantor's method can exploit this to zig-zag across the pattern, but it doesn't *rely* on there being any bounds. Instead of zig-zagging, we can follow a *spiral*, starting from any point, and be guaranteed to eventually reach all points. For example, if we treat the checkerboard as unbounded in *all* directions:

```{pipe="./code > /dev/null"}
allCheckerboardPixels =
  let c    = dim `div` 2
      quad = cantorCurve c (sDim checkerboard)
      all  = [[[c+x,c-y], [c-x,c-y], [c-x,c+y], [c+x,c+y]] | [x,y] <- quad]
      f [x1, y1] [x2, y2] = compare (abs (x1 - c) + abs (y1 - c))
                                    (abs (x2 - c) + abs (y2 - c))
      ord  = sortBy f . filter (sPred checkerboard) . concat $ all
   in pixelsOf id ord

allCheckerboard x y = let this = DM.lookup [x, y] allCheckerboardPixels
                       in maybe 255 id this
```

```{pipe="./runGrey > allcheckerboard.ppm"}
f = allCheckerboard
```

```{.unwrap pipe="bash"}
./root/static/procedural/includePic allcheckerboard | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

<!--

## Dimension Reduction ##

We can use Cantor's method to reduce the dimensions of a shape; for example, we can trace a line through the RBG colour cube to go from 3D to 1D, then trace the same line through an image to get a 2D representation:

```
{pipe="./code"}
rgbCube :: Shape
rgbCube = let pointCount = fromIntegral $ dim * dim
              sideLength = floor $ pointCount ** 1/3
           in Shape sideLength 3 (const True)

rgbTrace :: [[Int]]
rgbTrace = cantorShape rgbCube

rgbIncreasing =
  ("rgbTrace sums increase",
   T $ \n -> n > 0 && n < (length rgbTrace) - 1 ==>
             let x = sum (rgbTrace !! (n - 1))
                 y = sum (rgbTrace !! n)
              in x == y || x + 1 == y)

rgb2D    :: [[Int]]
rgb2D    = cantorShape (Shape scale 2 (const True))

rgbAdjust :: [Int] -> RGB
rgbAdjust [r, g, b] = (round $ fromIntegral r ** 3/2,
                       round $ fromIntegral g ** 3/2,
                       round $ fromIntegral b ** 3/2)

rgbPixels :: DM.Map [Int] [Int]
rgbPixels = DM.fromList $ zip rgb2D rgbTrace

rgbEmbedding :: Int -> Int -> RGB
rgbEmbedding x y = let this = DM.lookup [x, y] rgbPixels
                    in maybe (255,255,255) rgbAdjust this
```

```
{pipe="./root/static/procedural/runRgb > rgb.ppm"}
f = rgbEmbedding
```

```
{.unwrap pipe="bash"}
./root/static/procedural/includePic rgb | ./root/static/wrapCode.sh .unwrap | pandoc -t json
```

-->

<!-- Tests -->

```{pipe="./code > /dev/null"}
-- allTests can contain different types of tests, if they're wrapped in a T
allTests = [cantorNextIncrements, cantorNextBumpsUp, cantorNextList,
            cantorNextMonotonic, cantorNextLength {-, rgbIncreasing -}]

runTests [] = return ()
runTests ((name, test):xs) = putStrLn ("Testing " ++ name) >>
                             quickCheck test               >>
                             runTests xs
```

```{pipe="./run > results"}
main = runTests allTests
```

```{pipe="bash >> /dev/stderr"}
if grep "FAIL" < results
then
  cat results >> /dev/stderr
  exit 1
fi
```
