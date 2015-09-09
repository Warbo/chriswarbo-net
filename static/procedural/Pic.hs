module Pic where

-- Constants
scale  = 7
dim    = 2 ^ scale
pixels = [(x, y) | x <- [0..dim - 1], y <- [0..dim - 1]]

-- Types
type Bits  = [Bool]
type Pixel = (Int, Int)
type Mono  = Bool
type Grey  = Int
type RGB   = (Int, Int, Int)

-- Conversions
showMono :: Mono -> String
showMono True  = "1"
showMono False = "0"

showGrey :: Grey -> String
showGrey = show

showRGB :: RGB -> String
showRGB (r,g,b) = show r ++ " " ++ show g ++ " " ++ show b

toBits' :: Int -> Bits
toBits' 0 = [False]
toBits' 1 = [True]
toBits' n = let (d, m) = n `divMod` 2
             in toBits' m ++ toBits' d

toBits :: Int -> Bits
toBits n = take scale $ toBits' n ++ repeat False

fromBits :: Bits -> Int
fromBits []         = 0
fromBits (True :xs) = 1 + fromBits (False:xs)
fromBits (False:xs) = 2 * fromBits xs

-- Image
header :: Int -> Int -> String
header p c = unlines ["P" ++ show p,        -- Format
                      show dim,             -- Width
                      show dim,             -- Height
                      if c > 0 then show c  -- Colour range
                               else ""]

monoHeader   = header 1 0
greyHeader   = header 2 dim
colourHeader = header 3 dim

renderMono   :: (Int -> Int -> Mono) -> Pixel -> String
renderMono   f (x, y) = showMono (f x y)

renderGrey   :: (Int -> Int -> Grey) -> Pixel -> String
renderGrey   f (x, y) = showGrey (f x y)

renderColour :: (Int -> Int -> RGB)  -> Pixel -> String
renderColour f (x, y) = showRGB (f x y)

monoFrom   f = unlines $ monoHeader   : map (renderMono    f) pixels
greyFrom   f = unlines $ greyHeader   : map (renderGrey    f) pixels
colourFrom f = unlines $ colourHeader : map (renderColour  f) pixels

-- Helpers
bitWise :: (Bits -> Bits -> Bits) -> Int -> Int -> Int
bitWise f x y = fromBits (f (toBits x) (toBits y))

toRGB :: Bits -> RGB
toRGB bs = (fromBits (take scale                   bs),
            fromBits (take scale (drop      scale  bs)),
            fromBits (take scale (drop (2 * scale) bs)))

chunk :: Int -> [a] -> [[a]]
chunk 0 xs = []
chunk 1 xs = [xs]
chunk n xs = let m = length xs `div` n
              in take m xs : chunk (n - 1) (drop m xs)

hue :: Float -> (Float, Float, Float)
hue h = let (i, f) = properFraction (h * 6)
            q = (1 - f)
         in case i of
                 0 -> (1, f, 0)
                 1 -> (q, 1, 0)
                 2 -> (0, 1, f)
                 3 -> (0, q, 1)
                 4 -> (f, 0, 1)
                 5 -> (1, 0, q)

hues :: Int -> [RGB]
hues n = map (toScaleRGB . hue . (/ fromIntegral n) . fromIntegral) [0..n-1]

toScale :: Float -> Grey
toScale = floor . (fromIntegral dim *)

toScaleRGB :: (Float, Float, Float) -> RGB
toScaleRGB (x, y, z) = (toScale x, toScale y, toScale z)

cols' :: Int -> [(Float, Float, Float)]
cols' n = let n'  = n `div` 8
              xs  = map ((/ fromIntegral n') . fromIntegral) [1..n']
           in concatMap (`map` xs) [\b -> (0, 0, b),
                                    \g -> (0, g, 1),
                                    \b -> (0, 1, b),
                                    \r -> (r, 1, 0),
                                    \b -> (1, 1, b),
                                    \g -> (1, g, 1),
                                    \b -> (1, 0, b),
                                    \r -> (r, 0, 0)]

cols :: Int -> [RGB]
cols = map toScaleRGB . cols'
