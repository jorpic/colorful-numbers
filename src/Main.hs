module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG (SVG, renderSVG)
-- import Data.Colour.Palette.ColorSet (rybColor)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..))
import Data.Colour.Palette.Harmony (colorRamp, complement)


type Grid = [[Int]]

diffPairs :: [Int] -> [Int]
diffPairs = go
  where
    go (x:y:xs) = (y-x) : go (y:xs)
    go  _ = []

numbers :: Int -> Grid
numbers md = extend $ takeWhile (/=[]) $ map f [1..]
  where
    f ml = takeWhile (>0) [x*ml `mod` md | x <- [1..]]
    extend = map (take (md-1) . (++ repeat 0))

grid :: Grid -> Diagram SVG
grid = vcat . map (hcat . map cell)
  where
    cell i
      = text (show i) # fontSizeL 0.3 # fc comp
      <> square 1 # lc darkgrey # fc (color i) # lw veryThin
      where
        comp = complement (color i) !! 1

    -- color = colorRamp 23 $ brewerSet Accent 8
    color i
      | i > 0 = green' !! (i - 1)
      | i == 0  = black
      | otherwise = red' !! (1 - i)
      where
        green' = colorRamp 32 $ brewerSet YlGn 8
        red' = colorRamp 32 $ brewerSet OrRd 8

grids :: [Int] -> Diagram SVG
grids = vsep 1 . map col
  where
    col i = hsep 1 $ map grid $ iterateN 3 (map diffPairs) $ numbers i

row :: Int -> Diagram SVG
row = hsep 1 . map grid . iterateN 3 (map diffPairs) . numbers

main :: IO ()
main = do
    renderSVG
        "svg/grid.svg"
        (mkSizeSpec $ V2 (Just 800) (Just 1600))
        (grids [3,5,7,11,13,17,19,23,29])
    renderSVG
        "svg/row.svg"
        (mkSizeSpec $ V2 (Just 1200) (Just 500))
        (row 23)
