-- start: 08:24
-- 08:37: I don't understand, at all, my example is correct, but the result is not on the bigger image
-- Mouhahah, you are a perverse sir! First star at 9:57 when I realize the problem that ALL the world is lit outside
-- second star immediatly after
module Day20 where

import Control.Lens
import Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t =
  let (Text.replace "\n" "" -> algo, Text.drop 2 -> image) = Text.breakOn "\n\n" t
      grid = parse2DGrid id image
   in ( toBitInteger $ map (\case '#' -> True; _ -> False) $ Text.unpack algo,
        Map.keysSet (Map.filter (== '#') grid),
        getBounds (Map.keys grid),
        False
      )

-- * Generics

toBitInteger :: [Bool] -> Integer
toBitInteger l = foldl' f zeroBits (zip [0 ..] l)
  where
    f bitset (i, b)
      | b = bitset .|. bit i
      | otherwise = bitset

enhance :: (Integer, Set (V2 Int), (V2 Int, V2 Int), Bool) -> (Integer, Set (V2 Int), (V2 Int, V2 Int), Bool)
enhance (algo, image, bounds, stepParity) = (algo,,newBounds,not stepParity) $
  Set.fromList $ do
    x <- [xMin - 1 .. xMax + 1]
    y <- [yMin - 1 .. yMax + 1]

    let coord = V2 x y

    guard $ getPixelValue coord (algo, image, bounds, stepParity)

    pure coord
  where
    newBounds = (V2 (xMin - 1) (yMin -1), V2 (xMax + 1) (yMax + 1))
    (V2 xMin yMin, V2 xMax yMax) = bounds

toBinary = foldl' f 0
  where
    f acc v = acc * 2 + bool 0 1 v

inBound (V2 xMin yMin, V2 xMax yMax) (V2 x y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

getPixelValue coord (algo, image, bounds, stepParity) = testBit algo offset
  where
    offset = toBinary $ do
      dy <- [-1 .. 1]
      dx <- [-1 .. 1]
      let coord' = coord + V2 dx dy
      let v
            | Set.member coord' image = True
            | not (inBound bounds coord') && stepParity && testBit algo 0 = True
            | otherwise = False
      pure v

display g = display2DGrid (bool "." "#" <$> g)

-- * FIRST problem

day :: _ -> Int
day = length . Control.Lens.view _2 . applyN 2 enhance

-- * SECOND problem

day' :: _ -> Int
day' = length . Control.Lens.view _2 . applyN 50 enhance

-- * Tests

ex =
  parseContent
    [str|\
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 35
    it "of second star" $ do
      day' ex `shouldBe` 3351
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 5786
    it "on second star" $ do
      day' fileContent `shouldBe` 16757

-- 5960 is too high
