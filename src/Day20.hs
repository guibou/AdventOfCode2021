-- start: 08:24
-- 08:37: I don't understand, at all, my example is correct, but the result is not on the bigger image
-- Mouhahah, you are a perverse sir! First star at 9:57 when I realize the problem that ALL the world is lit outside
-- second star immediatly after
module Day20 where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Utils
import Control.Lens
import qualified Data.Vector.Unboxed as Vector

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t =
  let (Text.replace "\n" "" -> algo, Text.drop 2 -> image) = Text.breakOn "\n\n" t
   in (Vector.fromList $ map (\case {'#' -> True; _ -> False}) $ Text.unpack algo, parse2DGrid (\c -> if c == '#' then True else False) image, False)

-- * Generics

enhance (algo, image, stepParity) = (algo,,not stepParity) $
  Map.fromList $ do
    let (V2 xMin yMin, V2 xMax yMax) = getBounds (Map.keys image)

    x <- [xMin - 1 .. xMax + 1]
    y <- [yMin - 1 .. yMax + 1]

    let coord = V2 x y

    pure (coord, getPixelValue coord (algo, image, stepParity))

toBinary = foldl' f 0
  where
    f acc v = acc * 2 + bool 0 1 v

getPixelValue coord (algo, image, stepParity) = (Vector.!) algo offset
  where
    offset = toBinary $ do
      dy <- [-1 .. 1]
      dx <- [-1 .. 1]
      let delta = V2 dx dy
      let v = case Map.lookup (coord + delta) image of
            -- A case which does not exists is considered off or on, depending
            -- on the parity of the step
            Nothing -> (stepParity && (Vector.!) algo 0)
            -- If it exists, it does have a value
            Just r -> r
      pure v

display g = display2DGrid (bool "." "#" <$> g)

-- * FIRST problem

day :: _ -> Int
day = length . Map.filter id . Control.Lens.view _2 . applyN 2 enhance

-- * SECOND problem

day' :: _ -> Int
day' = length . Map.filter id . Control.Lens.view _2 . applyN 50 enhance

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
