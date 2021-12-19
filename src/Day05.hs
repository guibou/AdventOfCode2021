{-# LANGUAGE OverloadedStrings #-}

-- 11:48
-- 12:09
-- 12:10
module Day05 where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Linear hiding (ex)
import Relude.Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [(V2 Int, V2 Int)]
parseContent content = map ((\[(Text.unpack -> a), (Text.unpack -> b)] -> (readVec a, readVec b)) . (Text.splitOn " -> ")) . Text.lines $ content

readVec :: String -> V2 Int
readVec s =
  let (a, b) = (unsafeSplitOn2 "," . Text.pack $ s)
   in V2 (read $ Text.unpack a) (read $ Text.unpack b)

-- * Generics

isHorizontalOrVectical (V2 x0 y0, V2 x1 y1) = x1 == x0 || y1 == y0

deltaLine (V2 x0 y0, V2 x1 y1) =
  let dx = (x1 - x0)
      dy = (y1 - y0)
   in V2 (dx `div` gcd dx dy) (dy `div` gcd dx dy)

drawLine :: (V2 Int, V2 Int) -> [V2 Int]
drawLine line@(start, end) = go start
  where
    go x
      | x == end = [x]
      | otherwise = x:go (x+deltaLine line)

drawAllLines = mconcat . map drawLine

countDangerousSpots = length . Map.filter (>=2) . Map.fromListWith (+) . map (,1 :: Int) . drawAllLines

-- * FIRST problem

day :: _ -> Int
day c = countDangerousSpots (filter isHorizontalOrVectical c)

-- * SECOND problem

day' :: _ -> Int
day' = countDangerousSpots

-- * Tests

ex =
  parseContent
    [fmt|\
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 5
    it "of second star" $ do
      day' ex `shouldBe` 12
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4993
    it "on second star" $ do
      day' fileContent `shouldBe` 21101
