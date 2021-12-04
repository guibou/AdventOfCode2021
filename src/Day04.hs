-- start at 11:51
-- first at 12:18
-- first at 12:24
module Day04 where

import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude.Unsafe
import qualified Relude.Unsafe as Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> ([Int], _)
parseContent content = do
  let (header : rest) = Text.lines content

  let numbers = map (chunksOf 5) . chunksOf 25 $ map (read @Int . Text.unpack) $ Text.words (Text.unlines rest)

  (map (read @Int . Text.unpack) (Text.splitOn "," header), numbers)

-- * Generics

bingo (numbers, initGrids) = go (inits numbers) initGrids
  where
    go _ [] = []
    go (currentNumbers : xs) grids = case matchingGrid currentNumbers grids of
      [] -> go xs grids
      gridsToDelete ->
        let nextGrids = foldl' (flip Data.List.delete) grids gridsToDelete
            result = map (currentNumbers,) gridsToDelete
         in result <> go xs nextGrids
    go [] _ = error "WTF no winner, that's a crappy game"

matchingGrid :: [Int] -> [[[Int]]] -> [[[Int]]]
matchingGrid numbers grids = filter (matchNumbers numbers) grids

matchNumbers numbers grid =
  let numberSet = Set.fromList numbers
      gridSets = map (Set.fromList) (grid <> transpose grid)
   in any (`Set.isSubsetOf` numberSet) gridSets

scoreGrid :: ([Int], [[Int]]) -> Int
scoreGrid (numbers, grid) = Unsafe.last numbers * (sum $ (mconcat $ map Set.fromList grid) `Set.difference` (Set.fromList numbers))

-- * FIRST problem

day :: _ -> Int
day = scoreGrid . Unsafe.head . bingo

-- * SECOND problem

day' :: _ -> Int
day' = scoreGrid . Unsafe.last . bingo

-- * Tests

ex0 =
  parseContent
    [fmt|\
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 4512
    it "of second star" $ do
      day' ex0 `shouldBe` 1924
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 14093
    it "on second star" $ do
      day' fileContent `shouldBe` 17388
