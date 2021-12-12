-- Done with JP live, I love you JP :)
-- start: 22:36
-- first: 22:50
-- second: 23:16
module Day09 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Map (V2 Int) Int
parseContent t = Map.fromList $ do
  (lineIdx, l) <- zip [0..] lli
  (columnIdx, c) <- zip [0..] l

  pure (V2 lineIdx columnIdx, c)

  where
    toHeight c = Unsafe.read [c]
    lli = map (map toHeight . Text.unpack) . Text.lines $ t

-- * Generics


-- * FIRST problem
foo m = do
  (p, height) <- Map.toList m

  let 
      adjacentHeights = do
         d <- drop 1 connect4
         let coord = p + d
         case Map.lookup coord m of
           Nothing -> []
           Just height' -> [height']

  guard $ all (\adjCase -> adjCase > height) adjacentHeights
  pure (p, height)

risk height = 1 + height

day :: Map (V2 Int) Int -> Int
day = sum . map risk . map snd . foo 

-- * SECOND problem
day' = product . take 3 . reverse . sort . flow


flow :: Map (V2 Int) Int -> [Int]
flow m = let allCases = Set.fromList (map fst . filter ((/=9) . snd) . Map.toList $ m)
         in snd $ foldl' (f m) (Set.empty, []) allCases

f :: Map (V2 Int) Int -> (Set (V2 Int), [Int]) -> (V2 Int) -> (Set (V2 Int), [Int])
f m (alreadyVisited, foundBassinSizes) newCoord
  | newCoord `Set.member` alreadyVisited = (alreadyVisited, foundBassinSizes)
  | otherwise = let
     bassin = growBassin m newCoord
                 in (alreadyVisited `Set.union` bassin, length bassin : foundBassinSizes)

growBassin :: Map (V2 Int) Int -> (V2 Int) -> Set (V2 Int)
growBassin m coord = go coord Set.empty 
  where
    go :: (V2 Int) -> Set (V2 Int) -> Set (V2 Int)
    go coord visited
      | coord `Set.member` visited = visited
      | otherwise = let
         voisinAVisiter = do
           d <- drop 1 connect4
           let coord' = coord + d
           case Map.lookup coord' m of
             Nothing -> []
             Just 9 -> []
             Just _ -> [coord']
           in foldl' (flip go) (Set.insert coord visited) voisinAVisiter

-- * Tests
ex = parseContent [fmt|\
2199943210
3987894921
9856789892
8767896789
9899965678|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 15
    it "of second star" $ do
      day' ex `shouldBe` 1134
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 588
    it "on second star" $ do
      day' fileContent `shouldBe` 964712
