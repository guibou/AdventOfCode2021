module Day06 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map (Unsafe.read . Text.unpack) . Text.splitOn ","

toMap l = Map.fromListWith (+) (map (,1) l)

-- * Generics
step :: Map Int Int -> Map Int Int
step l = Map.fromListWith (+) $ do
  (x, n) <- Map.toList l
  if x == 0
     then [(6, n), (8, n)]
     else [((x-1), n)]

solve n = sum . applyN n step . toMap

-- * FIRST problem
day :: [Int] -> Int
day = solve 80

-- * SECOND problem
day' :: [Int] -> Int
day' = solve 256

-- * Tests
ex0 :: [Int]
ex0 = [3,4,3,1,2]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 5934
    it "of second star" $ do
      day' ex0 `shouldBe` 26984457539
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 379414
    it "on second star" $ do
      day' fileContent `shouldBe` 1705008653296


-- START AT 18:43
-- first star at 18:52
-- second star 19:12 ;(
