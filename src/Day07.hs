-- start 22:47
-- first star at 22:55
-- second at 23:00
module Day07 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import Data.Foldable (minimumBy, Foldable (maximum))

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map (Unsafe.read . Text.unpack) . Text.splitOn ","

-- * Generics
fuelCost costF l = sum $ map costF l

solve costF l = fuelCost (costF (minimumBy (comparing (\x -> fuelCost (costF x) l)) [0.. maximum l])) l


-- * FIRST problem
day :: _ -> Int
day = solve (\x x' -> abs $ x - x')

-- * SECOND problem
day' :: _ -> Int
day' = solve $ \x x' -> sum1ToN (abs $ x - x')

-- * Tests
ex0 = [16 :: Int,1,2,0,4,2,7,1,2,14]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 37
    it "of second star" $ do
      day' ex0 `shouldBe` 168
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 342534
    it "on second star" $ do
      day' fileContent `shouldBe` 94004208
