module Day01 where

import Data.List (zipWith3)
import Relude.Unsafe
import Utils
import qualified Data.Text as Text

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map (read.Text.unpack) . words

-- * Generics

-- * FIRST problem
day l = length $ filter (<0) $ zipWith (-) l (Relude.Unsafe.tail l)

-- * SECOND problem
day' values = day $ zipWith3 (\a b c -> a + b + c) values (drop 1 values) (drop 2 values)

-- * Tests

testContent =
  parseContent
    [str|
199
200
208
210
200
207
240
269
260
263
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day testContent `shouldBe` 7
    it "of second star" $ do
      day' testContent `shouldBe` 5
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1466
    it "on second star" $ do
      day' fileContent `shouldBe` 1491
