-- start: 10:21
-- pause at 10:25. Restart at 10:29
-- first at 10:38, don't do AoC at work, you are inefficient
-- stop 11;02
-- restart 13:45
-- lot of pause
-- stop at 14:32
module Day14 where

import Data.List (maximum, minimum)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MapMonoidal
import qualified Data.Text as Text
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t =
  let (header : _ : ls) = Text.lines t
   in (Text.unpack header, MapMonoidal.fromList $ map parseLine ls)

parseLine t =
  let (Text.unpack -> [a, c], Text.unpack -> [b]) = unsafeSplitOn2 " -> " t
   in ((a, c), b)

-- * Generics

polymerise :: MonoidalMap (Char, Char) Char -> [Char] -> Int -> MonoidalMap Char (Sum Int)
polymerise m l n = MapMonoidal.fromListWith (+) (map (,1) l) <> go l
  where
    polymerizeCoupleMemo = memoFix (polymeriseCouple m)

    go (a : b : xs) = polymerizeCoupleMemo (n, (a, b)) <> go (b : xs)
    go _ = mempty

polymeriseCouple _ _ (0, _) = mempty
polymeriseCouple m f (n, couple@(a, b)) =
  let c = m MapMonoidal.! couple
   in f ((n - 1), (a, c)) <> f ((n - 1), (c, b)) <> MapMonoidal.singleton c 1

score m =
  let v = MapMonoidal.elems m
   in maximum v - minimum v

solve n (s, m) = score $ polymerise m s n

-- * SECOND problem

day = getSum . solve 10

day' = getSum . solve 40

-- * Tests

ex =
  parseContent
    [fmt|\
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 1588
    it "of second star" $ do
      day' ex `shouldBe` 2188189693529
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3906
    it "on second star" $ do
      day' fileContent `shouldBe` 4441317262452
