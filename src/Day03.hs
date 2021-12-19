-- Start 07:32
-- start1: 07:44. Immediatly stop, I need to bring kids to school and go to work.
-- 07:59
module Day03 where

import qualified Data.Text as Text
import Relude.Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = map Text.unpack . Text.lines

-- * Generics

countLine l =
  let nb0 = length $ filter (== '0') l
      nb1 = length l - nb0
   in if nb0 > nb1 then '0' else '1'

toDec = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (acc * 2 + (read [x])) xs

toEpsilon = map f'
  where

f' '1' = '0'
f' '0' = '1'
f' _ = error "WTF"

rates e =
  let value = map countLine (transpose e)
      gammaRate = value
      epsilon = (toEpsilon value)
   in (gammaRate, epsilon)

-- * FIRST problem

day :: _ -> Int
day = uncurry (\x y -> toDec x * toDec y) . rates

recMatchPrefix f e = go (zip e e)
  where
    go [(x, _)] = x
    go l = go (map (\(res, crit') -> (res, drop 1 crit')) (filter (t . snd) l))
      where
        t (x : _) = x == f crit
        t _ = error "WTF"

        crit = countLine $ case transpose (map snd l) of
          (x : _) -> x
          _ -> error "WTF"

-- * SECOND problem

day' :: _ -> _
day' e =
  let (toDec -> a, toDec -> b) = (recMatchPrefix id e, recMatchPrefix f' e)
   in a * b

-- * Tests

ex =
  parseContent
    [str|\
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 198
    it "of second star" $ do
      day' ex `shouldBe` 230
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2640986
    it "on second star" $ do
      day' fileContent `shouldBe` 6822109
