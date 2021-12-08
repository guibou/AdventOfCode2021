-- Start 21:48
-- First start: 21:58
-- Second start: 22:21
module Day08 where

import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Relude.Unsafe as Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent = map (f . Text.splitOn " | ") . Text.lines
  where
    f [a, b] = (Text.words a, Text.words b)

-- * Generics

digitsPatterns =
  map
    Set.fromList
    [ "abcefg",
      "cf",
      "acdeg",
      "acdfg",
      "bcdf",
      "abdfg",
      "abdefg",
      "acf",
      "acbdefg",
      "abcdfg"
    ]

digitsSet = Set.fromList digitsPatterns

perms = map (\p -> Map.fromList (zip p "abcdefgh")) $ permutations "abcdefgh"

applyPerm permut value = Set.map (\x -> permut Map.! x) value

checkThatPermValid permut l = all (\x -> applyPerm permut x `Set.member` digitsSet) l

findAValidPerm l = find (\p -> checkThatPermValid p l) perms

-- * FIRST problem

day :: [([Text], [Text])] -> _
day = length . filter f . concatMap snd
  where
    f x = Text.length x `elem` [2, 4, 3, 7]

-- * SECOND problem

day' :: _ -> Int
day' = sum . map decodeLine

decodeLine :: ([Text], [Text]) -> Int
decodeLine (l, numbers) = Unsafe.read $ concatMap show digits
  where
    Just validPerm = findAValidPerm (map (Set.fromList . Text.unpack) l)
    patterns = map (applyPerm validPerm . Set.fromList . Text.unpack) numbers
    digits = map (\((`Data.List.elemIndex` digitsPatterns) -> Just i) -> i) patterns

-- * Tests

ex =
  parseContent
    [fmt|\
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 26
    it "of second star" $ do
      day' ex `shouldBe` 61229
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 488
    it "on second star" $ do
      day' fileContent `shouldBe` 1040429
