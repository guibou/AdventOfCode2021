-- start 21:54
-- first star 22:12
-- second star 22:25
module Day10 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Relude.Extra
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = map Text.unpack . Text.lines

data Status = Valid | Incomplete [Char] | SyntaxError Char deriving Show

openingChars = "{[<(" :: [Char]

closingOf '<' = '>'
closingOf '(' = ')'
closingOf '{' = '}'
closingOf '[' = ']'

-- * Generics
validLine l = go [] l
  where
    go [] [] = Valid
    go stack [] = Incomplete (map closingOf stack)
    go stack (x:xs)
      | x `elem` openingChars = go (x:stack) xs
      | otherwise = case stack of
                      [] -> SyntaxError x
                      (s:ss)
                        | closingOf s == x -> go ss xs
                        | otherwise -> SyntaxError x

score = \case
  Valid -> 0
  Incomplete _ -> 0
  SyntaxError ')' -> 3
  SyntaxError ']' -> 57
  SyntaxError '}' -> 1197
  SyntaxError '>' -> 25137

scoreCompletionString = go 0 
  where
    go s [] = s
    go s (x:xs) = go (5 * s + scoreChar x) xs

    scoreChar ')' = 1
    scoreChar ']' = 2
    scoreChar '}' = 3
    scoreChar '>' = 4

score' = \case
  Valid -> 0
  Incomplete stack -> scoreCompletionString stack
  SyntaxError _ -> 0

-- * FIRST problem
day :: _ -> Int
day = sum . map (score . validLine)

-- * SECOND problem
day' l = res Unsafe.!! (length res `div` 2)
  where
    res = sort (filter (/=0) (map (score' . validLine) l))


-- * Tests
ex = [
  "[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]"]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 26397
    it "of second star" $ do
      day' ex `shouldBe` 288957
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 323691
    it "on second star" $ do
      day' fileContent `shouldBe` 2858785164
