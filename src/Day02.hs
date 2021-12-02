-- start 7:48
-- first: 07:57
-- second: 08:01
module Day02 where

import Utils
import Text.Megaparsec

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [(Dir, Int)]
parseContent = unsafeParse parseContent'

data Dir = Forward | Down' | Up
  deriving (Show)

parseDirection :: Parser Dir
parseDirection = choice [
  "forward" $> Forward,
  "down" $> Down',
  "up" $> Up
  ]

parseLine = do
  d <- parseDirection
  _ <- " "
  n <- parseNumber
  pure (d, n)
  
parseContent' = Prelude.some parseLine
-- * Generics
moveSubmarine (depth, horizontal) = \case
  (Forward, dh) -> (depth, horizontal + dh)
  (Down', dd) -> (depth + dd, horizontal)
  (Up, du) -> (depth - du, horizontal)

moveSubmarine' (depth, horizontal, aim) = \case
  (Forward, dh) -> (depth + aim * dh, horizontal + dh, aim)
  (Down', dd) -> (depth, horizontal, aim+dd)
  (Up, du) -> (depth, horizontal, aim-du)


-- * FIRST problem
day :: [_] -> Int
day l = let (a, b) = foldl' moveSubmarine (0, 0) l
        in a * b

-- * SECOND problem
day' l = let (a, b, _) = foldl' moveSubmarine' (0, 0, 0) l in a * b

-- * Tests
ex0 = parseContent [fmt|\
forward 5
down 5
forward 8
up 3
down 8
forward 2|]


test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 150
    it "of second star" $ do
      day' ex0 `shouldBe` 900
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1694130
    it "on second star" $ do
      day' fileContent `shouldBe` 1698850445
