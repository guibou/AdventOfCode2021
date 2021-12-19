-- start: 09:55
-- first: 10:15
-- end: 10:21
module Day13 where

import Control.Applicative.Combinators.NonEmpty as CACN (sepBy1, some)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec hiding (sepBy1)
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent =
  unsafeParse
    ( ( (,) <$> (Set.fromList <$> (Prelude.some (parseV2 <* "\n")))
          <*> ( "\n"
                  *> (CACN.some (parseFold <* (void "\n" <|> eof)))
              )
      )
    )

parseFold = do
  _ <- "fold along "
  xy <- choice [X <$ "x", Y <$ "y"]
  _ <- "="
  n <- parseNumber
  pure (xy, n)

parseV2 :: Parser (V2 Int)
parseV2 = do
  x <- parseNumber
  _ <- ","
  y <- parseNumber
  pure (V2 x y)

data XY = X | Y deriving (Show)

-- * Generics

foldPaper paper (X, n) = Set.map (foldX n) paper
foldPaper paper (Y, n) = Set.map (foldY n) paper

foldX :: Int -> V2 Int -> V2 Int
foldX xLine v@(V2 x y)
  | x < xLine = v
  | x == xLine = error "WTF"
  | otherwise = V2 (2 * xLine - x) y

foldY :: Int -> V2 Int -> V2 Int
foldY yLine v@(V2 x y)
  | y < yLine = v
  | y == yLine = error "WTF"
  | otherwise = V2 x (2 * yLine - y)

-- * FIRST problem

day :: (Set (V2 Int), NonEmpty (XY, Int)) -> Int
day (paper, i :| _) = length $ foldPaper paper i

-- * SECOND problem

day' (paper, instr) = str2DGrid $ Map.fromList (map (,"*") (Set.toList $ foldl' foldPaper paper instr))

-- * Tests

ex :: (Set (V2 Int), NonEmpty (XY, Int))
ex =
  parseContent
    [str|\
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 17
    it "of second star" $ do
      day' ex
        `shouldBe` [strTrim|
                              *****
                              *   *
                              *   *
                              *   *
                              *****|]
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 759
    it "on second star" $ do
      day' fileContent
        `shouldBe` [strTrim|
                              *  * ****  **  ***  **** *  * ***  ***
                              *  * *    *  * *  *    * * *  *  * *  *
                              **** ***  *    *  *   *  **   *  * *  *
                              *  * *    *    ***   *   * *  ***  ***
                              *  * *    *  * * *  *    * *  *    * *
                              *  * ****  **  *  * **** *  * *    *  *|]
