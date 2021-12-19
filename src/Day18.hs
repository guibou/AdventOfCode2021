-- start: 17:21
-- pause at 18:19. I'm close to completion, but there is a bug ;)
-- I started again at 21:00, and at 00:00 I found the first star. I'm frustrated by my poor understanding of the rules
-- done at 00:20, the code is ugly, because I copy pasted by mega recursion in two different recursions.
module Day18 where

import Utils
import GHC.List (maximum)
import Data.Foldable (foldl1)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (some (parsePairs <* "\n"))

parsePairs = do
  void "["
  p <- (Single <$> parseNumber) <|> parsePairs
  void ","
  p' <- (Single <$> parseNumber) <|> parsePairs
  void "]"

  pure $ Pair p p'

data Pair
  = Pair Pair Pair
  | Single Int
  deriving (Show, Eq)

-- * Generics
addPair a b = reducePairFix $ Pair a b

data Action
  = Explode (Maybe Int) (Maybe Int)
  | Continue
  | Done
  deriving (Show)

reduceSingle input = go (0 :: Int) input Continue
  where
    go depth item action = case action of
      Done -> (item, Done)
      (Explode _ _) -> error "Should never happen"
      Continue -> case item of
        Single i
          | i >= 10 -> (split i, Done)
          | otherwise -> (item, Continue)
        Pair p p' ->
          let (newP, newAction) = go (depth + 1) p Continue
           in case newAction of
                Explode a b -> (Pair newP (addFirst p' b), Explode a Nothing)
                Done -> (Pair newP p', Done)
                Continue ->
                  let (newP', newAction') = go (depth + 1) p' Continue
                   in case newAction' of
                        Done -> (Pair newP newP', Done)
                        Continue -> (Pair newP newP', Continue)
                        Explode a b -> (Pair (addLast newP a) newP', Explode Nothing b)

reduceExplode input = go (0 :: Int) input Continue
  where
    go depth item action = case action of
      Done -> (item, Done)
      (Explode _ _) -> error "Should never happen"
      Continue -> case item of
        Single _
          | otherwise -> (item, Continue)
        Pair (Single a) (Single b)
          | depth >= 4 -> (Single 0, Explode (Just a) (Just b))
        Pair p p' ->
          let (newP, newAction) = go (depth + 1) p Continue
           in case newAction of
                Explode a b -> (Pair newP (addFirst p' b), Explode a Nothing)
                Done -> (Pair newP p', Done)
                Continue ->
                  let (newP', newAction') = go (depth + 1) p' Continue
                   in case newAction' of
                        Done -> (Pair newP newP', Done)
                        Continue -> (Pair newP newP', Continue)
                        Explode a b -> (Pair (addLast newP a) newP', Explode Nothing b)

reduceYadaYada = fixpoint (fst . reduceSingle . fixpoint (fst . reduceExplode))

reducePairFix input = reduceYadaYada input

addLast :: Pair -> Maybe Int -> Pair
addLast p Nothing = p
addLast (Pair a b) v = Pair a (addLast b v)
addLast (Single v) (Just v') = Single (v + v')

addFirst :: Pair -> Maybe Int -> Pair
addFirst p Nothing = p
addFirst (Pair a b) v = Pair (addFirst a v) b
addFirst (Single v) (Just v') = Single (v + v')

split :: Int -> Pair
split n = Pair (Single a) $ if res == 0 then Single a else Single (a + 1)
  where
    (a, res) = n `divMod` 2

-- * FIRST problem

day :: _ -> Int
day = magnitude . adds

-- * SECOND problem
display :: Pair -> String
display = display' (0 :: Int)

display' _ (Single v) = show v
display' depth (Pair p p') = [fmt|{openingAt depth}{display' (depth + 1) p},{display' (depth + 1) p'}{closingAt depth}|]

openingAt 0 = '['
openingAt 1 = '{'
openingAt 2 = '<'
openingAt 3 = '('
openingAt 4 = '\\'
openingAt 5 = '@'
openingAt _ = error "Too deep"

closingAt 0 = ']'
closingAt 1 = '}'
closingAt 2 = '>'
closingAt 3 = ')'
closingAt 4 = '/'
closingAt 5 = '#'
closingAt _ = error "Too deep"

day' l = maximum $ do
  (a, l') <- select l
  b <- l'
  pure $ magnitude (addPair a b)

-- * Tests

ex =
  parseContent
    [str|\
[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]\
|]

exList =
  parseContent
    [str|\
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]\
|]

adds = foldl1 addPair

magnitude (Single v) = v
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

homework =
  parseContent
    [str|\
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
|]

largerExample =
  parseContent
    [str|\
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
|]

sumTree (Single a) = a
sumTree (Pair a b) = sumTree a + sumTree b

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day homework `shouldBe` 4140
    it "of second star" $ do
      day' homework `shouldBe` 3993
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4243
    it "on second star" $ do
      day' fileContent `shouldBe` 4701

-- 4617 is too high
