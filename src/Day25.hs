-- I took 24 minutes, so many mistakes
module Day25 where

import qualified Data.Map as Map
import qualified Relude.Unsafe as Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t =
  let m = fmap Unsafe.fromJust . Map.filter isJust . parse2DGrid cToDir $ t
   in (snd $ getBounds (Map.keys m), m)

cToDir 'v' = Just South
cToDir '>' = Just East
cToDir '.' = Nothing
cToDir _ = error "WTF"

-- * Generics

data Dir = South | East deriving (Show, Eq)

clampCoord (V2 maxX maxY) (V2 x' y')
  | x' > maxX = V2 0 y'
  | y' > maxY = V2 x' 0
  | otherwise = V2 x' y'

move (b, m) dir =
  ( b,
    Map.fromList $ do
      (pos, cucumber) <- Map.toList m
      if cucumber == dir
        then do
          let pos' = clampCoord b $ case dir of
                South -> pos + V2 0 1
                East -> pos + V2 1 0

          if pos' `Map.member` m
            then pure (pos, cucumber)
            else pure (pos', cucumber)
        else pure (pos, cucumber)
  )

step a = move (move a East) South

day = go (0 :: Int)
  where
    go n e
      | e' == e = n + 1
      | otherwise = go (n + 1) e'
      where
        e' = step e

-- * Tests

ex =
  parseContent
    [str|\
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
|]

ex' =
  parseContent
    [str|\
...>...
.......
......>
v.....>
......>
.......
..vvv..
|]

display (V2 maxX maxY, m) = display2DGrid $
  Map.fromList $ do
    x <- [0 .. maxX]
    y <- [0 .. maxY]

    pure
      ( V2 x y,
        case Map.lookup (V2 x y) m of
          Just South -> "v"
          Just East -> ">"
          Nothing -> "."
      )

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 58
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 329
