{-# LANGUAGE TemplateHaskell #-}

module Day21 where

import Data.HashMap.Strict as HashMap
import Test.Hspec
import Utils

fileContent = (Player 2 0, Player 7 0)

-- * Generics

data Player = Player
  { position :: Int,
    score :: Int
  }
  deriving (Show, Eq, Hashable, Generic)

deriveMemoizable ''Player

-- * FIRST problem

infiniteDice = cycle [1 :: Int .. 100]

playerStep diceValue player =
  player
    { position = newPosition,
      score = score player + newPosition
    }
  where
    newPosition = (position player + diceValue - 1) `mod` 10 + 1

day = go 0 infiniteDice
  where
    go !nbRolls (splitAt 3 -> (sum -> delta, dice)) (p0, p1)
      | score p1 >= 1000 = nbRolls * score p0
      | otherwise = go (nbRolls + 3) dice (p1, playerStep delta p0)

-- * SECOND problem

day' ps = let V2 a b = fst $ go ps mempty in max a b
  where
    go (p0, p1) cache
      | Just res <- HashMap.lookup (p0, p1) cache = (res, cache)
      | score p1 >= 21 = (V2 (0 :: Int) 1, cache)
      | otherwise =
        let (res, cache') = first swapV2 $ Prelude.foldl' fFold (V2 0 0, cache) allQuantumDicesSums
         in (res, HashMap.insert (p0, p1) res cache')
      where
        fFold (scores, c) delta = first (+scores) $ go (p1, playerStep delta p0) c

swapV2 (V2 a b) = V2 b a

allQuantumDicesSums = do
  dice1 <- [1 .. 3]
  dice2 <- [1 .. 3]
  dice3 <- [1 .. 3]

  pure $ dice1 + dice2 + dice3

-- * Tests

ex = (Player 4 0, Player 8 0)

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 739785
    it "of second star" $ do
      day' ex `shouldBe` 444356092776315
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 805932
    it "on second star" $ do
      day' fileContent `shouldBe` 133029050096658
