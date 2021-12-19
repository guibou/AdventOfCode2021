{-# LANGUAGE ViewPatterns #-}

-- start: 16:40
-- first strat 17:00
-- second star 17:14
module Day17 where

import Utils
import Data.List

-- target area: x=207..263, y=-115..-63
fileContent = ((207, 263), (-115, -63))

-- * Generics

data Probe = Probe
  { position :: V2 Int,
    velocity :: V2 Int
  }
  deriving (Show, Generic)

step :: Probe -> Probe
step (Probe p v@(V2 vx vy)) = Probe p' v'
  where
    p' = p + v
    v' = V2 vx' (vy - 1)
    vx'
      | vx < 0 = vx + 1
      | vx > 0 = vx - 1
      | otherwise = 0

inArea (Probe (V2 x y) _) ((xmin, xmax), (ymin, ymax)) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

missed (Probe (V2 _ y) _) (_, (ymin, _)) = y < ymin

-- * FIRST problem

-- All of that can be solved actually, can we, or must we bruteforce it?
-- let's try to bruteforce it
hitOrMiss probe bounds mY
  | missed probe bounds = Nothing
  | inArea probe bounds = Just (mY)
  | otherwise = hitOrMiss newProbe bounds (max newProbeY mY)
  where
    newProbe@(Probe (V2 _ newProbeY) _) = step probe

-- * SECOND problem

solve bounds@((_, xMax), (yMin, _)) = do
  -- Initial velocity must be:
  -- for X: positive (>= 1) (otherwise it will never reach the positive area. Also, never bigger than xmax
  -- for Y: positive (>= 1) (otherwise, it will never go up, hehe ;)
  -- second star, it can actually be negative
  --
  -- TODO: the maximum value for yMin is arbitrary...
  vy <- [yMin .. 1000]
  vx <- [1 .. xMax + 1]

  let v = V2 vx vy

  case hitOrMiss (Probe (V2 0 0) v) bounds 1 of
    Nothing -> []
    Just vMax -> pure (vMax)

-- * Tests
day = maximum . solve

day' = length . solve

ex :: ((Int, Int), (Int, Int))
ex = ((20, 30), (-10, -5))

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 45
    it "of second star" $ do
      day' ex `shouldBe` 112
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 6555
    it "on second star" $ do
      day' fileContent `shouldBe` 4973

-- 4931 is too low

-- This solution sucks, because there is an arbitrary maxmimu value and it takes hours.
-- This kills my performances, I'll have to go back on the solution later
