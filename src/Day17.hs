{-# LANGUAGE ViewPatterns #-}

-- start: 16:40
-- first strat 17:00
-- second star 17:14
module Day17 where

import Data.List
import Utils

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

missed (Probe (V2 x y) (V2 vx _vy)) ((xmin, xmax), (ymin, _)) =
  or
    [ y < ymin,
      vx > 0 && x > xmax,
      (x + sum1ToN vx) < xmin
    ]

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
  --
  -- for Y: positive (>= 1) (otherwise, it will never go up, hehe ;)
  -- second star, it can actually be negative
  -- The maximum for Y is computed as such:
  -- It will climb as much as sum [1..vy] before starting the descent. Then it
  -- will descend on sum [1.. nstep] = ... TBT
  -- TODO: I hate this 120, I need to compute the correct value
  vy <- [yMin ..120]

  -- for X: positive (>= 1) (otherwise it will never reach the positive area. Also, never bigger than xmax
  -- If vy is positive, then we can easily find a minimum bound for vx (i.e. sum [1..minBound] < xmin)
  -- However, that's not super important for performances, because we also have to work with the y min
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
-- --> Done
