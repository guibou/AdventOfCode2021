-- start at 10:29
-- first at 10:36
-- secod at 10:46
module Day15 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Map as Map
import Path

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Map (V2 Int) Int
parseContent = parse2DGrid (\c -> Unsafe.read [c])

-- * Generics
makeHugeMap m = Map.unions $ do
  let (_, V2 w' h') = getBounds (Map.keys m)
  let w = w' + 1
  let h = h' + 1

  l <- [0..4]
  c <- [0..4]

  let delta = l + c
      fKey k = k + V2 (l * h) (c * w)
      fClamp x = (x - 1) `mod` 9 + 1
  let m' = Map.mapKeys fKey $ fmap fClamp $ fmap (+delta) m

  pure m'


-- * FIRST problem
day m = fst <$> shortestPath transition (+) (V2 0 0) end
 where
   (_, end) = getBounds (Map.keys m)
   transition p = do
     d <- drop 1 connect4
     let p' = p + d
     case Map.lookup p' m of
       Nothing -> []
       Just risk -> pure (risk, p')

-- * SECOND problem
day' :: _ -> Maybe Int
day' = day . makeHugeMap

-- * Tests
ex = parseContent [str|\
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581|]

-- I Used that one to check the huge map algo
smallGrid = parseContent [str|\
92
34|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` Just 40
    it "of second star" $ do
      day' ex `shouldBe` Just 315
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 458
    it "on second star" $ do
      day' fileContent `shouldBe` Just 2800
