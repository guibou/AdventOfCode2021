-- start 15:25
-- first 15:37
-- pause at 15:50
-- restart at 16:04
-- end at 16:08
module Day12 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = Map.fromListWith (++) $ do
  edge <- Text.lines t
  let [a, b] = Text.splitOn "-" edge
  
  [(a, [b]), (b, [a])]

isSmallCave t = Text.toLower t == t

-- * Generics
walkCave currentEdge visitedSmallCaves oneWasVisitedOnce m = do
  if currentEdge == "end"
     then pure ["end"]
     else do
          let nextVisited
               | isSmallCave currentEdge = Set.insert currentEdge visitedSmallCaves
               | otherwise = visitedSmallCaves

          nextCave <- m Map.! currentEdge
          guard $ nextCave /= "start"

          guard $ (nextCave `Set.notMember` visitedSmallCaves) || not oneWasVisitedOnce

          let oneWasVisitedOnce' = oneWasVisitedOnce || (isSmallCave nextCave && nextCave `Set.member` visitedSmallCaves)

          (currentEdge:) <$> walkCave nextCave nextVisited oneWasVisitedOnce' m


-- * FIRST problem
day :: _ -> Int
day = length . walkCave "start" mempty True


-- * SECOND problem
day' :: _ -> Int
day' = length . walkCave "start" mempty False

-- * Tests
ex = parseContent [str|\
start-A
start-b
A-c
A-b
b-d
A-end
b-end|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 10
    it "of second star" $ do
      day' ex `shouldBe` 36
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3563
    it "on second star" $ do
      day' fileContent `shouldBe` 105453
