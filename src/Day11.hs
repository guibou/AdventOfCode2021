-- start at 23:18
-- first star at 00:11
-- second star at 00:16
module Day11 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Map (V2 Int) Int
parseContent t = Map.fromList $ do
  (lineIdx, l) <- zip [0..] (Text.lines t)
  (colIdx, c) <- zip [0..] (Text.unpack l)

  pure (V2 lineIdx colIdx, Unsafe.read [c])

-- * Generics
data Status = EnergyLevel Int
            | Flash
            deriving (Show, Eq)

step :: Map (V2 Int) Int -> (Map (V2 Int) Int, Int)
step m = flashStep (fmap (+1) m)

stepN m = newFlashThisStep:stepN m'
  where (m', newFlashThisStep) = step m

flashStep :: Map (V2 Int) Int -> (Map (V2 Int) Int, Int)
flashStep octopusMap = finalizeFlash $ goFixpoint (statusMap, 0)
  where
    finalizeFlash :: (Map (V2 Int) Status, Int) -> (Map (V2 Int) Int, Int)
    finalizeFlash (m, c) = (fmap toEnergyLevel m, c)

    statusMap = fmap EnergyLevel octopusMap

    goFixpoint a
      | b == a = a
      | otherwise = goFixpoint b
      where b = go a

    go :: (Map (V2 Int) Status, Int) -> (Map (V2 Int) Status, Int)
    go (m, nbFlashs) = let
      isFlashing (EnergyLevel x) | x > 9 = True
      isFlashing _ = False

      flashing = Map.filter isFlashing m
      flashingCoords = do
        flashingCoord <- Map.keys flashing
        d <- connect8

        let coord = flashingCoord + d
        guard $ coord `Map.member` m

        if coord == flashingCoord
           then [(flashingCoord, Flash)]
           else [(coord, EnergyLevel 1)]
      newMap = foldl' f m flashingCoords
            in (newMap, nbFlashs + length flashing)

toEnergyLevel :: Status -> Int
toEnergyLevel (EnergyLevel i)= i
toEnergyLevel Flash = 0

f :: Map (V2 Int) Status -> ((V2 Int), Status) -> Map (V2 Int) Status
f m (coord, status) = Map.insertWith f' coord status m

f' :: Status -> Status -> Status
f' Flash _ = Flash
f' _ Flash = Flash
f' (EnergyLevel a) (EnergyLevel b) = EnergyLevel (a + b)

display g = do
  for_ [0..10] $ \l -> do
    for_ [0..10] $ \c -> do
      case Map.lookup (l, c) g of
        Nothing -> putStr "X"
        Just v -> putStr (show v)
    putStrLn ""
  


-- * FIRST problem

day :: _ -> Int
day content = sum $ take 100 $ stepN content

-- * SECOND problem
day' content = 1 + (length $ takeWhile (\c -> c /= length content) $ stepN content)

-- * Tests
ex = parseContent [fmt|\
11111
19991
19191
19991
11111|]

exLarge = parseContent [fmt|\
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day exLarge `shouldBe` 1656
    it "of second star" $ do
      day' exLarge `shouldBe` 195
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1694
    it "on second star" $ do
      day' fileContent `shouldBe` 346
