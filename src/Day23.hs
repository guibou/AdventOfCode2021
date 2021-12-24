{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
-- start at 12:11
-- pause at 13:33, I do have most of the path finding setup, now I need to
-- debug my transition function.
-- first star at 14:40
-- second star at 15:15
module Day23 where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Relude.Unsafe as Unsafe
import Utils hiding (set)
import Path
import Control.Lens
import Data.List (zip4)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = Configuration mempty (map toRoom . transpose . map (Text.unpack . Text.filter (\c -> c `elem` ("ABCD" :: [Char]))) . take 2 . drop 2 . Text.lines $ t) 2

toRoom :: [Char] -> Room
toRoom [a, b] = Room [a', b']
  where
    a' = Unsafe.fromJust $ readMaybe [a]
    b' = Unsafe.fromJust $ readMaybe [b]

-- * Generics
data Amphipod = A | B | C | D
  deriving (Show, Read, Generic, Ord, Eq, Hashable)

data Room = Room {unRoom :: [Amphipod]}
  deriving (Show, Generic, Hashable, Ord, Eq)

data Configuration = Configuration
  { alley :: HashMap Int Amphipod,
    rooms :: [Room],
    roomDepth :: Int
  }
  deriving (Show, Hashable, Ord, Eq, Generic)

targetRooms d = Configuration mempty [targetRoom d A, targetRoom d B, targetRoom d C, targetRoom d D] d


targetRoom d a = Room (replicate d a)

roomOffsets = [2,4,6,8]

nextConfiguration Configuration{alley, rooms, roomDepth} = amphipod_leaving_their_room <> amphipod_back_in_their_room
  where
  -- Case where one amphipod leave it's room and go to the left or the right
    amphipod_leaving_their_room = do
      (room, roomNumber, roomOffset, kindRoom) <- zip4 rooms [0..] roomOffsets [A, B, C, D]
      (newRoom, amphipod, cost) <- case room of
                    -- a can leave only if a is not in the correct room OR b is not in the correct room
                    Room (a:xa)
                      | a /= kindRoom || (any (/=kindRoom) xa) -> [(Room xa, a, roomDepth - length xa)]
                    _ -> []
      let newOffsets = walkTheAlleyForFreePlaces roomOffset alley
      newOffset <- newOffsets
      guard $ newOffset `notElem` roomOffsets
      let newAlley = HashMap.insert newOffset amphipod alley
      let newRooms = set (ix roomNumber) newRoom rooms
      pure $ (costPerPod amphipod * (cost + (abs (roomOffset - newOffset))), Configuration newAlley newRooms roomDepth)

    amphipod_back_in_their_room = do
      (position, amphi) <- HashMap.toList alley
      let roomNumberForThisAmphi = case amphi of
                                     A -> 0
                                     B -> 1
                                     C -> 2
                                     D -> 3
      (costIntoRoom, newRoomConfiguration) <- case rooms Unsafe.!! roomNumberForThisAmphi of
                                Room bs
                                  | all (==amphi) bs -> [(roomDepth - length bs, Room (amphi:bs))]
                                _ -> []
      -- check that the path is free
      guard $ checkPathIsFree alley position (roomOffsets Unsafe.!! roomNumberForThisAmphi)
      -- compute the cost
      let cost = costIntoRoom + (abs (position - roomOffsets Unsafe.!! roomNumberForThisAmphi))
      -- return the new room configuration
      let newAlley = HashMap.delete position alley
      let newRooms = set (ix roomNumberForThisAmphi) newRoomConfiguration rooms
      pure $ (costPerPod amphi * cost, Configuration newAlley newRooms roomDepth)

checkPathIsFree alley a b
   | a < b = all (\p -> not (HashMap.member p alley)) [a + 1 .. b]
   | otherwise = all (\p -> not (HashMap.member p alley)) [b .. a - 1]

walkTheAlleyForFreePlaces :: Int -> HashMap Int Amphipod -> [Int]
walkTheAlleyForFreePlaces startingPoint alley = go startingPoint (1) <> go startingPoint (-1)
  where
    go (-1) _ = []
    go 11 _ = []
    go p dp
      | HashMap.member p alley = []
      | otherwise = p : go (p + dp) dp

costPerPod A = 1
costPerPod B = 10
costPerPod C = 100
costPerPod D = 1000
-- * FIRST problem

day start = fst <$> shortestPath nextConfiguration (+) start (targetRooms 2)
day' start = fst <$> shortestPath nextConfiguration (+) (upgradeRoomForSecondStar start) (targetRooms 4)

-- * SECOND problem
upgradeRoomForSecondStar (Configuration alley rooms _) = Configuration alley rooms' 4
  where
    rooms' = zipWith upgrade rooms [[D, D], [C, B], [B, A], [A, C]]
    upgrade (Room [x, y]) l = Room $ x:l <> [y]

-- * Tests
display (Configuration{alley, rooms, roomDepth}) = do
  let
    roomsContent = transpose $ map (pad roomDepth . unRoom) rooms
    pad :: Int -> [Amphipod] -> [Char]
    pad s l = reverse $ take s (map ((\(show -> [c]) -> c)) (reverse l) <> cycle ['.'])


  putStrLn (replicate 12 '#')
  putStrLn $ "#" <> showAlley alley <> "#"
  let showLevel l = putStrLn $ "###" <> intercalate "#" (map pure l) <> "###"
  mapM_ showLevel roomsContent
  putStrLn "  #########"
  putStrLn ""

showAlley m = concat $ do
  i <- [0..10]
  case HashMap.lookup i m of
    Nothing -> pure "."
    Just v -> pure $ show v

ex = parseContent [str|\
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
|]

progress targetCost c = mapM_ f (nextConfiguration c)
  where
   f (c, e) = do
    when (isNothing targetCost || Just c == targetCost) $ do
      print c
      print e
      display e

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` Just 12521
    it "of second star" $ do
      day' ex `shouldBe` Just 44169
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 14627
    it "on second star" $ do
      day' fileContent `shouldBe` Just 41591
--
-- 14625 is too low
--
--
-- problem = Configuration (HashMap.fromList [(5, D), (7, B), (9, D)]) [Room (Just (Just A, A)), Room (Just (Nothing, B)), Room (Just (Just C, C)), Room Nothing]
-- problem' = parseContent [str|\
-- ############
-- #.....D.B.D#
-- ###A#.#C#.###
-- ###A#B#C#.###
--   #########
-- |]
