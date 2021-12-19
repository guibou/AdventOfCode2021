-- start at 12:05
-- pause at 13:01
-- ends at 17:25, I've played a bit with kids, but hell, I got stuck by that one
module Day19 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Path
import Text.Megaparsec
import Utils
import Data.List (maximum)
import qualified Relude.Unsafe as Unsafe

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (parseScanner `sepBy` "\n")

parseBeacon = V3 <$> parseNumber <*> ("," *> parseNumber) <*> ("," *> parseNumber)

parseScanner = do
  void "--- scanner "
  n <- parseNumber
  void "---\n"
  beacon <- Prelude.some (parseBeacon <* "\n")
  pure (n, beacon)

-- * Generics

axis =
  [ V3 (0 :: Double) 0 1,
    V3 0 1 0,
    V3 1 0 0
  ]

rotMat = do
  (a, axis') <- select axis
  (b, [_]) <- select axis'

  negateA <- [-1, 1]
  negateB <- [-1, 1]

  let a' = negateA * a
  let b' = negateB * b

  pure $ (fmap truncate) <$> V3 a' b' (a' `cross` b')

-- * FIRST problem

matchScanners scanner scanner' = Map.keys $
  Map.filter (>= 12) $
    Map.fromListWith (+) $ do
      coord <- scanner
      coord' <- scanner'

      -- returns the offset between each scanner
      -- associated with count 1
      pure (coord - coord', 1)

matchScannerWithRot scanner scanner' = do
  rotM <- rotMat

  case matchScanners scanner ((fmap (*! rotM) scanner')) of
    [] -> []
    [doffset] -> pure (doffset, rotM)

fiz l = do
  (scanN, beacons) <- l
  (scanN', beacons') <- l

  guard $ scanN /= scanN'

  case matchScannerWithRot beacons beacons' of
    [] -> []
    [(doffset, rotM)] ->
      [ ((scanN, scanN'), (doffset, rotM))
      ]

invI m = fmap truncate <$> (inv33 (fmap fromIntegral <$> m))

changeFrame (doffset, rotM) scanner' = fmap (a . b) scanner'
  where
    a = (+ doffset)
    b = (*! rotM)

manhatanSize (abs -> V3 x y z) = x + y + z

stupidApplyFoo :: [(Int, [V3 Int])] -> (Int, Int)
stupidApplyFoo l =
  let fizData = fiz l
      fizMap = Map.fromList fizData
      (nStart, Set.fromList -> set0):others = l
      bizu = do
              (number, beacons) <- others
              let Just (_, path) = pathTo nStart number (map fst fizData)
              let [dx] = applyTrans fizMap path number [(V3 0 0 0)]
              pure $ (dx, Set.fromList (applyTrans fizMap path number beacons))

      dists = do
        (p, px) <- select (map fst bizu)
        p' <- px

        pure $ manhatanSize (p - p')

  in (length $ Set.union set0 $ Set.unions $ map snd bizu, maximum dists)

pathTo nStart n l = shortestPath nextVertex (+) n nStart
  where
    nextVertex a = map (1,) $ map snd $ (filter (\(x, _) -> x == a)) l

applyTrans :: Map (Int, Int) _ -> [Int] -> Int -> [V3 Int] -> [V3 Int]
applyTrans _ [] _ beacons = beacons
applyTrans m (v : vs) current beacons = case Map.lookup (v, current) m of
  Nothing -> error "WTF"
  Just trans -> applyTrans m vs v (changeFrame trans beacons)

-- * SECOND problem

day = fst . stupidApplyFoo
day' = snd . stupidApplyFoo

-- * Tests

ex :: [(Int, [V3 Int])]
ex =
  parseContent
    [str|\
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 79
    it "of second star" $ do
      day' ex `shouldBe` 3621
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 405
    it "on second star" $ do
      day' fileContent `shouldBe` 12306
