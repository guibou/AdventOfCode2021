-- start: 15:27
-- first: 16:23
-- second: 16:32
module Day16 where

import Data.List (maximum, minimum)
import qualified Data.Text as Text
import Utils
import Prelude hiding (group)

fileContent :: _
fileContent = parseContent $(getFile)

textToBits :: Text -> [Bool]
textToBits = Text.foldr f [] . Text.toLower
  where
    f c l = cToL c <> l

pattern F = False

pattern T = True

cToL :: Char -> [Bool]
cToL '0' = [F, F, F, F]
cToL '1' = [F, F, F, T]
cToL '2' = [F, F, T, F]
cToL '3' = [F, F, T, T]
cToL '4' = [F, T, F, F]
cToL '5' = [F, T, F, T]
cToL '6' = [F, T, T, F]
cToL '7' = [F, T, T, T]
cToL '8' = [T, F, F, F]
cToL '9' = [T, F, F, T]
cToL 'a' = [T, F, T, F]
cToL 'b' = [T, F, T, T]
cToL 'c' = [T, T, F, F]
cToL 'd' = [T, T, F, T]
cToL 'e' = [T, T, T, F]
cToL 'f' = [T, T, T, T]
cToL c = error (Text.pack $ "WTF" <> [c])

parseContent :: Text -> _
parseContent = textToBits

display = map f
  where
    f True = '1'
    f False = '0'

data PacketTree
  = Literal Int
  | Operator Int [Packet]
  deriving (Show)

data Packet = Packet Int PacketTree
  deriving (Show)

decode :: [Bool] -> (Packet, [Bool])
decode bits =
  let (version, rest) = splitAt 3 bits
      (packedId, rest') = splitAt 3 rest
      (packet, rest'') = case packedId of
        [T, F, F] -> decodeLiteral rest'
        op -> decodeOperator (boolToInt op) rest'
   in (Packet (boolToInt version) packet, rest'')

decodeLiteral bits' = (Literal (boolToInt lit), leftover)
  where
    (lit, leftover) = go bits'
    go bits
      | continue = first (group<>) $ go rest
      | otherwise = (group, rest)
      where
        (continue : group, rest) = splitAt 5 bits

decodeOperator _ [] = error "Empty operator"
decodeOperator op (lengthTypeId : bits) =
  case lengthTypeId of
    False ->
      let (totalLengthInBits, rest) = splitAt 15 bits
          (packets, rest') = splitAt (boolToInt totalLengthInBits) rest
       in (Operator op (readToCompletion packets), rest')
    True ->
      let (nbSubs, rest) = splitAt 11 bits
          (subs, rest'') = readNPackets (boolToInt nbSubs) rest
       in (Operator op subs, rest'')

-- * Generics

boolToInt :: [Bool] -> Int
boolToInt l = foldl' (\x y -> x * 2 + b2i y) 0 l

b2i True = 1
b2i False = 0

readToCompletion [] = []
readToCompletion bits =
  let (pt, rest) = decode bits
   in pt : readToCompletion rest

readNPackets 0 bits = ([], bits)
readNPackets n (decode -> (packet, bits)) =
  let (packets, rest) = readNPackets (n -1) bits
   in (packet : packets, rest)

-- * FIRST problem

sumVersions :: Packet -> Int
sumVersions (Packet v sub) = v + sumVersionPacketTree sub

sumVersionPacketTree e = case e of
  Literal _ -> 0
  Operator _ subs -> sum (map sumVersions subs)

day :: _ -> Int
day = sumVersions . fst . decode

-- * SECOND problem

evalPacket :: Packet -> Int
evalPacket (Packet _ sub) = evalPacketTree sub

evalPacketTree e = case e of
  Literal i -> i
  Operator op subs -> evalOp op (map evalPacket subs)

evalOp :: Int -> [Int] -> Int
evalOp 0 l = sum l
evalOp 1 l = product l
evalOp 2 l = minimum l
evalOp 3 l = maximum l
evalOp 5 [a, b] = if a > b then 1 else 0
evalOp 6 [a, b] = if a < b then 1 else 0
evalOp 7 [a, b] = if a == b then 1 else 0
evalOp _ _ = error "Unknown operator"

day' :: _ -> Int
day' = evalPacket . fst . decode

-- * Tests

simpleLiteral = parseContent "D2FE28"

opPacket0 = parseContent "38006F45291200"

opPacket1 = parseContent "EE00D40C823060"

ex = parseContent "A0016C880162017C3686B18A3D4780"

exs =
  map
    parseContent
    ( Text.lines
        [str|\
C200B40A82
04005AC33890
880086C3E88112
CE00C43D881120
D8005AC2A8F0
F600BC2D8F
9C005AC2F8F0
9C0141080250320F1802104A08\
|]
    )

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 31
    it "of second star" $ do
      map day' exs `shouldBe` [3, 54, 7, 9, 1, 0, 0, 1]
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 993
    it "on second star" $ do
      day' fileContent `shouldBe` 144595909277
