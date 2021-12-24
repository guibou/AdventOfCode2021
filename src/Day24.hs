module Day24 where

import Control.Monad
import qualified Data.Map as Map
import Data.SBV
import qualified Data.Text as Text
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (Prelude.some (parseInstruction <* (eof <|> void "\n")))

-- * Generics

data Variable = W | X | Y | Z deriving (Show, Eq, Ord)

parseVariable :: Parser Variable
parseVariable =
  (W <$ "w")
    <|> (X <$ "x")
    <|> (Y <$ "y")
    <|> (Z <$ "z")

data Operand = VarO Variable | NumberO Integer deriving (Show)

parseOperand = do
  (VarO <$> parseVariable) <|> (NumberO <$> parseNumber)

data Instruction
  = Inp Variable
  | BinInst BinOp Variable Operand
  deriving (Show)

data BinOp = Add | Mul | Div | Mod | Eql deriving (Show)

instr2 ctor = BinInst <$> (ctor <$ (symbol s)) <*> parseVariable <*> (" " *> parseOperand)
  where
    s = Text.toLower (Text.pack (show ctor))

parseInstruction =
  choice
    [ Inp <$ "inp " <*> parseVariable,
      instr2 Add,
      instr2 Mul,
      instr2 Div,
      instr2 Mod,
      instr2 Eql
    ]

buildVariable name = do
  v <- sInt64 ("s" <> show name)

  constrain $ (v .>= 1 .&& v .<= 9)

  pure v

f (m, variables, varCount) instruction = do
  case instruction of
    Inp v -> do
      var <- buildVariable varCount
      pure (Map.insert v var m, var : variables, varCount + 1)
    BinInst Mul v (NumberO 0) -> do
      pure (Map.insert v (literal 0) m, variables, varCount)
    BinInst op v o' -> do
      o <- case o' of
        VarO v' -> pure $ m Map.! v'
        NumberO i -> pure (literal (fromIntegral i))
      res <- evalBinop op (m Map.! v) o
      pure (Map.insert v res m, variables, varCount)

evalBinop :: BinOp -> SInt64 -> SInt64 -> SymbolicT IO SInt64
evalBinop op a b = case op of
  Add -> pure $ a + b
  Mul -> pure $ a * b
  Div -> do
    constrain $ b ./= 0
    pure $ a `sQuot` b
  Mod -> do
    constrain $ a .>= 0
    constrain $ b .> 0
    pure $ a `sMod` b
  Eql -> pure $ oneIf (a .== b)

instructionToProblem goal instructions = do
  let z = literal 0

  let startMap = Map.fromList [(X, z), (Y, z), (Z, z), (W, z)]
  (finalMap, variables, _) <- foldM f (startMap, [], 0 :: Int) instructions

  constrain (finalMap Map.! Z .== 0)
  goal "goal" (toNumber variables)

toNumber :: Num t => [t] -> t
toNumber = go 1
  where
    go _ [] = 0
    go p (x : xs) = p * x + go (10 * p) xs

-- * FIRST problem

day = sbvSolve maximize

day' = sbvSolve minimize

-- * SECOND problem

sbvSolve :: _ -> [Instruction] -> IO Int64
sbvSolve goal insts = do
  res <- optimize Lexicographic $ instructionToProblem goal insts
  case res of
    LexicographicResult r -> pure $ toNumber $ map Unsafe.fromJust $ (reverse (map (\i -> getModelValue ("s" <> show i) r) [0 :: Int .. 13]))
    _ -> error "WTF"

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldReturn` 36969794979199
    it "on second star" $ do
      day' fileContent `shouldReturn` 11419161313147

-- too high: 99197949796963
--
-- correct: 36969794979199
