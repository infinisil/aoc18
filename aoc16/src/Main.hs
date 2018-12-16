{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Data.Bits
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Vector.Unboxed        (Vector, (!), (//))
import qualified Data.Vector.Unboxed        as V
import           Data.Void
import           Debug.Trace
import           Paths_aoc16
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data EncodedOp = EncodedOp Int Int Int Int deriving (Show, Eq, Ord)

opcode :: EncodedOp -> Int
opcode (EncodedOp code _ _ _) = code

type Registers = Vector Int

data Sample = Sample
  { sampleBefore :: Registers
  , sampleOp     :: EncodedOp
  , sampleAfter  :: Registers
  } deriving (Show, Eq, Ord)

type Input = ([Sample], [EncodedOp])

parser :: Parser Input
parser = (,) <$> (many (parseSample <* newline)) <*> (newline *> newline *> many (parseEOp <* newline)) where
  parseEOp :: Parser EncodedOp
  parseEOp = EncodedOp <$> decimal <*> d <*> d <*> d where
    d = space1 *> decimal
  parseSample :: Parser Sample
  parseSample = Sample <$> (string "Before: " *> parseRegisters <* newline) <*> (parseEOp <* newline) <*> (string "After:  " *> parseRegisters <* newline)
  parseRegisters :: Parser Registers
  parseRegisters = V.fromList <$> (char '[' *> decimal `sepBy` string ", " <* char ']')

data Operation = Operation
  { opName :: String
  , opFun  :: Int -> Int -> Int -> State Registers ()
  }

instance Eq Operation where
  Operation a _ == Operation b _ = a == b

instance Ord Operation where
  Operation a _ `compare` Operation b _ = a `compare` b

instance Show Operation where
  show (Operation name _) = "Op " ++ name

opr :: String -> (Int -> Int -> Int) -> Operation
opr name op = Operation name $ \a b c -> modify (\v -> v // [(c, (v ! a) `op` (v ! b))])

opi :: String -> (Int -> Int -> Int) -> Operation
opi name op = Operation name $ \a b c -> modify (\v -> v // [(c, (v ! a) `op` b)])

addr = opr "addr" (+)
addi = opi "addi" (+)
mulr = opr "mulr" (*)
muli = opi "muli" (*)
banr = opr "banr" (.&.)
bani = opi "bani" (.&.)
borr = opr "borr" (.|.)
bori = opi "bori" (.|.)

setr = Operation "setr" $ \a _ c -> modify (\v -> v // [(c, v ! a)])
seti = Operation "seti" $ \a _ c -> modify (\v -> v // [(c, a)])

gtir = Operation "gtir" $ \a b c -> modify (\v -> v // [(c, if a > v ! b then 1 else 0)])
gtri = Operation "gtri" $ \a b c -> modify (\v -> v // [(c, if v ! a > b then 1 else 0)])
gtrr = Operation "gtrr" $ \a b c -> modify (\v -> v // [(c, if v ! a > v ! b then 1 else 0)])

eqir = Operation "eqir" $ \a b c -> modify (\v -> v // [(c, if a == v ! b then 1 else 0)])
eqri = Operation "eqri" $ \a b c -> modify (\v -> v // [(c, if v ! a == b then 1 else 0)])
eqrr = Operation "eqrr" $ \a b c -> modify (\v -> v // [(c, if v ! a == v ! b then 1 else 0)])

operations :: Set Operation
operations = Set.fromList
  [ addr, addi
  , mulr, muli
  , banr, bani
  , borr, bori
  , setr, seti
  , gtir, gtri, gtrr
  , eqir, eqri, eqrr
  ]

behavesLike :: Sample -> Operation -> Bool
behavesLike (Sample initial (EncodedOp _ a b c) goal) (Operation _ op) = execState (op a b c) initial == goal

behavingGE3 :: Sample -> Bool
behavingGE3 sample = Set.size (Set.filter (behavesLike sample) operations) >= 3

part1 :: Input -> Int
part1 (samples, _) = length $ filter behavingGE3 samples

possibleOperations :: Set Operation -> Sample -> Set Operation
possibleOperations ops sample = Set.filter (behavesLike sample) ops

possibles :: Set Operation -> Int -> Set Sample -> Set Operation
possibles leftOperations code samples = foldl possibleOperations leftOperations samples

operationMapping :: [Sample] -> Map Int Operation
operationMapping samples = go possibleOpsForCode where
  samplesForCode = Map.fromListWith Set.union $ map (\sample -> (opcode (sampleOp sample), Set.singleton sample)) samples
  possibleOpsForCode = Map.mapWithKey (possibles operations) samplesForCode

  go :: Map Int (Set Operation) -> Map Int Operation
  go m
    | Map.null notUnique = done
    | otherwise = Map.union done (go notDone)
    where
    (unique, notUnique) = Map.partition ((==1) . Set.size) m
    done = Map.map (Set.elemAt 0) unique
    doneOps = Set.fromList $ Map.elems done
    notDone = Map.map (`Set.difference` doneOps) notUnique

runWithMapping :: Map Int Operation -> EncodedOp -> State Registers ()
runWithMapping mapping = trace (show mapping) $ \(EncodedOp code a b c) -> opFun (mapping Map.! code) a b c

part2 :: Input -> Int
part2 (samples, program) = evalState result initial
  where
  runit = runWithMapping (operationMapping samples)
  initial = V.generate 4 (const 0)
  result = do
    traverse runit program
    gets (!0)

time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

main = readInput >>= challenges

readInput :: IO Input
readInput = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> fail $ show err
    Right input -> return input

challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  print $ part2 input
  return ()

