{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Bifunctor
import           Data.List
import           Data.Text                   (Text)
import qualified Data.Text.IO                as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import qualified Data.Vector.Unboxed         as IV
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Void
import           Debug.Trace
import           GHC.Word
import           Paths_aoc14
import           System.Directory            (doesFileExist)
import           System.Environment          (getArgs)
import           Text.Megaparsec             hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import qualified SeqDetector                 as D

type Parser = Parsec Void Text

type Input = Int

parser :: Parser Input
parser = decimal

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

time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

part1' :: Input -> IV.Vector Word8
part1' input = runST (steps input)

steps :: forall s . Input -> ST s (IV.Vector Word8)
steps input = do
  arr <- V.unsafeNew (2 + input * 2)
  V.unsafeWrite arr 0 3
  V.unsafeWrite arr 1 7
  step' (input + 10) (2, (0, 1)) arr
  IV.freeze (V.take 10 (V.drop input arr))

step' :: forall s . Int -> (Int, (Int, Int)) -> V.MVector s Word8 -> ST s ()
step' n (size, (i1, i2)) arr
  | size >= n = return ()
  | otherwise = do
    v1 <- V.read arr i1
    v2 <- V.read arr i2
    let (n1, n2) = (v1 + v2) `divMod` 10
    when (n1 /= 0) $ V.write arr size n1
    V.write arr (size + fromIntegral n1) n2
    let newsize = size + 1 + fromIntegral n1

    let newi1 = (i1 + fromIntegral v1 + 1) `mod` newsize
    let newi2 = (i2 + fromIntegral v2 + 1) `mod` newsize

    step' n (newsize, (newi1, newi2)) arr


data AState s = AState
  { size     :: Int
  , arr      :: V.MVector s Word8
  , seqState :: D.State
  , indices  :: (Int, Int)
  }

type CST s = ContT Int (StateT (AState s) (ST s))

push :: forall s a . (D.State -> Word8 -> (Bool, D.State)) -> Word8 -> (Int -> CST s ()) -> CST s ()
push d val exit = do
  AState size arr st i <- get
  {-# SCC "case" #-} case d st val of
    (True, _) -> exit (size - 5)
    (False, st') -> do
      newarr <- if V.length arr == size then V.unsafeGrow arr size else return arr
      V.unsafeWrite newarr size val
      put $ AState (size + 1) newarr st' i

digits :: forall i . Integral i => i -> [i]
digits 0 = [0]
digits n = reverse $ go n where
  go :: i -> [i]
  go 0 = []
  go n = r : go d where
    (d, r) = quotRem n 10

part2 :: Input -> Int
part2 input = runST $ do
  arr <- V.unsafeNew 2
  V.unsafeWrite arr 0 3
  V.unsafeWrite arr 1 7
  let d = D.detector (map fromIntegral $ digits input)
  flip evalStateT (AState 2 arr D.initial (0, 1)) $
    runContT (callCC (step'' d)) return

step'' :: forall s . (D.State -> Word8 -> (Bool, D.State)) -> (Int -> CST s ()) -> CST s Int
step'' d exit = do
  AState _ arr _ (i1, i2) <- get
  v1 <- V.unsafeRead arr i1
  v2 <- V.unsafeRead arr i2
  let (n1, n2) = (v1 + v2) `divMod` 10

  {-# SCC "ifthing" #-}if n1 /= 0 then do
    push d n1 exit
    push d n2 exit
  else
    push d n2 exit
  --when (n1 /= 0) $ push arr n1
  --push arr n2
  s <- gets size

  let newi1 = (i1 + fromIntegral v1 + 1) `mod` s
  let newi2 = (i2 + fromIntegral v2 + 1) `mod` s

  modify $ \state -> state { indices = (newi1, newi2) }

  step'' d exit

newtype Digit = Digit Word8 deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show Digit where
  show (Digit n) = show n

instance Bounded Digit where
  minBound = Digit 0
  maxBound = Digit 9

data SeqState = N
              | N8
              | N86
              | N864
              | N8648
              | N86480

next :: SeqState -> Word8 -> (Bool, SeqState)
next N 8      = (False, N8)
next N _      = (False, N)
next N8 6     = (False, N86)
next N8 8     = (False, N8)
next N8 _     = (False, N)
next N86 4    = (False, N864)
next N86 8    = (False, N8)
next N86 _    = (False, N)
next N864 8   = (False, N8648)
next N864 _   = (False, N)
next N8648 0  = (False, N86480)
next N8648 6  = (False, N86)
next N8648 _  = (False, N)
next N86480 1 = (True, N)
next N86480 _ = (False, N)


main :: IO ()
main = readInput >>= challenges

challenges :: Input -> IO ()
challenges input = do
  print input
  print $ part1' input
  print $ part2 input
  return ()

nextRecipes :: Int -> Int -> [Int]
nextRecipes a b
  | first == 0 = [second]
  | otherwise = [first, second] where
  (first, second) = (a + b) `divMod` 10

part1 :: Input -> [Int]
part1 input = take 10 $ drop input $ snd $ execState (replicateM input step) ((0, 1), [3, 7])

step :: State ((Int, Int), [Int]) ()
step = do
  ((a, b), l) <- get
  let next = nextRecipes (l !! a) (l !! b)
      newl = l ++ next
      newa = (a + (l !! a) + 1) `mod` length newl
      newb = (b + (l !! b) + 1) `mod` length newl
  put ((newa, newb), newl)
  return ()

