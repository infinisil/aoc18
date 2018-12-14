{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Bifunctor
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

type Input = Int

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
  V.write arr 0 3
  V.write arr 1 7
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

type A s = ContT Int (StateT ((Int, V.MVector s Word8), SeqState) (ST s))

push :: forall s . Word8 -> (Int -> A s Int) -> A s ()
push val exit = do
  ((size, arr), st) <- get
  case next st val of
    Nothing -> exit (size - 5) *> return ()
    Just st' -> do
      newarr <- if V.length arr == size then trace ("Growing by " ++ show size) $ V.grow arr size else return arr
        --V.grow arr size
      V.write newarr size val
      put ((size + 1, newarr), st')

part2 :: Int
part2 = runST $ do
  arr <- V.unsafeNew 21000000
  V.write arr 0 3
  V.write arr 1 7
  evalStateT (runContT (callCC (step'' (0, 1))) return) ((2, arr), N)

step'' :: forall s . (Int, Int) -> (Int -> A s Int) -> A s Int
step'' (i1, i2) exit = do
  v1 <- gets (snd . fst) >>= flip V.read i1
  v2 <- gets (snd . fst) >>= flip V.read i2
  let (n1, n2) = (v1 + v2) `divMod` 10

  if n1 /= 0 then do
    push n1 exit
    push n2 exit
  else
    push n2 exit
  --when (n1 /= 0) $ push arr n1
  --push arr n2
  size <- gets (fst . fst)

  let newi1 = (i1 + fromIntegral v1 + 1) `mod` size
  let newi2 = (i2 + fromIntegral v2 + 1) `mod` size

  step'' (newi1, newi2) exit

data SeqState = N
              | N8
              | N86
              | N864
              | N8648
              | N86480

next :: SeqState -> Word8 -> Maybe SeqState
next N 8      = Just N8
next N _      = Just N
next N8 6     = Just N86
next N8 8     = Just N8
next N8 _     = Just N
next N86 4    = Just N864
next N86 8    = Just N8
next N86 _    = Just N
next N864 8   = Just N8648
next N864 _   = Just N
next N8648 0  = Just N86480
next N8648 6  = Just N86
next N8648 _  = Just N
next N86480 1 = Nothing
next N86480 _ = Just N


main :: IO ()
main = do
  input <- getArgs >>= \case
    [] -> return 864801
    [arg] -> return $ read arg
  challenges input

challenges :: Input -> IO ()
challenges input = do
  print $ part1' input
  print $ part2
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

