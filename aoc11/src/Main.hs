{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Ord
import           Data.Text                   (Text)
import qualified Data.Text.IO                as TIO
import           Data.Void
import           Debug.Trace
import           Paths_aoc11
import           System.Directory            (doesFileExist)
import           System.Environment          (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Text.Printf

type Parser = Parsec Void Text
type Input = Int

parser :: Parser Input
parser = decimal

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = do
  putStrLn $ part1 input
  putStrLn $ part2 input
  return ()

part1 :: Input -> String
part1 input = show x ++ "," ++ show y where
  ((x, y), c) = forSize input 3

forSize :: Input -> Int -> ((Int, Int), Int)
forSize input size = ((mo + 1, di + 1), c) where
  s = zip [0..] $ concat $ sums input size
  (m, c) = maximumBy (comparing snd) s
  (di, mo) = m `divMod` (n - size + 1)

part2 :: Input -> String
part2 input = pretty $ printingMaxBy pretty (comparing (snd.snd)) values where
  values = parMap rdeepseq (\i -> (i, forSize input i)) [1..n]
  pretty (index, ((x, y), count)) = "Potential solution: " ++ show x ++ "," ++ show y ++ "," ++ show index

printingMaxBy :: forall a . (a -> String) -> (a -> a -> Ordering) -> [a] -> a
printingMaxBy s f (x:xs) = go x xs where
  go :: a -> [a] -> a
  go y [] = x
  go y (x:xs)
    | x `f` y == GT = trace (s x) $ go x xs
    | otherwise = go y xs

n = 300

pretty :: [[Int]] -> String
pretty points = intercalate "\n" (map (concatMap (printf "%+.02d ")) points) where

grid :: Input -> [[Int]]
grid serial = [ [ powerLevel serial (x, y) | x <- [1..n] ] | y <- [1..n] ]

sums :: Input -> Int -> [[Int]]
sums input size =
  [
    [
      sum $ concat gyx
      | x <- [1..(n - size + 1)]
      , let gyx = map (take size . drop (x - 1)) gy
    ]
    | y <- [1..(n - size + 1)]
    , let gy = take size $ drop (y - 1) g
  ] where
  g = grid input

powerLevel :: Input -> (Int, Int) -> Int
powerLevel serial (x, y) = onlyHundreds - 5 where
  rackid = x + 10
  start = rackid * y
  withserial = start + serial
  withrack = withserial * rackid
  onlyHundreds = (withrack `mod` 1000) `div` 100
