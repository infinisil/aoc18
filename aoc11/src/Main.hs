{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.List
import           Data.Ord
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Debug.Trace
import           Paths_aoc11
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
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
  print $ part1 input 3
  print $ part2 input
  return ()

part1 :: Input -> Int -> ((Int, Int), Int)
part1 input size = ((mo + 1, di + 1), c) where
  s = zip [0..] $ concat $ sums input size
  (m, c) = maximumBy (comparing snd) s
  (di, mo) = m `divMod` (n - size + 1)

part2 :: Input -> (Int, ((Int, Int), Int))
part2 input = maximumBy (comparing (snd.snd)) $ map (\i -> trace (show i) (i, part1 input i)) [1..n]

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
