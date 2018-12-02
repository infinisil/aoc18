module Main where

import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc1
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Input = [Int]

parser :: Parsec Void Text Input
parser = many (signed (return ()) decimal <* newline) <* eof

main :: IO ()
main = do
  inputFile <- getDataFileName "input"
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = mapM_ (print . ($ input)) [part1, part2]

part1 :: Input -> Int
part1 = sum

part2 :: Input -> Int
part2 input = firstDuplicate sums
  where sums = scanl (+) 0 (cycle input)

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = go Set.empty
  where go set (x:xs)
          | x `Set.member` set = x
          | otherwise = go (x `Set.insert` set) xs

