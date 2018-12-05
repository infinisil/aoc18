{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Random
import           Data.Char
import           Data.List
import           Data.Text                       (Text)
import qualified Data.Text.IO                    as TIO
import           Data.Time.Clock
import           Data.Void
import           Paths_aoc5
import           Statistics.Distribution.Poisson
import           System.Directory                (doesFileExist)
import           System.Environment              (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Input = String

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  input <- head . lines <$> readFile inputFile
  challenges input

time :: IO a -> IO a
time io = do
  start <- getCurrentTime
  result <- io
  stop <- getCurrentTime
  print $ stop `diffUTCTime` start
  return result

challenges :: Input -> IO ()
challenges input = do
  time $ print $ part1 input
  time $ print $ part2 input
  return ()

react :: String -> String
react = go [] where
  go stack []      = stack
  go [] (c:cs)     = go [c] cs
  go (x:xs) (c:cs)
    | toLower x == toLower c && x /= c = go xs cs
    | otherwise = go (c:x:xs) cs

part1 :: Input -> Int
part1 input = length $ react input

part2 :: Input -> Int
part2 input = minimum $ map (\c -> length $ react $ filter ((/=c) . toLower) input') ['a'..'z'] where
  input' = react input
