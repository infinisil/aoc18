{-# LANGUAGE LambdaCase    #-}
module Main where

import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc@day@
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Input = [Int]

parser :: Parsec Void Text Input
parser = many (decimal <* newline) <* eof

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> getDataFileName "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = do
  return ()

