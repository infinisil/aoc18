{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc8
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type Input = Tree

data Tree = Node [Tree] [Int] deriving Show

parseTree :: Parser Tree
parseTree = do
  childCount <- decimal
  char ' '
  metaCount <- decimal
  children <- replicateM childCount (char ' ' *> parseTree)
  metas <- replicateM metaCount (char ' ' *> decimal)
  return $ Node children metas

parser :: Parser Input
parser = parseTree

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parseTree "input" contents of
    Left err    -> error $ show err
    Right input -> challenges input
  return ()

challenges :: Input -> IO ()
challenges input = do
  print $ sumMetadata input
  print $ value input
  return ()

sumMetadata :: Input -> Int
sumMetadata (Node sub meta) = sum meta + sum (map sumMetadata sub)

value :: Input -> Int
value (Node [] meta) = sum meta
value (Node sub meta) = sum $ map (value . (sub !!)) $ filter inBounds $ map (subtract 1) meta where
  inBounds i = i >= 0 && i < length sub

