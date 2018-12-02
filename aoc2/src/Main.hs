{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Set.BKTree            (BKTree)
import qualified Data.Set.BKTree            as BK
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc2
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Input = [String]

parser :: Parsec Void Text Input
parser = many (many letterChar <* newline) <* eof

main :: IO ()
main = do
  inputFile <- getDataFileName "input"
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = do
  print $ checksum input
  putStrLn $ correctid input

checksum :: Input -> Int
checksum input = doubles * triples where
  idchecksums = map idchecksum input
  doubles = length $ filter fst idchecksums
  triples = length $ filter snd idchecksums

idchecksum str = (2 `elem` charcounts, 3 `elem` charcounts) where
  charcounts = Map.elems $ Map.fromListWith (+) $ (,1) <$> str

correctid :: Input -> String
correctid = go BK.empty where
  go tree (x:xs) = case BK.elemsDistance 1 x tree of
    []  -> go (BK.insert x tree) xs
    [y] -> common x y
  common [] [] = []
  common (x:xs) (y:ys)
    | x == y = x : common xs ys
    | otherwise = common xs ys

