{-# LANGUAGE LambdaCase    #-}
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
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Input = [String]

parser :: Parsec Void Text Input
parser = many (many letterChar <* newline) <* eof

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
  print $ checksum input
  putStrLn $ correctid input

checksum :: Input -> Int
checksum input = doubles * triples where
  idchecksums = map idchecksum input
  doubles = length $ filter fst idchecksums
  triples = length $ filter snd idchecksums

idchecksum str = (2 `elem` charcounts, 3 `elem` charcounts) where
  charcounts = Map.elems $ Map.fromListWith (+) $ (,1) <$> str

-- Because the default Metric instance for String is unfit
-- and we can't redefine it easily, we use a String wrapper instead
newtype Str = Str String deriving Eq

instance BK.Metric Str where
  distance (Str x) (Str y) = go x y where
    go [] [] = 0
    go (x:xs) (y:ys) =
      (if x == y then 0 else 1) + go xs ys

correctid :: Input -> String
correctid = go BK.empty where
  go tree (x:xs) = case BK.elemsDistance 1 (Str x) tree of
    []      -> go (BK.insert (Str x) tree) xs
    [Str y] -> common x y
  common [] [] = []
  common (x:xs) (y:ys)
    | x == y = x : common xs ys
    | otherwise = common xs ys

