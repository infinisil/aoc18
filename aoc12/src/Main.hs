{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Data.Fix
import           Data.Functor
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc12
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           RuleGeneration


$(generateRules)

main :: IO ()
main = readInput >>= challenges

challenges :: Input -> IO ()
challenges input = do
  print $ part1 20 input
  print $ part2 50000000000 input
  return ()

part1 :: Int -> Input -> Int
part1 its (Input init _) = sum $ mapMaybe f ii where
  i = iterate step' (0, init)
  f (_, False) = Nothing
  f (i, True)  = Just i
  (startIndex, res) = i !! its
  ii = zip [startIndex..] res

part2 :: Int -> Input -> Int
part2 its (Input init _) = y where
  i = iterate step' (0, init)
  (index, ((startIndex, res), diff)) = head $ catMaybes $ zipWith (\i m -> (i,) <$> m) [0..] $ zipWith t i (tail i)
  f (_, False) = Nothing
  f (i, True)  = Just i
  x = sum $ mapMaybe f $ zip [startIndex..] res
  count = length $ filter id $ res
  y = diff * (its - index) * count + x


t (ithis, this) (inext, next)
  | this == next = Just ((ithis, this), inext - ithis)
  | otherwise = Nothing

rulesToMap :: [Rule] -> Map (P, P, P, P, P) P
rulesToMap rules = Map.fromList $ map (\(Rule c r) -> (c, r)) rules

step' :: (Int, [Bool]) -> (Int, [Bool])
step' (startIndex, tape) = clean (startIndex - 2, result) where
  extendedTape = tape ++ [False, False, False, False]
  result = cata cataThing ffff extendedTape
  cataThing :: Fun ([Bool] -> [Bool]) -> [Bool] -> [Bool]
  cataThing (Fun fun) [] = []
  cataThing (Fun fun) (x:xs) = a : b xs where
    (a, b) = fun x

-- | Cleans a pair of (startIndex, pots) into a new pair such that pots has all False values cut from the ends
clean :: (Int, [P]) -> (Int, [P])
clean (i, []) = (i, [])
clean (i, False:ts) = clean (i + 1, ts)
clean (i, True:ts) = (i, True : cleanright ts) where
  cleanright [] = []
  cleanright (True:ts) = True : cleanright ts
  cleanright (False:ts) = case cleanright ts of
    [] -> []
    t  -> False : t

showField :: [Bool] -> String
showField []           = []
showField (True:rest)  = '#' : showField rest
showField (False:rest) = '.' : showField rest

-- ((P, P, P, P) -> P -> ((P, P, P, P), P)) ->
