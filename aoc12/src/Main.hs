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
  print $ snd $ generation input 20
  print $ snd $ generation input 50000000000
  return ()

generation :: Input -> Int -> ([Bool], Int)
generation (Input init _) n = findN n detectUnchanging where
  iterations = iterate step (0, init)
  detectUnchanging = zipWith unchanged iterations (tail iterations)
  unchanged (i, this) (j, next)
    | this == next = ((i, this), Just (j - i))
    | otherwise = ((i, this), Nothing)

findN :: Int -> [((Int, [Bool]), Maybe Int)] -> ([Bool], Int)
findN 0 (((startIndex, tape), _):_) = (tape, sum $ zipWith (\i v -> if v then i else 0) [startIndex ..] tape)
findN n (((startIndex, tape), Just diff):_) = (tape, sum $ zipWith (\i v -> if v then i + n * diff else 0) [startIndex ..] tape)
findN n ((_, Nothing):rest) = findN (n - 1) rest

rulesToMap :: [Rule] -> Map (P, P, P, P, P) P
rulesToMap rules = Map.fromList $ map (\(Rule c r) -> (c, r)) rules

step :: (Int, [Bool]) -> (Int, [Bool])
step (startIndex, tape) = clean (startIndex - 2, result) where
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
