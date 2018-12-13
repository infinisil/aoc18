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
  --mapM_ (putStrLn . showField . fst . generation input) [0..]
  return ()

generation :: Input -> Int -> ([Bool], Int)
generation (Input init _) = findN detectUnchanging where
  iterations = iterate step (0, init)
  detectUnchanging = zipWith unchanged iterations (tail iterations)
  unchanged (i, this) (j, next)
    | this == next = ((i, this), Just (j - i))
    | otherwise = ((i, this), Nothing)


findN :: [((Int, [Bool]), Maybe Int)] -> Int -> ([Bool], Int)
findN (((startIndex, tape), _):_) 0 = (tape, sum $ zipWith (\i v -> if v then i else 0) [startIndex ..] tape)
findN (((startIndex, tape), Just diff):_) n = (tape, sum $ zipWith (\i v -> if v then i + n * diff else 0) [startIndex ..] tape)
findN ((_, Nothing):rest) n = findN rest (n - 1)

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
