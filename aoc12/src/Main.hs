{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

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

type Parser = Parsec Void Text

type P = Bool
data Rule = Rule
  { condition :: (P, P, P, P, P)
  , result    :: P
  } deriving Show

data Input = Input
  { initial :: [P]
  , rules   :: [Rule]
  } deriving Show

parser :: Parser Input
parser = Input <$> parseInitial <*> (newline *> many parseRule) where
  parseRule :: Parser Rule
  parseRule = Rule <$> parseCondition <*> (string " => " *> parsePlant <* newline)
  parseCondition :: Parser (P, P, P, P, P)
  parseCondition = (,,,,) <$> parsePlant <*> parsePlant <*> parsePlant <*> parsePlant <*> parsePlant
  parseInitial :: Parser [P]
  parseInitial = string "initial state: " *> many parsePlant <* newline
  parsePlant :: Parser P
  parsePlant = try (char '#' $> True) <|> char '.' $> False

readInput :: IO Input
readInput = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> error $ show err
    Right input -> return input

main :: IO ()
main = readInput >>= challenges

challenges :: Input -> IO ()
challenges input = do
  print $ part1 20 input
  return ()

part1 :: Int -> Input -> Int
part1 its (Input init rules) = sum $ mapMaybe f ii where
  m = rulesToMap rules
  i = iterate (step m) (0, init)
  f (_, False) = Nothing
  f (i, True)  = Just i
  (startIndex, res) = i !! its
  ii = zip [startIndex..] res

part2 :: Int -> Input -> Int
part2 its (Input init rules) = y where
  m = rulesToMap rules
  i = iterate (step m) (0, init)
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

step :: Map (P, P, P, P, P) P -> (Int, [Bool]) -> (Int, [Bool])
step rules (startIndex, tape) = clean (startIndex - 2, snd result) where
  result = mapAccumL fun (False, False, False, False) (tape ++ [False, False, False, False])
  fun (p1, p2, p3, p4) this = ((p2, p3, p4, this), l) where
    l = Map.findWithDefault False (p1, p2, p3, p4, this) rules

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
