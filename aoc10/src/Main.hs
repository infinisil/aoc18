{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Data.Either
import           Data.Functor
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Debug.Trace
import           Paths_aoc10
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text
type Point = (Vect, Vect)
type Input = [Point]

type Vect = (Int, Int)

parseInput :: Parser Input
parseInput = many (parsePoint <* newline) where
  parseInt :: Parser Int
  parseInt = space *> signed (return ()) decimal
  parsePosition :: Parser Vect
  parsePosition = (,) <$> (string "position=<" *> parseInt) <*> (char ',' *> parseInt <* char '>')
  parseVelocity :: Parser Vect
  parseVelocity = (,) <$> (string "velocity=<" *> parseInt) <*> (char ',' *> parseInt <* char '>')
  parsePoint :: Parser Point
  parsePoint = (,) <$> parsePosition <*> (space *> parseVelocity)

getFile :: FilePath -> Parser a -> IO a
getFile name parser = do
  dataFile <- getDataFileName name
  exists <- doesFileExist dataFile
  let inputFile = if exists then dataFile else name
  contents <- TIO.readFile inputFile
  case parse parser name contents of
    Left err    -> error $ show err
    Right input -> return input

readInput :: IO Input
readInput = do
  inputFile <- fromMaybe "input" . listToMaybe <$> getArgs
  getFile inputFile parseInput

type Letter = [[Bool]]
type Letters = Map Char Letter

parseLetters :: Parser Letters
parseLetters = Map.fromList <$> ((many (notChar '\n') *> newline) *> many parseLetter) where
  parseLetter :: Parser (Char, Letter)
  parseLetter = (,) <$> (anyChar <* newline) <*> many line
  line :: Parser [Bool]
  line = many (try (char ' ' $> False) <|> char '#' $> True) <* newline

readLetters :: IO Letters
readLetters = getFile "letters" parseLetters

time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

main :: IO ()
main = do
  input <- readInput
  letters <- readLetters
  let res1 = part2 input
  putStrLn $ part1 letters input res1
  print res1
  return ()


part1 :: Letters -> Input -> Int -> String
part1 letters input t = reverse res where
  (pos, vel) = unzip input
  posx = map fst pos
  posy = map snd pos
  velx = map fst vel
  vely = map snd vel
  xs = zipWith (\p v -> p + t * v) posx velx
  ys = zipWith (\p v -> p + t * v) posy vely
  minx = minimum xs
  miny = minimum ys
  lm = buildMaps letters
  lc = Map.map (length . filter id . concat) letters
  res = refineGuess (minx, miny) t lc lm IntMap.empty 0 input


mm :: Int -> [Int] -> (Int, Int)
mm dist (x:xs) = go (0, x) (0, x) 1 xs where
  go :: (Int, Int) -> (Int, Int) -> Int -> [Int] -> (Int, Int)
  go (li, _) (ui, _) _ [] = (li, ui)
  go (li, lower) (ui, upper) _ _ | upper - lower >= dist = (li, ui)
  go (li, lower) (ui, upper) i (x:xs) = case (lower `compare` x, x `compare` upper) of
    (GT, _) -> go (i, x) (ui, upper) (i + 1) xs
    (_, GT) -> go (li, lower) (i, x) (i + 1) xs
    _       -> go (li, lower) (ui, upper) (i + 1) xs



type Guess = IntMap (Set Char)

refineGuess :: (Int, Int) -> Int -> Map Char Int -> LetterMap -> Guess -> Int -> Input -> String
refineGuess _ _ lc _ guess _ [] = IntMap.foldl (\str set -> (if Set.size set == 1 then Set.elemAt 0 set else select set) : str) "" guess where
  select set = fst $ minimumBy (comparing snd) $ map (\c -> (c, lc Map.! c)) (Set.elems set)
refineGuess (minx, miny) t lc lm guess ii (((posx, posy), (velx, vely)):rest)
  | done = IntMap.foldl (\str set -> Set.elemAt 0 set : str) "" newGuess
  | otherwise = refineGuess (minx, miny) t lc lm newGuess (ii + 1) rest
  where
    (i, letterx) = (posx + t * velx - minx) `divMod` 8
    lettery = posy + t * vely - miny
    possibleChars = case Map.lookup (lettery, letterx) lm of
      Nothing -> error $"Lettermap doesn't have entry for " ++ show (lettery, letterx) ++ " with index " ++ show ii ++ " mins are " ++ show (miny, minx)
      Just e -> e
    newGuess = IntMap.insertWith Set.intersection i possibleChars guess
    done = IntMap.foldl (\b set -> b && Set.size set == 1) True newGuess

type LetterMap = Map (Int, Int) (Set Char)

-- | Builds a Map from letter coordinates to a set of characters corresponding to the characters having a point at that coordinate
buildMaps :: Letters -> LetterMap
buildMaps letters = Map.fromList [ ((x, y), Map.keysSet $ Map.filter (\l -> l !! x !! y) letters) | y <- [0..5], x <- [0..9]]

possibleTimesForY :: Int -> (Int, Int) -> (Int, Int) -> [Int]
possibleTimesForY n (p1, p2) (v1, v2) | v1 < v2 = possibleTimesForY n (p2, p1) (v2, v1)
possibleTimesForY n (p1, p2) (v1, v2) = [lower..upper] where
  b = p1 - p2
  k = v1 - v2
  lower = (-n - b + k - 1) `div` k
  upper = (n - b) `div` k

part2 :: Input -> Int
part2 input = fst res where
  (pos, vel) = unzip input
  (posx, posy) = unzip pos
  (velx, vely) = unzip vel
  n = 8
  (i1, i2) = mm n vely
  pos1 = posy !! i1
  pos2 = posy !! i2
  vel1 = vely !! i1
  vel2 = vely !! i2
  tcands = possibleTimesForY n (pos1, pos2) (vel1, vel2)
  tcandsWithBounds = map (\t -> let
                             y1 = pos1 + t * vel1
                             y2 = pos2 + t * vel2
                         in (t, (min y1 y2, max y1 y2))) tcands
  res = singleCandidate n tcandsWithBounds (zip posy vely)

-- | @singleCandidate n ts ys@ goes through the list of y parameters to find the first candidate for t from ts that passes most of them, meaning once all candidates
-- except one have been eliminated, that one is returned, even though not all parameters have been searched through. A candidate for t is invalid when the range of
-- the resulting y coordinates for all parameters exceeds the maximum range which is n.
singleCandidate :: Int -> [(Int, (Int, Int))] -> [(Int, Int)] -> (Int, (Int, Int))
singleCandidate n [t] _ = t
singleCandidate n ts ((posy, vely):rest) = singleCandidate n xx rest where
  xx = mapMaybe extendRange ts
  extendRange :: (Int, (Int, Int)) -> Maybe (Int, (Int, Int))
  extendRange (t, (miny, maxy))
    | newmax - newmin > n = Nothing
    | otherwise = Just (t, (newmin, newmax))
    where y = posy + t * vely
          newmin = min miny y
          newmax = max maxy y
