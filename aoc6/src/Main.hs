{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Applicative        (liftA2)
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Ord
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc6
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type X = Int
type Y = Int
type Tile = (X, Y)
type Input = [Tile]

parser :: Parser Input
parser = many ((,) <$> decimal <* string ", " <*> decimal <* newline) <* eof

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

enclosure' :: Input -> ((Int, Int), (Int, Int))
enclosure' input = (xs, ys) where
  xs = liftA2 (,) minimum maximum $ map fst input
  ys = liftA2 (,) minimum maximum $ map snd input

printField :: Input -> String
printField input = intercalate "\n" [ rr | y <- [ miny - 1 .. maxy + 1 ], let rr = [ showInt r | x <- [ minx - 1 .. maxx + 1 ], let r = findNearest input (x, y) ]] where
  ((minx, maxx), (miny, maxy)) = enclosure' input
  showInt Nothing  = '.'
  showInt (Just n) = chr (ord 'a' + n)


enclosure :: Input -> ([Tile], [Tile])
enclosure input = (field, border) where
  ((minx, maxx), (miny, maxy)) = enclosure' input
  field = [ (xx, yy) | xx <- [minx .. maxx], yy <- [miny .. maxy] ]
  border = [ (xx, yy) | xx <- [minx - 1 .. maxx + 1], yy <- [miny - 1 ,  maxy + 1] ]
        ++ [ (xx, yy) | xx <- [minx - 1 ,  maxx + 1], yy <- [miny - 1 .. maxy + 1] ]

uniqueMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
uniqueMinimumBy f [] = Nothing
uniqueMinimumBy f (x:xs) = r where
  r = case go f (x, True) xs of
    (m, True)  -> Just m
    (_, False) -> Nothing
  go :: (a -> a -> Ordering) -> (a, Bool) -> [a] -> (a, Bool)
  go _ v [] = v
  go f (y, valid) (x:xs) = case x `f` y of
    LT -> go f (x, True) xs
    EQ -> go f (y, False)  xs
    GT -> go f (y, valid) xs

findNearest :: Input -> Tile -> Maybe Int
findNearest input tile = fmap snd $ uniqueMinimumBy (comparing fst) $ zip z [0..] where
  z = map (distance tile) input

distance :: Tile -> Tile -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part1 :: Input -> Int
part1 input = maximum $ Map.elems xx where
  (field, border) = enclosure input
  infiniteIndices = Set.fromList $ map (findNearest input) border
  test = Map.fromListWith (+) $ map ((,1) . findNearest input) field
  xx = Map.withoutKeys test infiniteIndices

doit :: [Tile] -> [(Tile, Int)] -> Int -> [(Tile, Int)]
doit [] candidates maxDist = candidates
doit (t:ts) candidates maxDist = doit ts yy maxDist where
  xx = map (\(tile, v) -> (tile, v + distance tile t)) candidates
  yy = filter ((<maxDist) . snd) xx

part2 :: Input -> Int -> Int
part2 ((x, y):xs) maxSize = length $ doit xs init maxSize where
  init = [ ((xx, yy), d) | xx <- [x - maxSize .. x + maxSize], yy <- [y - maxSize .. y + maxSize], let d = distance (xx, yy) (x, y), d < maxSize ]

challenges :: Input -> IO ()
challenges input = do
  print input
  print $ part1 input
  print $ part2 input 10000
  return ()

