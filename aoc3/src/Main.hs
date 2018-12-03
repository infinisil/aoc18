{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc3
import           System.Directory
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Rect = Rect
  { x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  } deriving Show

data Claim = Claim
  { claimId :: Int
  , rect    :: Rect
  }

instance Eq Claim where
  Claim id1 _ == Claim id2 _ = id1 == id2

instance Ord Claim where
  Claim id1 _ `compare` Claim id2 _ = id1 `compare` id2

type Input = [Claim]

parser :: Parsec Void Text Input
parser = many (parseClaim <* newline) <* eof
  where parseClaim = Claim <$> (char '#' *> decimal <* space1 <* char '@' <* space1) <*> parseRect
        parseRect = Rect <$> decimal <*> (char ',' *> decimal <* char ':') <*> (space1 *> decimal) <*> (char 'x' *> decimal)

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  print $ part2 input
  return ()

playfield = Rect 0 0 1000 1000

part1 :: Input -> Int
part1 input = countConflicts (map rect input) playfield

countConflicts :: [Rect] -> Rect -> Int
countConflicts [] _ = 0
countConflicts rects (Rect _ _ 1 1)
  | length rects > 1 = 1
  | otherwise = 0
countConflicts rects r = sum x where
  x = map (\p -> countConflicts (filter (intersects p) rects) p) (split r)


part2 :: Input -> Int
part2 claims = claimId $ Set.elemAt 0 nonConflicting where
  claimSet = Set.fromList claims
  nonConflicting = claimSet `Set.difference` conflicting claimSet playfield

conflicting :: Set Claim -> Rect -> Set Claim
conflicting claims _ | length claims <= 1 = Set.empty
conflicting claims (Rect _ _ 1 1) = claims
conflicting claims r = Set.unions x where
  x = map (\p -> conflicting (Set.filter (intersects p . rect) claims) p) (split r)


-- Utils
-- =====

-- | Splits a rectangle into 4 partitions
split :: Rect -> [Rect]
split (Rect x y w h) =
  [ Rect x y hw hh
  , Rect (x + hw) y hw' hh
  , Rect x (y + hh) hw hh'
  , Rect (x + hw) (y + hh) hw' hh'
  ] where
    (hw, hwr) = w `divMod` 2
    hw' = hw + hwr
    (hh, hhr) = h `divMod` 2
    hh' = hh + hhr

-- | Checks whether two rectangles intersect
intersects :: Rect -> Rect -> Bool
intersects (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = intersects1d (x1, x1 + w1) (x2, x2 + w2)
  && intersects1d (y1, y1 + h1) (y2, y2 + h2) where
    intersects1d :: (Int, Int) -> (Int, Int) -> Bool
    intersects1d (x1, y1) (x2, y2) = max x1 x2 < min y1 y2

