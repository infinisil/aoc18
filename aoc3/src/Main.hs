{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array.ST
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc3
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Claim = Claim
  { id     :: Int
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  } deriving Show
type Input = [Claim]

parser :: Parsec Void Text Input
parser = many (Claim <$> (char '#' *> decimal <* space1) <*> (char '@' *> space1 *> decimal) <*> (char ',' *> decimal <* char ':') <*> (space1 *> decimal) <*> (char 'x' *> decimal) <* newline) <* eof

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> return "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

range :: Input -> (Int, Int)
range input = (wide, high) where
  wide = maximum $ map (\claim -> left claim + width claim) input
  high = maximum $ map (\claim -> left claim + width claim) input

challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  return ()

type Fabric = [[Int]]

addClaim :: Claim -> State Fabric ()
addClaim claim =
  forM_ [(left claim)..(left claim + width claim)] $ \x ->
    forM_ [(top claim)..(top claim + height claim)] $ \y ->
      modify $ trans (trans (+1) y) x

addClaim' :: Claim -> ST s (STArray s i e)
addClaim' = undefined

trans :: (a -> a) -> Int -> [a] -> [a]
trans _ _ []     = error "empty list"
trans f 0 (x:xs) = f x : xs
trans f n (x:xs) = x : trans f (n-1) xs

part1 :: Input -> Int
part1 input = countConflicts fabric where
  fabric = execState (forM input addClaim) (replicate 1000 (replicate 1000 0))

countConflicts :: Fabric -> Int
countConflicts = length . filter (>=2) . concat

part2 :: Input -> ()
part2 input = undefined
