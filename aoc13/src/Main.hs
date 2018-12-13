{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.Bits
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V
import           Data.Void
import           Paths_aoc13
import           System.Directory     (doesFileExist)
import           System.Environment   (getArgs)
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

type Parser = Parsec Void Text


data Corner = Positive | Negative deriving Show

data Tile = Straight
          | Corner Corner
          | Intersection
          | Empty
          deriving Show

data Direction = U | R | D | L deriving (Show, Enum)
data Decision = GoLeft | GoStraight | GoRight deriving Show
data Cart = Cart Decision Direction deriving Show

newtype Coord = Coord (Int, Int) deriving Eq
instance Ord Coord where
  Coord (x1, y1) `compare` Coord (x2, y2) = (y1, x1) `compare` (y2, x2)
instance Show Coord where
  show (Coord (x, y)) = show x ++ "," ++ show y

type Track = Vector (Vector Tile)
type Carts = Map Coord Cart

type Input = (Track, Carts)

delta :: Coord -> Direction -> Coord
delta (Coord (x, y)) dir = Coord $ case dir of
  U -> (x, y - 1)
  D -> (x, y + 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

turn :: Bool -> Direction -> Direction
turn True (fromEnum -> val)  = toEnum $ (val + 1) `mod` 4
turn False (fromEnum -> val) = toEnum $ (val - 1) `mod` 4

deflect :: Corner -> Direction -> Direction
deflect Positive (fromEnum -> val) = toEnum $ val `xor` bit 0
deflect Negative (fromEnum -> val) = toEnum $ val `xor` (bit 0 .|. bit 1)

newDirDec :: Tile -> Direction -> Decision -> (Direction, Decision)
newDirDec tile dir dec = case (tile, dec) of
  (Straight, _)              -> (dir, dec)
  (Corner corner, _)         -> (deflect corner dir, dec)
  (Intersection, GoStraight) -> (dir, GoRight)
  (Intersection, GoLeft)     -> (turn False dir, GoStraight)
  (Intersection, GoRight)    -> (turn True dir, GoLeft)
  (Empty, _)                 -> error "Your cart derailed!"

parser :: Parser Input
parser = bimap V.fromList Map.unions . unzip <$> many parseLine
  where
  parseLine :: Parser (Vector Tile, Map Coord Cart)
  parseLine = bimap V.fromList Map.unions . unzip <$> many parseTile <* newline
  parseTile :: Parser (Tile, Map Coord Cart)
  parseTile = do
    SourcePos _ line column <- getPosition
    let cart = Map.singleton (Coord (unPos column - 1, unPos line - 1)) . Cart GoLeft
    notChar '\n' >>= \c -> return $ case c of
      ' '  -> (Empty, Map.empty)
      '-'  -> (Straight, Map.empty)
      '|'  -> (Straight, Map.empty)
      '/'  -> (Corner Positive, Map.empty)
      '\\' -> (Corner Negative, Map.empty)
      '+'  -> (Intersection, Map.empty)
      '>'  -> (Straight, cart R)
      '<'  -> (Straight, cart L)
      '^'  -> (Straight, cart U)
      'v'  -> (Straight, cart D)

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
    Left err    -> fail $ show err
    Right input -> return input

main = readInput >>= challenges

challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  print $ part2 input

part1 :: Input -> Coord
part1 (track, carts) = evalState go carts
  where
  go :: State Carts Coord
  go = tick track >>= \case
    [] -> go
    (coord:_) -> return coord

part2 :: Input -> Coord
part2 (track, carts) = evalState go carts where
  go :: State Carts Coord
  go = do
    tick track
    gets ((==1) . Map.size) >>= \case
      False -> go
      True -> gets (fst . Map.elemAt 0)

tick :: Track -> State Carts [Coord]
tick track = do
  carts <- get
  map snd . Map.toAscList <$> Map.traverseMaybeWithKey (curry (step track)) carts

-- | Moves a single cart one step
step :: Track -> (Coord, Cart) -> State Carts (Maybe Coord)
step track (coord, Cart dec dir) = gets (Map.member coord) >>= \case
  -- Don't move carts that have been removed already due to a collision
  False -> return Nothing
  True -> do
    -- Delete our cart from the cart coordinate map, we would insert it at a different coordinate later
    modify (Map.delete coord)
    let newCoord@(Coord (x, y)) = delta coord dir
    gets (Map.member newCoord) >>= \case
      -- If there's already a cart where we want to move, that's a collision!
      True -> do
        modify (Map.delete newCoord)
        return $ Just newCoord
      False -> do
        -- x and y are inverted here because we read the file per-line, but the line-wise coordinate is y
        let tile = track ! y ! x
            (newdir, newdec) = newDirDec tile dir dec
        modify $ Map.insert newCoord (Cart newdec newdir)
        return Nothing


