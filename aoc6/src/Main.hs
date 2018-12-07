{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Applicative        (liftA2)
import           Control.Monad.RWS.Strict
import           Data.Char
import           Data.Functor
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Ord
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
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

readInput :: FilePath -> IO Input
readInput inputFile = do
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> error $ show err
    Right input -> return input

time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  readInput inputFile >>= challenges

challenges :: Input -> IO ()
challenges input = do
  time $ print $ part1 input
  time $ print $ part2 10000 input
  return ()


-- Part 1
-- ======

part1 :: Input -> Int
part1 input = maximum $ Map.elems finiteCounts where
  (field, border) = enclosure input
  infiniteIndices = Set.fromList $ map (findNearest input) border
  counts = Map.fromListWith (+) $ map ((,1) . findNearest input) field
  finiteCounts = Map.withoutKeys counts infiniteIndices

distance :: Tile -> Tile -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

bounds :: Input -> ((X, X), (Y, Y))
bounds input = (xs, ys) where
  xs = liftA2 (,) minimum maximum $ map fst input
  ys = liftA2 (,) minimum maximum $ map snd input

enclosure :: Input -> ([Tile], [Tile])
enclosure input = (field, border) where
  ((minx, maxx), (miny, maxy)) = bounds input
  field = [ (x, y) | x <- [minx .. maxx], y <- [miny .. maxy] ]
  border = [ (x, y) | x <- [minx - 1 .. maxx + 1], y <- [miny - 1 ,  maxy + 1] ]
        ++ [ (x, y) | x <- [minx - 1 ,  maxx + 1], y <- [miny - 1 .. maxy + 1] ]

uniqueMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
uniqueMinimumBy cmp [] = Nothing
uniqueMinimumBy cmp (x:xs) = r where
  r = case go cmp (x, True) xs of
    (m, True)  -> Just m
    (_, False) -> Nothing
  go cmp v [] = v
  go cmp (y, valid) (x:xs) = case x `cmp` y of
    LT -> go cmp (x, True) xs
    EQ -> go cmp (y, False)  xs
    GT -> go cmp (y, valid) xs

findNearest :: Input -> Tile -> Maybe Int
findNearest input tile = fmap fst . uniqueMinimumBy (comparing snd) .
  zip [0..] $ map (distance tile) input

printField :: Input -> String
printField input = intercalate "\n" [ rr | y <- [ miny - 1 .. maxy + 1 ], let rr = [ showInt r | x <- [ minx - 1 .. maxx + 1 ], let r = findNearest input (x, y) ]] where
  ((minx, maxx), (miny, maxy)) = bounds input
  showInt Nothing  = '.'
  showInt (Just n) = chr (ord 'a' + n)


-- Part 2
-- ======

part2 :: Int -> Input -> Int
part2 maxSum input = slicesToArea slices where
  median = medianTile input
  within tile = (<maxSum) . sum . map (distance tile) $ input
  initialState = TurtleState median PosX Nothing
  (_, _, slices) = runRWS findOutline within initialState

-- | Algorithm to find the outline of the area
--
-- 0. Make sure we're inside the area
-- 1. Walk until we've gone outside the area, mark this as the start
-- 2. Turn left and step forward until we're in again
-- 3. Turn right and step forward until we're out again
-- 4. Repeat 2 and 3 until we're back at the start again
findOutline :: Turtle ()
findOutline = do
  isIn <- check
  when isIn $ do
    stepOutside
    markStart
    goround False

-- | Check whether the current tile is in the area
check :: Turtle Bool
check = do
  pos <- gets position
  ($pos) <$> ask

-- | Go forward through the area until we aren't in the area anymore
stepOutside :: Turtle ()
stepOutside = do
  x <- check
  when x $ step *> stepOutside

-- | Mark the start of the algorithm at the current position
markStart :: Turtle ()
markStart = do
  pos <- gets position
  modify $ \state -> state { roundStart = Just pos }

-- | Make a turnaround in the given turn direction until we've switched from in area -> out of area or vice versa.
-- Then reverse our turn direction and continue. Terminate when we've reached the starting position.
-- Report each position in the area we find to be inbound
goround :: Turn -> Turtle ()
goround t = do
  turn t
  end <- step
  unless end $ do
    inIt <- maybeReport
    goround inIt

-- | Do a single step forward, return whether we've reached our starting tile already
step :: Turtle Bool
step = do
  state@TurtleState { direction, position, roundStart } <- get
  let (x, y) = position
      newPos = case direction of
        PosX -> (x + 1, y)
        NegX -> (x - 1, y)
        PosY -> (x, y + 1)
        NegY -> (x, y - 1)
  put $ state { position = newPos }
  return $ roundStart == Just newPos

-- | Check whether the current tile is in bounds or not, if it is, report this position as inbound
maybeReport :: Turtle Bool
maybeReport = do
  isIn <- check
  when isIn $ do
    pos <- gets position
    tell $ tileSlice pos
  return isIn

-- | Turn the turtle either left or right
turn :: Turn -> Turtle ()
turn turn = modify mod where
  mod state@TurtleState { direction } = state { direction = newDir turn direction }
  newDir :: Turn -> Dir -> Dir
  newDir True PosX  = PosY
  newDir True PosY  = NegX
  newDir True NegX  = NegY
  newDir True NegY  = PosX
  newDir False PosY = PosX
  newDir False NegX = PosY
  newDir False NegY = NegX
  newDir False PosX = NegY


-- | Finds the median tile in the input, having the unique property of certainly being within the area already
-- This is because the median has the same amount of tiles up, down, left and right (because we get the middle in x
-- and the middle in y), meaning any movement will either keep the same summed distance (because we get one block
-- nearer to the ones above at the same time as we go one block farther from all below, same for left-right) to all
-- of them or increase it (when we go up and some points are exactly to our left/right, then we increase the
-- distance to those, while all of the rest stays the same)
--
-- Thanks to glguy on Freenode for explaining me this
medianTile :: Input -> Tile
medianTile input = (x !! halflen, y !! halflen) where
  halflen = length input `div` 2
  x = sort $ map fst input
  y = sort $ map snd input


-- | Slices of a bounded pixelated 2d area. Key represent an X coordinate, Values represent minY, maxY on that X
-- coordinate. This structure can't represent non-convexity in the Y direction.
newtype Slices = Slices (IntMap (Y, Y)) deriving Show

-- | Slices get combined by merging each of the X coordinates, taking the mimimum and maximum of the Y coordinates
-- This means the area between in the Y axis gets filled out
instance Semigroup Slices where
  Slices a <> Slices b = Slices $ IntMap.unionWith combine a b where
    combine (mina, maxa) (minb, maxb) = (min mina minb, max maxa maxb)

instance Monoid Slices where
  mempty = Slices IntMap.empty

-- | A slice consisting of only a single tile
tileSlice :: Tile -> Slices
tileSlice (x, y) = Slices $ IntMap.singleton x (y, y)

-- | Calculates the are of these slices, essentially integration.
slicesToArea :: Slices -> Int
slicesToArea (Slices m) = IntMap.foldr (\(mi, ma) s -> s + ma - mi + 1) 0 m

-- | A turtle program does
-- - Read a function for determining whether a tile is in the area or not
-- - Write out the slices representing the reported area
-- - have a State for the turtle
type Turtle = RWS (Tile -> Bool) Slices TurtleState

-- | The state of the turtle
data TurtleState = TurtleState
  { position   :: Tile -- ^ Our current position
  , direction  :: Dir -- ^ Our current direction
  , roundStart :: Maybe Tile -- ^ Whether the round started already and where it started
  }

data Dir = PosX
         | NegX
         | PosY
         | NegY

-- | A turn direction, either left, represented by False, or right, represented by True
type Turn = Bool


