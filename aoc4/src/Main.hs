{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Data.Functor
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time
import           Data.Void
import           Paths_aoc4
import           System.Environment         (getArgs)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text


type GuardId = Int
data Event = ShiftBegin' GuardId
              | FallsAsleep'
              | WakesUp'
              deriving (Eq, Ord, Show)
data LogEntry = LogEntry
  { logTime  :: LocalTime
  , logEvent :: Event
  } deriving (Eq, Ord, Show)
type Input' = [LogEntry]


data ShiftEvent = FallsAsleep | WakesUp deriving Show
data ShiftEntry = ShiftEntry
  { time  :: LocalTime
  , event :: ShiftEvent
  } deriving Show
data Shift = Shift
  { startTime :: LocalTime
  , guard     :: GuardId
  , entries   :: [ShiftEntry]
  } deriving Show
type Input = [Shift]


parser :: Parser Input'
parser = many entryParser <* eof where
  entryParser :: Parser LogEntry
  entryParser = LogEntry <$> timeParser <*> (char ' ' *> eventParser <* newline)
  timeParser :: Parser LocalTime
  timeParser = char '[' *> many (notChar ']') <* char ']' >>= parseTimeM False defaultTimeLocale "%F %R"
  eventParser :: Parser Event
  eventParser = ShiftBegin' <$> (string "Guard #" *> decimal <* string " begins shift")
           <|> string "falls asleep" $> FallsAsleep'
           <|> string "wakes up" $> WakesUp'

process :: Input' -> Input
process input = byShifts sortedInput [] where
  sortedInput = sortBy (flip compare) input

  byShifts :: [LogEntry] -> [ShiftEntry] -> [Shift]
  byShifts [] entries = []
  byShifts (LogEntry t (ShiftBegin' i):rest) entries = Shift t i entries : byShifts rest []
  byShifts (LogEntry t FallsAsleep':rest) entries = byShifts rest (ShiftEntry t FallsAsleep:entries)
  byShifts (LogEntry t WakesUp':rest) entries = byShifts rest (ShiftEntry t WakesUp:entries)

main = readInput >>= challenges

readInput :: IO Input
readInput = do
  inputFile <- getArgs >>= \case
    [] -> return "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> fail $ show err
    Right input -> return $ process input

collectByGuard :: Semigroup a => (LocalTime -> [ShiftEntry] -> a) -> Input -> Map GuardId a
collectByGuard f = go Map.empty where
  go m []                       = m
  go m (Shift t i entries:rest) = go (Map.insertWith (<>) i (f t entries) m) rest


sleepAmount :: [ShiftEntry] -> NominalDiffTime
sleepAmount [] = 0
sleepAmount (ShiftEntry sleep FallsAsleep:ShiftEntry wake WakesUp:rest) = wake `diffLocalTime` sleep <> sleepAmount rest

newtype SleepMinutes = SleepMinutes (IntMap Int) deriving Show

instance Semigroup SleepMinutes where
  SleepMinutes x <> SleepMinutes y = SleepMinutes $ IntMap.unionWith (+) x y

sleepMinutes :: [ShiftEntry] -> SleepMinutes
sleepMinutes [] = SleepMinutes IntMap.empty
sleepMinutes (ShiftEntry sleep FallsAsleep:ShiftEntry wake WakesUp:rest) = SleepMinutes x <> sleepMinutes rest where
  m = todMin . localTimeOfDay
  x = IntMap.fromDistinctAscList (map (,1) [m sleep..m wake])

part1 :: Map GuardId SleepMinutes -> Input -> [Int]
part1 sleeps input = map (maxSleeper*) maxMinutes where
  maxSleeper = fst . maximumBy (comparing snd) . Map.assocs . collectByGuard (const sleepAmount) $ input
  SleepMinutes minutes = sleeps Map.! maxSleeper
  maxMinutes = snd . fromJust . maxElemsBy compare . IntMap.assocs $ minutes

part2 :: Map GuardId SleepMinutes -> Input -> [Int]
part2 sleeps input = map (uncurry (*)) maxes where
  Just (_, maxes) = doubleMax sleeps

challenges :: Input -> IO ()
challenges input = do
  let sleeps = collectByGuard (const sleepMinutes) input
  print $ part1 sleeps input
  print $ part2 sleeps input
  return ()

byMinute :: Map GuardId SleepMinutes -> [Map GuardId Int]
byMinute sleeps = map (\minute -> Map.map (\(SleepMinutes minutes) -> IntMap.findWithDefault 0 minute minutes) sleeps) [0..59]

doubleMax :: Map GuardId SleepMinutes -> Maybe (Int, [(GuardId, IntMap.Key)])
doubleMax m = z where
  x = fmap (\(SleepMinutes y) -> IntMap.assocs y) <$> Map.assocs m
  y = maxElemsBy (comparing (\l -> if null l then 0 else maximum . map snd $ l)) x
  z :: Maybe (Int, [(GuardId, IntMap.Key)])
  z = y >>= (\(a, b) -> fmap (\minutes -> [ (i, m) | i <- b, m <- minutes ]) <$> maxElemsBy compare a)

-- Utils
instance Semigroup NominalDiffTime where
  x <> y = x + y

-- Because our version of time doesn't have this function yet
diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

-- This could really be in some library
maxElemsBy :: (v -> v -> Ordering) -> [(k, v)] -> Maybe (v, [k])
maxElemsBy f = go Nothing where
  go m [] = m
  go Nothing ((minute, amount):rest) = go (Just (amount, [minute])) rest
  go (Just (maxAmount, minutes)) ((minute, amount):rest) = case f amount maxAmount of
    GT -> go (Just (amount, [minute])) rest
    EQ -> go (Just (amount, minute:minutes)) rest
    LT -> go (Just (maxAmount, minutes)) rest
