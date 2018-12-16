{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Applicative        (liftA2)
import           Control.Concurrent         (threadDelay)
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Either
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
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as V
import           Data.Void
import           Debug.Trace
import           Paths_aoc15
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           System.IO
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Type = Goblin | Elf deriving (Eq, Show)

opponent :: Type -> Type
opponent Goblin = Elf
opponent Elf    = Goblin

data Unit = Unit
  { unitType   :: Type
  , unitHealth :: Int
  } deriving Show

unitAttack :: Unit -> Int
unitAttack = const 3

type Field = Vector (Vector Bool)
type Units = Map (Int, Int) Unit

type Walls = (Int, Int) -> Bool

data Input = Input
  { field :: Field
  , units :: Units
  } deriving Show

walls :: Field -> Walls
walls field (x, y) = field ! x ! y

parser :: Parser Input
parser = uncurry Input . bimap V.fromList mconcat . unzip <$> many parseLine <* eof where
  parseLine :: Parser (Vector Bool, Units)
  parseLine = bimap V.fromList mconcat . unzip <$> many parseTile <* newline
  parseTile :: Parser (Bool, Units)
  parseTile = do
    SourcePos _ (subtract 1 . unPos -> x) (subtract 1 . unPos -> y) <- getPosition
    let singleUnit t = Map.singleton (x, y) (Unit t 200)
    flip fmap (notChar '\n') $ \case
      '#' -> (True, mempty)
      '.' -> (False, mempty)
      'E' -> (False, singleUnit Elf)
      'G' -> (False, singleUnit Goblin)

nextMove :: Units -> Type -> (Int, Int) -> Walls -> Maybe (Int, Int)
nextMove units target starting walls
  -- Can't move if surrounded
  | null freeSurrounding = Nothing
  -- Start the path search with all free surrounding spaces, our walls now include the starting tile (we don't want to include this in the future)
  | otherwise = go (Map.fromSet id freeSurrounding) (liftA2 (||) (starting==) walls)
  where
    freeSurrounding = Set.filter (isFree walls units) (nextTo starting)

    -- | Takes a map mapping from which positions we're taking into account currently to the first path part of it
    -- Returns the next step we should take
    go :: Map (Int, Int) (Int, Int) -> Walls -> Maybe (Int, Int)
    go ways walls
      -- We found a tile that's next to a target, return the smallest one (reading order)
      | not (null targs) = Just $ snd $ Map.findMin targs
      -- We don't have any more tiles we could explore, no destination to move to
      | null next = Nothing
      -- We haven't fonud anything in this round, expand to new territorry while blocking the already visited
      | otherwise = go next (liftA2 (||) (\k -> Map.member k ways) walls)
      where
        targs = Map.filterWithKey isNextToTarget ways
        isNextToTarget k _ = any (isType target units) (nextTo k)
        combine a k v = Map.unionWith min a (Map.fromSet (const v) n) where
          -- Only empty or target fields
          n = Set.filter (\k -> maybe True ((==target) . unitType) (Map.lookup k units) && not (walls k)) $ nextTo k

        next = Map.foldlWithKey combine Map.empty ways

isFree :: Walls -> Units -> (Int, Int) -> Bool
isFree w units coords = not (w coords) && not (Map.member coords units)

isType :: Type -> Map (Int, Int) Unit -> (Int, Int) -> Bool
isType t units coords = maybe False ((==t) . unitType) $ Map.lookup coords units

printField :: (Int, Int) -> Walls -> StateT Units IO ()
printField (maxX, maxY) walls = do
  u <- get
  liftIO $ forM_ [0..maxX] $ \x -> do
    forM_ [0..maxY] $ \y -> case Map.lookup (x, y) u of
      Nothing -> putStr $ if walls (x, y) then "#" else "."
      Just u  -> putStr $ if unitType u == Goblin then "G" else "E"
    let ux = map snd $ Map.toAscList $ Map.filterWithKey (\(kx, ky) v -> kx == x) u
    unless (null ux) $ putStr "  "
    x <- forM ux $ \(Unit t h) -> do
      return $ (if t == Goblin then "G" else "E") ++ "(" ++ show h ++ ")"
    putStr $ intercalate ", " x
    putStrLn ""


step :: Int -> Walls -> (Int, Int) -> StateT Units IO Bool
step elfpower walls coords = gets (Map.lookup coords) >>= \case
  Nothing -> return True
  Just unit@(Unit t unitHealth) -> do
    (g, e) <- gets $ Map.partition ((==Goblin) . unitType)
    if null g || null e then
      return False
    else do
      u <- gets $ Map.delete coords
      let prevTargets = targetsFor u (nextTo coords) (opponent t)
      (targets, moved) <- if Set.null prevTargets then
        case nextMove u (opponent t) coords walls of
          Nothing -> do
            put $ Map.insert coords unit u
            return (prevTargets, False)
          Just newCoords -> do
            when (coords == newCoords) $ liftIO $ print $ "Didnt move!"
            put $ Map.insert newCoords unit u
            return (targetsFor u (nextTo newCoords) (opponent t), True)
      else do
        put $ Map.insert coords unit u
        return (prevTargets, False)
      attacked <- attack (if t == Elf then elfpower else 3) targets

      return True

attack :: Int -> Set (Int, Int) -> StateT Units IO Bool
attack power targets
  | null targets = return False
  | otherwise = do
      u <- get
      let (coords, Unit t health) = minimumBy (comparing (unitHealth . snd)) $ map (\k -> (k, u Map.! k)) (Set.toAscList targets)
          newHealth = health - power
      x <- gets $ Map.member coords
      unless x $ error "hi"
      if newHealth <= 0 then
        modify $ Map.delete coords
      else do
        modify $ Map.insert coords (Unit t newHealth)
      return True

run :: Int -> Int -> Walls -> StateT Units IO Int
run elfpower n walls = do
  notDone <- tick elfpower n walls
  if notDone then run elfpower (n + 1) walls else return n

s = 31

part1 :: Int -> Input -> IO Int
part1 ep (Input (walls -> w) u) = flip evalStateT u $ do
  --printField (s, s) w
  n <- run ep 0 w
  u <- Map.elems <$> get
  let left = sum $ map unitHealth u
  --liftIO $ putStrLn $ "Ended after " ++ show n
  --liftIO $ putStrLn $ "Health left " ++ show left
  return $ n * left

part2 :: Input -> IO Int
part2 i@(Input (walls -> w) u) = do
  ep <- go u w 3
  part1 ep i
  where
  go :: Units -> Walls -> Int -> IO Int
  go u w e = do
    l <- evalStateT (losses e w) u
    if l == 0 then return e else go u w (e + 1)

losses :: Int -> Walls -> StateT Units IO Int
losses ep w = do
  start <- Map.size <$> gets (Map.filter ((==Elf) . unitType))
  n <- run ep 0 w
  stop <- Map.size <$> gets (Map.filter ((==Elf) . unitType))
  return $ start - stop

tick :: Int -> Int -> Walls -> StateT Units IO Bool
tick elfpower n walls = do
  u <- get
  moves <- Map.traverseWithKey (\k v -> step elfpower walls k) u
  let notDone = Map.foldl (&&) True moves
  --liftIO $ putStrLn $ "\nAfter " ++ show (n + 1) ++ " rounds"
  --printField (s, s) walls
  --liftIO $ threadDelay 100000
  return notDone

targetsFor :: Units -> Set (Int, Int) -> Type -> Set (Int, Int)
targetsFor units next target = nearTargets where
  nearTargets = Set.filter (\(x, y) -> maybe False ((==target) . unitType) $ Map.lookup (x, y) units) next

  -- TODO: Make sure this one isn't dead
  --u <- get
  --let c = connected u (opponent unitType) (x, y)



nextTo :: (Int, Int) -> Set (Int, Int)
nextTo (x, y) = Set.fromList
  [ (x - 1, y), (x + 1, y)
  , (x, y - 1), (x, y + 1)
  ]

time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

readInput :: FilePath -> IO Input
readInput inputFile = do
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> fail $ show err
    Right input -> return input

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
  part1 3 input >>= print
  part2 input >>= print
  return ()

