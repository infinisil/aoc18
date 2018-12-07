{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Paths_aoc7
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type Order = (Char, Char)
type Input = [Order]

parser :: Parser Input
parser = many ((,) <$> (string "Step " *> anyChar <* string " must be finished before step ") <*> (anyChar <* string " can begin.") <* newline) <* eof

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
    Right input -> challenges $ map swap input

part1 :: Input -> [Char]
part1 input = undefined

swap (a, b) = (b, a)

allReady :: [Char] -> [Order] -> [Char]
allReady all input = sort $ map fst $ filter snd $ map (\c -> (c, maybe True (const False) $ findIndex ((==c) . fst) input)) all where

dothem :: [Char] -> [Order] -> [Char]
dothem all xs = case allReady all xs of
  []     -> all
  (r:rs) -> r : dothem (filter (/=r) all) (filter (\(a, b) -> b /= r) xs)


type Worker = Maybe (Char, Int)


data WorkerState = WorkerState
  { todo    :: [Char]
  , reqs    :: [Order]
  , workers :: [Maybe (Char, Int)]
  }

trans :: State WorkerState ()
trans = undefined

getWork :: State WorkerState (Maybe (Char, Int))
getWork = do
  t <- gets todo
  r <- gets reqs
  case allReady t r of
    [] -> return Nothing
    (x:_) -> do
      modify $ \state -> state
        { todo = filter (/=x) t
        }
      return $ Just (x, 60 + 1 + (ord x - ord 'A'))

giveWork :: State WorkerState ()
giveWork = do
  ws <- gets workers
  newworkers <- forM ws $ \w -> do
    case w of
      Just x -> return $ Just x
      Nothing -> do
        work <- getWork
        return work
  modify $ \state -> state
    { workers = newworkers
    }
  return ()

passMinute :: Int -> State WorkerState Int
passMinute n = do
  modify $ \state -> state
    { workers = map (fmap (fmap (\x -> x - 1))) $ workers state
    }
  ws <- gets workers
  neww <- forM ws $ \w -> do
    case w of
      Just (c, 0) -> do
        modify $ \state -> state
          { reqs = filter (\(a, b) -> b /= c) $ reqs state
          }
        return Nothing
      _           -> return w
  modify $ \state -> state
    { workers = neww
    }
  giveWork
  w <- gets workers
  case all empty w of
    True  -> return n
    False -> passMinute (n + 1)
  where
    empty Nothing = True
    empty _       = False


decreaseTime :: Worker -> Worker
decreaseTime Nothing       = Nothing
decreaseTime (Just (c, i)) = Just (c, i - 1)

nextMinute :: [Worker] -> [Char] -> [Order] -> Int
nextMinute workers all input = undefined where
  xx = map decreaseTime workers
  done (Just (c, 0)) = [c]
  done _             = []
  alldone = concatMap done xx
  newinput = filter (\(a, b) -> b `notElem` alldone) input
  newall = filter (\x -> x `notElem` alldone) all
  r = case allReady newall newinput of
    [] -> 0
    xs -> 1 + nextMinute undefined newall newinput

allTasks :: Input -> [Char]
allTasks input = nub $ map fst input ++ map snd input

challenges :: Input -> IO ()
challenges input = do
  let all = allTasks input
  print $ dothem all input
  print $ evalState (passMinute 0) (WorkerState all input (replicate 5 Nothing))
  return ()

