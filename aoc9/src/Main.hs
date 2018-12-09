{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.CircularList          (CList)
import qualified Data.CircularList          as CL
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.Maybe

type Marble = Int

data GameState = GameState
  { _field  :: !(CList Marble)
  , _scores :: !(IntMap Int)
  , _player :: !Int
  } deriving Show

makeLenses ''GameState

highscore :: Int -> Int -> Int
highscore count marbles = maximum $ IntMap.elems $ execState (mapM (step count) [1..marbles]) initialState ^. scores

main :: IO ()
main = do
  print $ highscore 435 71184
  print $ highscore 435 (100 * 71184)

initialState :: GameState
initialState = GameState
  { _field = CL.singleton 0
  , _scores = IntMap.empty
  , _player = 0
  }

step :: Int -> Int -> State GameState ()
step count newMarble
  | newMarble `mod` 23 == 0 = do
      p <- use player
      field %= CL.rotNL 7
      m <- fromJust . CL.focus <$> use field
      scores %= IntMap.insertWith (+) p (newMarble + m)
      field %= CL.removeR
      player %= (`mod` count) . (+1)
  | otherwise = do
      field %= CL.insertL newMarble . CL.rotR
      player %= (`mod` count) . (+1)
