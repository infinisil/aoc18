{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SeqDetector where

import           Data.Fix
import           Data.List
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           GHC.Word
import           KMP

newtype State = State Word8

newtype Fun a = Fun (Word8 -> Maybe a) deriving Functor

test :: [Word8] -> Fix Fun
test seq = ana f (0, 0) where
  f :: (Int, Int) -> Fun (Int, Int)
  f (i, j) = Fun $ \input -> case n i j input of
    (Just _, _, _)    -> Nothing
    (Nothing, i', j') -> Just (i', j')
  n = next (build seq)

initial = State 0

detector :: forall i . (Bounded i, Integral i) => [i] -> (State -> i -> (Bool, State))
detector seq = \(State s) n -> State <$> result `V.unsafeIndex` fromIntegral s `VU.unsafeIndex` fromIntegral (n + minBound) where
  result = V.generate (length seq) forLen
  withoutLast = init seq
  forLen :: Int -> VU.Vector (Bool, Word8)
  forLen l = VU.generate (fromIntegral (maxBound @i) - fromIntegral (minBound @i) + 1) x where
    x :: Int -> (Bool, Word8)
    x b = (full == seq, fromIntegral $ length new)
      where
      full = take l seq ++ [fromIntegral b]
      new = head $ filter (`isPrefixOf` withoutLast) (tails full)

--data SeqState = N
--              | N8
--              | N86
--              | N864
--              | N8648
--              | N86480
--
--fixed864801 :: State -> Word8 -> (Bool, SeqState)
--fixed864801 N 8      = (False, N8)
--fixed864801 N _      = (False, N)
--fixed864801 N8 6     = (False, N86)
--fixed864801 N8 8     = (False, N8)
--fixed864801 N8 _     = (False, N)
--fixed864801 N86 4    = (False, N864)
--fixed864801 N86 8    = (False, N8)
--fixed864801 N86 _    = (False, N)
--fixed864801 N864 8   = (False, N8648)
--fixed864801 N864 _   = (False, N)
--fixed864801 N8648 0  = (False, N86480)
--fixed864801 N8648 6  = (False, N86)
--fixed864801 N8648 _  = (False, N)
--fixed864801 N86480 1 = (True, N)
--fixed864801 N86480 _ = (False, N)
