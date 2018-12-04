{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Monad.Random
import           Data.Bits
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set.BKTree            (BKTree, Metric)
import qualified Data.Set.BKTree            as BK
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           Data.Char                  (chr, ord)
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void                  (Void)
import           GHC.Word                   (Word32)
import           Paths_aoc2                 (getDataFileName)
import           System.Directory
import           System.Environment         (getArgs)
import           Text.Printf                (printf)

type Input = [String]

parser :: Parsec Void Text Input
parser = many (many letterChar <* newline) <* eof

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> getDataFileName "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input

challenges :: Input -> IO ()
challenges input = do
  print $ checksum input
  putStrLn $ correctid input

checksum :: Input -> Int
checksum input = doubles * triples where
  idchecksums = map idchecksum input
  doubles = length $ filter fst idchecksums
  triples = length $ filter snd idchecksums

idchecksum str = (2 `elem` charcounts, 3 `elem` charcounts) where
  charcounts = Map.elems $ Map.fromListWith (+) $ (,1) <$> str

correctid :: Input -> String
correctid = go BK.empty where
  go tree (x:xs) = case BK.elemsDistance 1 quints tree of
    []  -> go (BK.insert quints tree) xs
    [q] -> common x (qDecode 26 q)
    where quints = qEncode x

common [] [] = []
common (x:xs) (y:ys)
  | x == y = x : common xs ys
  | otherwise = common xs ys



-- Quint implementation
-- ====================

{-|
A Quint is a way to encode Strings consisting only of maximum 32 lowercased letters.
It is designed to allow for calculating the hamming distance between two of them very fast.

Every Word32 contains a single bit of every character of the string.
The least significant bits contain the last character.
-}
newtype Quint = Quint (Word32, Word32, Word32, Word32, Word32) deriving Eq

instance Show Quint where
  show (Quint (q0, q1, q2, q3, q4)) = printf "%032b\n%032b\n%032b\n%032b\n%032b" q0 q1 q2 q3 q4

instance Metric Quint where
  -- This is the main thing this representation is optimized for
  -- Calculating the hamming distance between two strings is only a couple bit operations!
  Quint (x0, x1, x2, x3, x4) `distance` Quint (y0, y1, y2, y3, y4) = popCount $
    xor x0 y0 .|. xor x1 y1 .|. xor x2 y2 .|. xor x3 y3 .|. xor x4 y4

setIf False _ = zeroBits
setIf True n  = bit n

qEncode :: String -> Quint
qEncode = go (Quint (0, 0, 0, 0, 0)) where
  go :: Quint -> String -> Quint
  go quints [] = quints
  go (Quint (q0, q1, q2, q3, q4)) (c:cs) = go (Quint (q0', q1', q2', q3', q4')) cs where
    n = fromIntegral $ ord c - ord 'a'

    q0' = shiftL q0 1 .|. (shiftR n 0 .&. bit 0)
    q1' = shiftL q1 1 .|. (shiftR n 1 .&. bit 0)
    q2' = shiftL q2 1 .|. (shiftR n 2 .&. bit 0)
    q3' = shiftL q3 1 .|. (shiftR n 3 .&. bit 0)
    q4' = shiftL q4 1 .|. (shiftR n 4 .&. bit 0)

qDecode :: Int -> Quint -> String
qDecode n q = reverse $ go n q where
  go 0 _ = []
  go k (Quint (q0, q1, q2, q3, q4)) = c : go (k-1) (Quint (shiftR q0 1, shiftR q1 1, shiftR q2 1, shiftR q3 1, shiftR q4 1)) where
    c = chr $ ord 'a' + fromIntegral n

    n = shiftL (q0 .&. bit 0) 0
      .|. shiftL (q1 .&. bit 0) 1
      .|. shiftL (q2 .&. bit 0) 2
      .|. shiftL (q3 .&. bit 0) 3
      .|. shiftL (q4 .&. bit 0) 4



-- Random generation
-- =================

randomList :: MonadRandom m => Int -> m [String]
randomList chars = chunked chars <$> getRandomRs ('a', 'z') where
  chunked n list = chunk : chunked n rest where
    (chunk, rest) = splitAt n list

changeLetter :: MonadRandom m => String -> m String
changeLetter input = do
  index <- getRandomR (0, length input - 1)
  let oldletter = input !! index
  newletter <- head . filter (/=oldletter) <$> getRandomRs ('a', 'z')
  return $ replace newletter index input
  where
    replace :: a -> Int -> [a] -> [a]
    replace _ _ []     = error "empty list"
    replace v 0 (x:xs) = v : xs
    replace v n (x:xs) = x : replace v (n-1) xs

randomInput :: MonadRandom m => Int -> Int -> m (Input, String)
randomInput len chars = do
  list <- take (len - 1) <$> randomList chars
  orig <- uniform list
  changed <- changeLetter orig
  return (list ++ [changed], common orig changed)

generateInputs :: IO ()
generateInputs = do
  createDirectoryIfMissing True "inputs"
  solutions <- forM ns $ \n -> do
    putStr $ "Generating input for length " ++ show n ++ "... "
    (input, solution) <- randomInput n 26
    putStr "Finished. Now writing to file... "
    writeFile ("inputs/" ++ show n ++ ".txt") (unlines input)
    putStrLn "Finished."
    return (n, solution)
  writeFile "inputs/solutions.txt" $ concatMap (\(n, s) -> show n ++ " " ++ s ++ "\n") solutions
  where
    ns = [1000,2000..9000] ++ [10000,20000..100000]
