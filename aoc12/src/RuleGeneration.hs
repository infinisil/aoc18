{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module RuleGeneration where

import           Data.Fix
import           Data.Functor
import           Data.List
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Language.Haskell.TH
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Rule = Rule
  { condition :: (P, P, P, P, P)
  , result    :: P
  } deriving (Show, Eq, Ord)

data Input = Input
  { initial :: [P]
  , rules   :: [Rule]
  } deriving Show

parser :: Parser Input
parser = Input <$> parseInitial <*> (newline *> many parseRule) where
  parseRule :: Parser Rule
  parseRule = Rule <$> parseCondition <*> (string " => " *> parsePlant <* newline)
  parseCondition :: Parser (P, P, P, P, P)
  parseCondition = (,,,,) <$> parsePlant <*> parsePlant <*> parsePlant <*> parsePlant <*> parsePlant
  parseInitial :: Parser [P]
  parseInitial = string "initial state: " *> many parsePlant <* newline
  parsePlant :: Parser P
  parsePlant = try (char '#' $> True) <|> char '.' $> False


transformRules :: [Rule] -> [Rule']
transformRules = map f . groupBy cond . sort where
  cond (Rule (l1, l2, l3, l4, _) _) (Rule (r1, r2, r3, r4, _) _) =
    (l1, l2, l3, l4) == (r1, r2, r3, r4)
  f [Rule (c1, c2, c3, c4, _) fn, Rule _ tn] =
    Rule' [c1, c2, c3, c4] (fn, tn)

type P = Bool
data Rule' = Rule' [P] (P, P)

readInput :: IO Input
readInput = do
  contents <- TIO.readFile "input"
  case parse parser "input" contents of
    Left err    -> fail $ show err
    Right input -> return input

newtype Fun a = Fun { unFun :: Bool -> (Bool, a) } deriving Functor

generateRules :: Q [Dec]
generateRules = transformRules . rules <$> runIO readInput >>= mapM genDec where
  genDec :: Rule' -> Q Dec
  genDec (Rule' prev (retFalse, retTrue)) = do
    let expr = UInfixE (ConE 'Fix) (VarE '($))
             $ UInfixE (ConE 'Fun) (VarE '($))
             $ LamCaseE
               [ createCase False retFalse
               , createCase True retTrue
               ]
    return $ FunD (mkName $ toChar <$> prev) [Clause [] (NormalB expr) []]
    where
      createCase this out = Match (ConP (toBool this) []) (NormalB result) []
        where target = mkName (toChar <$> tail prev ++ [this])
              result = TupE [ConE (toBool out), VarE target]
              toBool True  = 'True
              toBool False = 'False

  toChar False = 'f'
  toChar True  = 't'
