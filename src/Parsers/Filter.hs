{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Filter parser
module Parsers.Filter where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import qualified Data.Attoparsec as A
import Data.Attoparsec.Text
       (Parser, choice, skipWhile, char, asciiCI, many', many1,
        scientific, scan, anyChar, takeWhile, takeWhile1, inClass)
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.ISO8601
import Parsers.Unescape
import Prelude ()
import Prelude.Compat
import Types.Common

expr :: Parser FilterExpr
expr = foldl mkExprParser simpleExpr [("and", And), ("or", Or)]

mkExprParser :: Parser FilterExpr -> (Text, FilterBooleanOperator) -> Parser FilterExpr
mkExprParser expr' (opName, opVal) = do
  t <- expr' <* skipSpace
  assoc t <|> return t
  where
    op = mkOp (opName, opVal)
    assoc' t = assoc t <|> return t
    assoc e1 = do
      e2 <- (op <* skipSpace1) *> expr' <* skipSpace
      assoc' $ FilterBoolOp opVal e1 e2

simpleExpr :: Parser FilterExpr
simpleExpr = parens (expr <* skipSpace) <|> exprSingle
  where
    exprSingle = do
      skipSpace
      c <- col <* skipSpace1
      o <- opr <* skipSpace1
      t <- term
      return $ FilterRelOp o c t

term :: Parser FilterTerm
term = termSingle <|> termList

termSingle :: Parser FilterTerm
termSingle = str <|> bool <|> nil <|> jsDate <|> num

termList :: Parser FilterTerm
termList = TermList <$> braces (sepByComma item)
  where
    braces = between (char '[') (char ']')
    item = skipSpace *> termSingle <* skipSpace

col :: Parser ColumnName
col = takeWhile1 (not . inClass excluded)
  where
    excluded = "():{}[] \t\r\n"

num :: Parser FilterTerm
num = do
  x <- scientific
  return (mkTerm x)
  where
    mkTerm = either TermFloat TermInt . floatingOrInteger

opr :: Parser FilterRelationalOperator
opr =
  choice $
  mkOp <$>
  [ ("eq", Equal)
  , ("~eq", NotEqual)
  , ("gt", GreaterThan)
  , ("~le", GreaterThan)
  , ("ge", GreaterThanOrEqual)
  , ("~lt", GreaterThanOrEqual)
  , ("lt", LessThan)
  , ("~ge", LessThan)
  , ("le", LessThanOrEqual)
  , ("~gt", LessThanOrEqual)
  , ("in", In)
  , ("~in", NotIn)
  , ("contains", Contains)
  , ("~contains", NotContains)
  ]

mkOp (s, v) = asciiCI s >> return v

bool :: Parser FilterTerm
bool = TermBool <$> (bool' "true" True <|> bool' "false" False)
  where
    bool' s v = const v <$> asciiCI s

nil :: Parser FilterTerm
nil = asciiCI "null" >> return TermNull

jsDate :: Parser FilterTerm
jsDate = TermDate <$> (takeWhile1 (inClass included) >>= getDate)
  where
    included = "0123456789:.+-TZD"
    getDate s =
      case parseISO8601 (T.unpack s) of
        Just d -> return d
        Nothing -> fail "expected an ISO8601 date"

str :: Parser FilterTerm
str = TermStr <$> (char '"' *> str')

str' :: Parser Text
str' = do
  s <- scan startState go <* anyChar
  case unescapeText s of
    Right r -> return r
    Left err -> fail (show err)
  where
    startState = False
    go a c
      | a = Just False
      | c == '"' = Nothing
      | otherwise = Just (c == '\\')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

sepByComma :: Parser a -> Parser [a]
sepByComma = sepBy (char ',')

between :: Applicative f => f a1 -> f b -> f a -> f a
between open close p = open *> p <* close

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep p = sepBy1 sep p <|> return []

sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep p = do
  x <- p
  xs <- many' (sep >> p)
  return (x : xs)

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

skipSpace1 :: Parser ()
skipSpace1 = const () <$> takeWhile1 isSpace