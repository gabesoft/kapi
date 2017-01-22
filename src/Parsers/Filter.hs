{-# LANGUAGE OverloadedStrings #-}

-- | Filter parser
module Parsers.Filter where

import Control.Applicative ((<|>))
import Control.Monad (void, replicateM)
import Data.Attoparsec.Text
       (Parser, choice, skipWhile, char, asciiCI, many', satisfy,
        scientific, takeWhile1, inClass, option,
        notInClass, (<?>))
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace, chr, isHexDigit)
import Data.List (find)
import Data.Maybe
import Data.Scientific (floatingOrInteger, Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.ISO8601
import Numeric (readHex)
import Prelude ()
import Prelude.Compat
import Types.Common

data SpecialCharacter
  = BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

parse :: Text -> Either String FilterExpr
parse = A.parseOnly expr

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
termSingle = jsDate <|> bool <|> null' <|> str <|> num

termList :: Parser FilterTerm
termList = TermList <$> braces (sepByComma item) <?> "list"
  where
    braces = between '[' ']'
    item = skipSpace *> termSingle <* skipSpace

col :: Parser ColumnName
col = do
  name <- takeWhile1 (not . inClass excluded)
  b <- boost
  return $ ColumnName name b
  where
    excluded = "():{}[] \t\r\n"
    boost = option 1 $ char ':' *> value
    floatOrInt :: Scientific -> Either Double Integer
    floatOrInt = floatingOrInteger
    value = do
      x <- scientific
      either return (return . fromIntegral) (floatOrInt x)

num :: Parser FilterTerm
num = do
  x <- scientific
  return (mkTerm x)
  where
    mkTerm = either TermFloat TermInt . floatingOrInteger

mkOp :: (Text, a) -> Parser a
mkOp (s, v) = (asciiCI s >> return v) <?> T.unpack s

bool :: Parser FilterTerm
bool = TermBool <$> (bool' "true" True <|> bool' "false" False) <?> "bool"
  where
    bool' s v = const v <$> asciiCI s

null' :: Parser FilterTerm
null' = asciiCI "null" >> return TermNull <?> "null"

jsDate :: Parser FilterTerm
jsDate = TermDate <$> (takeWhile1 (inClass included) >>= getDate) <?> "iso8601 date"
  where
    included = "0-9:.TZD+-"
    getDate s =
      case parseISO8601 (T.unpack s) of
        Just d -> return d
        Nothing -> fail "expected an ISO8601 date"

str :: Parser FilterTerm
str = TermStr <$> (str' '"' <|> str' '\'') <?> "string"

str' :: Char -> Parser Text
str' delim = between delim delim (T.concat <$> many' strPart)
  where
    strPart = escapedHex <|> special <|> unspecial
    special = char '\\' *> choice (specialParser <$> (fst <$> specialChars))
    unspecial = A.takeWhile1 $ notInClass [delim, '\\']
    escapedHex = T.singleton <$> (specialParser '\\' *> hexu)
    specialToText = T.singleton . fromSpecialCharacter . fromJust . toSpecialCharacter
    specialParser c = specialToText <$> char c

hexu :: Parser Char
hexu = char 'u' *> hex <?> "hex"

hex :: Parser Char
hex = do
  cs <- replicateM 4 (satisfy isHexDigit)
  case readHex cs of
    ((h,_):_) -> return (chr h)
    [] -> fail "expected a hex digit"

fromSpecialCharacter :: SpecialCharacter -> Char
fromSpecialCharacter BackSpace = chr 0x08
fromSpecialCharacter FormFeed = chr 0x0C
fromSpecialCharacter NewLine = '\n'
fromSpecialCharacter CarriageReturn = '\r'
fromSpecialCharacter Tab = '\t'
fromSpecialCharacter VerticalTab = '\v'
fromSpecialCharacter SingleQuote = '\''
fromSpecialCharacter DoubleQuote = '"'
fromSpecialCharacter Backslash = '\\'

toSpecialCharacter :: Char -> Maybe SpecialCharacter
toSpecialCharacter c = snd <$> find ((==) c . fst) specialChars

specialChars :: [(Char, SpecialCharacter)]
specialChars =
  [ ('b', BackSpace)
  , ('f', FormFeed)
  , ('n', NewLine)
  , ('r', CarriageReturn)
  , ('t', Tab)
  , ('v', VerticalTab)
  , ('\'', SingleQuote)
  , ('"', DoubleQuote)
  , ('\\', Backslash)
  ]

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

parens :: Parser a -> Parser a
parens = between '(' ')'

sepByComma :: Parser a -> Parser [a]
sepByComma = sepBy (char ',')

between :: Char -> Char -> Parser a -> Parser a
between l r = between' (char l) (char r)

between'
  :: Applicative f
  => f a1 -> f b -> f a -> f a
between' open close p = open *> p <* close

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
skipSpace1 = void $ takeWhile1 isSpace
