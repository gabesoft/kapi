{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Parsers.Filter
module Main (main) where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Parsers.Filter (parse)
import Test.Hspec
import TestHelper
import Types.Common

main :: IO ()
main = hspec $ describe "Parsers.Filter" $ mapM_ (runCase Right) validCases

runCase ::
     (t -> Either String FilterExpr) -> (Text, t) -> SpecWith (Arg Expectation)
runCase ctor (input, expected) = it (unpack input) $ parse input `shouldBe` ctor expected

validCases :: [(Text, FilterExpr)]
validCases =
  [ ("(colStr2 eq \"June\")", FilterRelOp Equal "colStr2" (TermStr "June"))
  , ("(colStr2 eq \"The \\\"great\\\" good\")", FilterRelOp Equal "colStr2" (TermStr "The \"great\" good"))
  , ("(colStr2 eq 'The great good')", FilterRelOp Equal "colStr2" (TermStr "The great good"))
  , ("(colStr2 eq 'The \"great\" good')", FilterRelOp Equal "colStr2" (TermStr "The \"great\" good"))
  , ("(colStr2 eq '')", FilterRelOp Equal "colStr2" (TermStr ""))
  , ("(colStr2 eq 'The \\'great\\' good')", FilterRelOp Equal "colStr2" (TermStr "The \'great\' good"))
  , ("(colStr2 eq 'The \\'great\\' good')", FilterRelOp Equal "colStr2" (TermStr "The \'great\' good"))
  , ("(colStr2 eq \"a \\u00faabc\\\"def\")", FilterRelOp Equal "colStr2" (TermStr "a \250abc\"def"))
  , ( "(col1 ~eq \"Blue and CO.\")" , FilterRelOp NotEqual "col1" (TermStr "Blue and CO."))
  , ( "(col1 ~eq 'a \\u0010 \\u0a1f e')" , FilterRelOp NotEqual "col1" (TermStr "a \DLE \2591 e"))
  , ("colInt lt 123", FilterRelOp LessThan "colInt" (TermInt 123))
  , ("colInt:0.3 lt 123", FilterRelOp LessThan (ColumnName "colInt" 0.3) (TermInt 123))
  , ("colFloat lt 123.93", FilterRelOp LessThan "colFloat" (TermFloat 123.93))
  , ("colBool eq true", FilterRelOp Equal "colBool" (TermBool True))
  , ("colId eq 56d7de0dc788cb1d6eb91abb", FilterRelOp Equal "colId" (TermId "56d7de0dc788cb1d6eb91abb"))
  , ("colId eq 56d7de0dc788cb1d6eb91ac6-56d7de0dc788cb1d6eb91abc", FilterRelOp Equal "colId" (TermId "56d7de0dc788cb1d6eb91ac6-56d7de0dc788cb1d6eb91abc"))
  , ("colNull eq null", FilterRelOp Equal "colNull" TermNull)
  , ("  (colNull eq null)", FilterRelOp Equal "colNull" TermNull)
  , ("colNotNull ~eq null", FilterRelOp NotEqual "colNotNull" TermNull)
  , ( "colDate ge 2017-01-08T02:26:16.302Z"
    , FilterRelOp GreaterThanOrEqual "colDate" (dateTerm "2017-01-08T02:26:16.302Z"))
  , ( "colDate ~le 1994-11-05T08:15:30-05:00"
    , FilterRelOp GreaterThan "colDate" (dateTerm "1994-11-05T08:15:30-05:00"))
  , ( "colBoolList in [true, false]"
    , FilterRelOp In "colBoolList" (TermList [TermBool True, TermBool False]))
  , ( "colDateList in [2017-01-08T02:26:16.302Z, 1994-11-05T08:15:30-05:00,2011-12-19T15:28:46.493Z]"
    , FilterRelOp
        In
        "colDateList"
        (TermList
           [ dateTerm "2017-01-08T02:26:16.302Z"
           , dateTerm "1994-11-05T08:15:30-05:00"
           , dateTerm "2011-12-19T15:28:46.493Z"
           ]))
  , ( "colBoolList ~in [true, 123, 99.33, null, \"x\"]"
    , FilterRelOp
        NotIn
        "colBoolList"
        (TermList
           [TermBool True, TermInt 123, TermFloat 99.33, TermNull, TermStr "x"]))
  , ( "title:1 eq \"hoop\" and (desc:2 ~in [\"hoola\",\"boop\"])"
    , FilterBoolOp
        And
        (FilterRelOp Equal "title" $ TermStr "hoop")
        (FilterRelOp NotIn (ColumnName "desc" 2) $
         TermList [TermStr "hoola", TermStr "boop"]))
  , mkAnd (expr 1) (expr 2)
  , mkAnd (expr 1) (parens $ mkOr (expr 2) (expr 3))
  , mkAnd (expr 1) (parens $ mkOr (expr 2) (parens $ mkAnd (expr 4) (expr 5)))
  , mkAnd (mkAnd (expr 2) (expr 3)) (expr 1)
  , mkOr (mkAnd (expr 2) (expr 3)) (expr 1)
  , ( "(a eq 1) and (b in [2,true]) or (c lt 10)"
    , FilterBoolOp
        Or
        (FilterBoolOp
           And
           (FilterRelOp Equal "a" (TermInt 1))
           (FilterRelOp In "b" (TermList [TermInt 2, TermBool True])))
        (FilterRelOp LessThan "c" (TermInt 10)))
  ]

expr :: Int -> (Text, FilterExpr)
expr n = validCases !! n

mkAnd :: (Text, FilterExpr) -> (Text, FilterExpr) -> (Text, FilterExpr)
mkAnd = mkOp ("and", And)

mkOr :: (Text, FilterExpr) -> (Text, FilterExpr) -> (Text, FilterExpr)
mkOr = mkOp ("or", Or)

parens :: (Text, b) -> (Text, b)
parens (e1, r1) = (T.concat ["(", e1, ")"], r1) 

mkOp
  :: (Text, FilterBooleanOperator)
  -> (Text, FilterExpr)
  -> (Text, FilterExpr)
  -> (Text, FilterExpr)
mkOp (opName, opCtor) (e1, r1) (e2, r2) =
  (T.concat [e1, " ", opName, " ", e2], FilterBoolOp opCtor r1 r2)
