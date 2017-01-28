{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.ElasticSearch
module Main
  ( main
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Database.Bloodhound hiding (mkSearch)
import Parsers.Filter
import Persistence.ElasticSearch
import Test.Hspec
import Test.QuickCheck
import TestHelper

main :: IO ()
main = hspec $ describe "Persistence.ElasticSearch" (mapM_ runCase cases)

runCase :: (T.Text, Either EsError Search) -> SpecWith (Arg Expectation)
runCase c = it (T.unpack $ fst c) (verifySearch c)

verifySearch :: (T.Text, Either EsError Search) -> Expectation
verifySearch (filter, exp) =
  mkSearch (Just $ fromRight $ parse filter) [] [] 3 12 `shouldBe` exp

fromRight :: Either a b -> b
fromRight (Right x) = x

cases :: [(T.Text, Either EsError Search)]
cases =
  [ ( "title eq 'haskell'"
    , Right
        Search
        { queryBody =
          Just
            (TermQuery
               Term
               { termField = "title"
               , termValue = "haskell"
               }
               Nothing)
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "read eq true"
    , Right
        Search
        { queryBody =
          Just
            (TermQuery
               Term
               { termField = "read"
               , termValue = "true"
               }
               Nothing)
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "read eq false"
    , Right
        Search
        { queryBody =
          Just
            (TermQuery
               Term
               { termField = "read"
               , termValue = "false"
               }
               Nothing)
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "name in 123"
    , Left $ EsError 400 "Unexpected TermInt 123. Expected a list.")
  , ( "title contains 'bamboo'"
    , Right
        Search
        { queryBody =
          Just
            (QueryMatchQuery
               MatchQuery
               { matchQueryField = FieldName "title"
               , matchQueryQueryString = QueryString "bamboo"
               , matchQueryOperator = Or
               , matchQueryZeroTerms = ZeroTermsNone
               , matchQueryCutoffFrequency = Nothing
               , matchQueryMatchType = Nothing
               , matchQueryAnalyzer = Nothing
               , matchQueryMaxExpansions = Nothing
               , matchQueryLenient = Nothing
               , matchQueryBoost = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "title contains 'word1 word2'"
    , Right
        Search
        { queryBody =
          Just
            (QueryMatchQuery
               MatchQuery
               { matchQueryField = FieldName "title"
               , matchQueryQueryString = QueryString "word1 word2"
               , matchQueryOperator = Or
               , matchQueryZeroTerms = ZeroTermsNone
               , matchQueryCutoffFrequency = Nothing
               , matchQueryMatchType = Just MatchPhrase
               , matchQueryAnalyzer = Nothing
               , matchQueryMaxExpansions = Nothing
               , matchQueryLenient = Nothing
               , matchQueryBoost = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "title ~eq 'haskell'"
    , Right
        Search
        { queryBody =
          Just
            (QueryBoolQuery
               BoolQuery
               { boolQueryMustMatch = []
               , boolQueryMustNotMatch =
                 [ TermQuery
                     Term
                     { termField = "title"
                     , termValue = "haskell"
                     }
                     Nothing
                 ]
               , boolQueryShouldMatch = []
               , boolQueryMinimumShouldMatch = Nothing
               , boolQueryBoost = Nothing
               , boolQueryDisableCoord = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "version lt 12"
    , Right
        Search
        { queryBody =
          Just
            (QueryRangeQuery
               RangeQuery
               { rangeQueryField = FieldName "version"
               , rangeQueryRange = RangeDoubleLt (LessThan 12.0)
               , rangeQueryBoost = Boost 1.0
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "date ge 2017-01-23T01:21:15.513Z"
    , Right
        Search
        { queryBody =
          Just
            (QueryRangeQuery
               RangeQuery
               { rangeQueryField = FieldName "date"
               , rangeQueryRange =
                 RangeDateGte (GreaterThanEqD $ date "2017-01-23T01:21:15.513Z")
               , rangeQueryBoost = Boost 1.0
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "version le 12"
    , Right
        Search
        { queryBody =
          Just
            (QueryRangeQuery
               RangeQuery
               { rangeQueryField = FieldName "version"
               , rangeQueryRange = RangeDoubleLte (LessThanEq 12.0)
               , rangeQueryBoost = Boost 1.0
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "version gt 12"
    , Right
        Search
        { queryBody =
          Just
            (QueryRangeQuery
               RangeQuery
               { rangeQueryField = FieldName "version"
               , rangeQueryRange = RangeDoubleGt (GreaterThan 12.0)
               , rangeQueryBoost = Boost 1.0
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "name in ['bob', 'tom', 'alice']"
    , Right
        Search
        { queryBody = Just (TermsQuery "name" ("bob" :| ["tom", "alice"]))
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "name in []"
    , Right
        Search
        { queryBody = Just (IdsQuery (MappingName "") [])
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "title ~contains 'haskell'"
    , Right
        Search
        { queryBody =
          Just
            (QueryBoolQuery
               BoolQuery
               { boolQueryMustMatch = []
               , boolQueryMustNotMatch =
                 [ QueryMatchQuery
                     MatchQuery
                     { matchQueryField = FieldName "title"
                     , matchQueryQueryString = QueryString "haskell"
                     , matchQueryOperator = Or
                     , matchQueryZeroTerms = ZeroTermsNone
                     , matchQueryCutoffFrequency = Nothing
                     , matchQueryMatchType = Nothing
                     , matchQueryAnalyzer = Nothing
                     , matchQueryMaxExpansions = Nothing
                     , matchQueryLenient = Nothing
                     , matchQueryBoost = Nothing
                     }
                 ]
               , boolQueryShouldMatch = []
               , boolQueryMinimumShouldMatch = Nothing
               , boolQueryBoost = Nothing
               , boolQueryDisableCoord = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "(title eq 'haskell') and (description contains 'reader monad')"
    , Right
        Search
        { queryBody =
          Just
            (QueryBoolQuery
               BoolQuery
               { boolQueryMustMatch =
                 [ TermQuery
                     Term
                     { termField = "title"
                     , termValue = "haskell"
                     }
                     Nothing
                 , QueryMatchQuery
                     MatchQuery
                     { matchQueryField = FieldName "description"
                     , matchQueryQueryString = QueryString "reader monad"
                     , matchQueryOperator = Or
                     , matchQueryZeroTerms = ZeroTermsNone
                     , matchQueryCutoffFrequency = Nothing
                     , matchQueryMatchType = Just MatchPhrase
                     , matchQueryAnalyzer = Nothing
                     , matchQueryMaxExpansions = Nothing
                     , matchQueryLenient = Nothing
                     , matchQueryBoost = Nothing
                     }
                 ]
               , boolQueryMustNotMatch = []
               , boolQueryShouldMatch = []
               , boolQueryMinimumShouldMatch = Nothing
               , boolQueryBoost = Nothing
               , boolQueryDisableCoord = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  , ( "(title eq 'haskell' and description contains 'reader monad') or (version gt 12 and version lt 15)"
    , Right
        Search
        { queryBody =
          Just
            (QueryBoolQuery
               BoolQuery
               { boolQueryMustMatch = []
               , boolQueryMustNotMatch = []
               , boolQueryShouldMatch =
                 [ QueryBoolQuery
                     BoolQuery
                     { boolQueryMustMatch =
                       [ TermQuery
                           Term
                           { termField = "title"
                           , termValue = "haskell"
                           }
                           Nothing
                       , QueryMatchQuery
                           MatchQuery
                           { matchQueryField = FieldName "description"
                           , matchQueryQueryString = QueryString "reader monad"
                           , matchQueryOperator = Or
                           , matchQueryZeroTerms = ZeroTermsNone
                           , matchQueryCutoffFrequency = Nothing
                           , matchQueryMatchType = Just MatchPhrase
                           , matchQueryAnalyzer = Nothing
                           , matchQueryMaxExpansions = Nothing
                           , matchQueryLenient = Nothing
                           , matchQueryBoost = Nothing
                           }
                       ]
                     , boolQueryMustNotMatch = []
                     , boolQueryShouldMatch = []
                     , boolQueryMinimumShouldMatch = Nothing
                     , boolQueryBoost = Nothing
                     , boolQueryDisableCoord = Nothing
                     }
                 , QueryBoolQuery
                     BoolQuery
                     { boolQueryMustMatch =
                       [ QueryRangeQuery
                           RangeQuery
                           { rangeQueryField = FieldName "version"
                           , rangeQueryRange = RangeDoubleGt (GreaterThan 12.0)
                           , rangeQueryBoost = Boost 1.0
                           }
                       , QueryRangeQuery
                           RangeQuery
                           { rangeQueryField = FieldName "version"
                           , rangeQueryRange = RangeDoubleLt (LessThan 15.0)
                           , rangeQueryBoost = Boost 1.0
                           }
                       ]
                     , boolQueryMustNotMatch = []
                     , boolQueryShouldMatch = []
                     , boolQueryMinimumShouldMatch = Nothing
                     , boolQueryBoost = Nothing
                     , boolQueryDisableCoord = Nothing
                     }
                 ]
               , boolQueryMinimumShouldMatch = Nothing
               , boolQueryBoost = Nothing
               , boolQueryDisableCoord = Nothing
               })
        , filterBody = Nothing
        , sortBody = Nothing
        , aggBody = Nothing
        , highlight = Nothing
        , trackSortScores = False
        , from = From 3
        , size = Size 12
        , searchType = SearchTypeDfsQueryThenFetch
        , fields = Nothing
        , source = Nothing
        })
  ]
