{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.ElasticSearch
module Main
  ( main
  ) where

import Data.Bson ((=:))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Database.Bloodhound hiding (mkSearch)
import Parsers.Filter
import Persistence.ElasticSearch
import Test.Hspec
import Test.QuickCheck
import TestHelper
import Types.Common (Record(..), RecordData(..))

main :: IO ()
main =
  hspec $ do
    describe "Persistence.ElasticSearch" $ do
      describe "mkSearch" $ do (mapM_ runCase cases)
      describe "extractRecords" $ do
        it "full result" verifyExtractRecordsFullResult
        it "partial result" verifyExtractRecordsPartialResult

runCase :: (T.Text, Either EsError Search) -> SpecWith (Arg Expectation)
runCase c = it (T.unpack $ fst c) (verifySearch c)

verifySearch :: (T.Text, Either EsError Search) -> Expectation
verifySearch (filter, exp) =
  mkSearch (Just $ fromRight $ parse filter) [] [] 3 12 `shouldBe` exp

verifyExtractRecordsFullResult =
  extractRecords [] searchResults `shouldBe` extractResults1

verifyExtractRecordsPartialResult =
  extractRecords ["read", "userId", "feedId", "_id"] searchResults `shouldBe`
  extractResults2

cases :: [(T.Text, Either EsError Search)]
cases =
  [ ( "title eq 'haskell'"
    , Right
        Search
        { queryBody =
            Just
              (TermQuery
                 Term {termField = "title", termValue = "haskell"}
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
              (TermQuery Term {termField = "read", termValue = "true"} Nothing)
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
              (TermQuery Term {termField = "read", termValue = "false"} Nothing)
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
                         Term {termField = "title", termValue = "haskell"}
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
                     RangeDateGte
                       (GreaterThanEqD $ date "2017-01-23T01:21:15.513Z")
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
                         Term {termField = "title", termValue = "haskell"}
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
                                 {termField = "title", termValue = "haskell"}
                                 Nothing
                             , QueryMatchQuery
                                 MatchQuery
                                 { matchQueryField = FieldName "description"
                                 , matchQueryQueryString =
                                     QueryString "reader monad"
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
                                 , rangeQueryRange =
                                     RangeDoubleGt (GreaterThan 12.0)
                                 , rangeQueryBoost = Boost 1.0
                                 }
                             , QueryRangeQuery
                                 RangeQuery
                                 { rangeQueryField = FieldName "version"
                                 , rangeQueryRange =
                                     RangeDoubleLt (LessThan 15.0)
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

searchResults =
  SearchResult
  { took = 2
  , timedOut = False
  , shards =
      ShardResult {shardTotal = 5, shardsSuccessful = 5, shardsFailed = 0}
  , searchHits =
      SearchHits
      { hitsTotal = 38527
      , maxScore = Just 1.0
      , hits =
          [ Hit
            { hitIndex = IndexName "kapi-xandar"
            , hitType = MappingName "post"
            , hitDocId =
                DocId "57856cdeed521f1f17040f30-56e60f7d1432afe539337b7e"
            , hitScore = Just 1.0
            , hitSource =
                Just
                  (Record
                     [ "read" =: False
                     , mkTxtField "createdAt" "2016-03-14T01:10:21.985Z"
                     , "post" =:
                       [ mkTxtField "summary" "Post summary"
                       , mkTxtField
                           "link"
                           "http://feedproxy.google.com/~r/CreativeBloq/~3/grqVfmk5H-s/ridvan-maloku"
                       , mkTxtField "date" "2016-07-11T08:39:01.000Z"
                       , mkTxtField
                           "guid"
                           "http://www.creativebloq.com/authors/ridvan-maloku"
                       , mkTxtField "author" mempty
                       , "source" =:
                         [ mkTxtField "url" "http://www.creativebloq.com/"
                         , mkTxtField "title" "Creative Bloq"
                         ]
                       , mkTxtField "comments" mempty
                       , mkTxtField "title" "Ridvan Maloku"
                       , mkTxtField "description" "Post description"
                       ]
                     , mkIntField "__v" 1
                     , mkTxtField "userId" "56e60f591432afe539337b7b"
                     , mkTxtField "feedId" "56e49e771432afe539336afb"
                     , mkTxtField "postId" "57856cdeed521f1f17040f30"
                     , mkTxtField "updatedAt" "2016-03-14T01:11:31.558Z"
                     , mkTxtField "title" "Creative Bloq"
                     , mkTxtField "subscriptionId" "56e60f7d1432afe539337b7e"
                     , mkStrListField "tags" ["design"]
                     ])
            , hitHighlight = Nothing
            }
          , Hit
            { hitIndex = IndexName "kapi-xandar"
            , hitType = MappingName "post"
            , hitDocId =
                DocId "57856ce9ed521f1f17040f34-56e5c9cb1432afe53933789e"
            , hitScore = Just 1.0
            , hitSource =
                Just
                  (Record
                     [ "read" =: False
                     , mkTxtField "createdAt" "2016-03-13T20:12:59.849Z"
                     , "post" =:
                       [ mkTxtField "summary" "Another summary"
                       , mkTxtField
                           "link"
                           "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm"
                       , mkTxtField "date" "2016-07-12T15:12:12.000Z"
                       , mkTxtField
                           "guid"
                           "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm"
                       , mkTxtField "author" mempty
                       , mkTxtField
                           "comments"
                           "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm#/page/1"
                       , mkTxtField
                           "title"
                           "\192 l\8217Assembl\233e nationale, les \171 amendements citoyens \187 dans l\8217impasse"
                       , mkTxtField "description" "Another description"
                       ]
                     , mkIntField "__v" 1
                     , mkTxtField "userId" "56da18c8fa51ced05cd8db0a"
                     , mkTxtField "feedId" "56e5afe91432afe539336d64"
                     , mkTxtField "postId" "57856ce9ed521f1f17040f34"
                     , mkTxtField "updatedAt" "2016-03-13T20:13:00.295Z"
                     , mkTxtField "title" "Next INpact"
                     , mkTxtField "subscriptionId" "56e5c9cb1432afe53933789e"
                     , mkStrListField
                         "tags"
                         ["press", "tech", "informatique", "geek", "it"]
                     ])
            , hitHighlight = Nothing
            }
          ]
      }
  , aggregations = Nothing
  , scrollId = Nothing
  }

extractResults1 =
  [ Record
      [ "read" =: False
      , mkTxtField "createdAt" "2016-03-14T01:10:21.985Z"
      , "post" =:
        [ mkTxtField "summary" "Post summary"
        , mkTxtField
            "link"
            "http://feedproxy.google.com/~r/CreativeBloq/~3/grqVfmk5H-s/ridvan-maloku"
        , mkTxtField "date" "2016-07-11T08:39:01.000Z"
        , mkTxtField "guid" "http://www.creativebloq.com/authors/ridvan-maloku"
        , mkTxtField "author" mempty
        , "source" =:
          [ mkTxtField "url" "http://www.creativebloq.com/"
          , mkTxtField "title" "Creative Bloq"
          ]
        , mkTxtField "comments" mempty
        , mkTxtField "title" "Ridvan Maloku"
        , mkTxtField "description" "Post description"
        ]
      , mkIntField "__v" 1
      , mkTxtField "userId" "56e60f591432afe539337b7b"
      , mkTxtField "feedId" "56e49e771432afe539336afb"
      , mkTxtField "postId" "57856cdeed521f1f17040f30"
      , mkTxtField "updatedAt" "2016-03-14T01:11:31.558Z"
      , mkTxtField "title" "Creative Bloq"
      , mkTxtField "subscriptionId" "56e60f7d1432afe539337b7e"
      , mkStrListField "tags" ["design"]
      , mkRecId "57856cdeed521f1f17040f30-56e60f7d1432afe539337b7e"
      ]
  , Record
      [ "read" =: False
      , mkTxtField "createdAt" "2016-03-13T20:12:59.849Z"
      , "post" =:
        [ mkTxtField "summary" "Another summary"
        , mkTxtField
            "link"
            "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm"
        , mkTxtField "date" "2016-07-12T15:12:12.000Z"
        , mkTxtField
            "guid"
            "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm"
        , mkTxtField "author" mempty
        , mkTxtField
            "comments"
            "https://www.nextinpact.com/news/100622-a-l-assemblee-nationale-amendements-citoyens-dans-l-impasse.htm#/page/1"
        , mkTxtField
            "title"
            "\192 l\8217Assembl\233e nationale, les \171 amendements citoyens \187 dans l\8217impasse"
        , mkTxtField "description" "Another description"
        ]
      , mkIntField "__v" 1
      , mkTxtField "userId" "56da18c8fa51ced05cd8db0a"
      , mkTxtField "feedId" "56e5afe91432afe539336d64"
      , mkTxtField "postId" "57856ce9ed521f1f17040f34"
      , mkTxtField "updatedAt" "2016-03-13T20:13:00.295Z"
      , mkTxtField "title" "Next INpact"
      , mkTxtField "subscriptionId" "56e5c9cb1432afe53933789e"
      , mkStrListField "tags" ["press", "tech", "informatique", "geek", "it"]
      , mkRecId "57856ce9ed521f1f17040f34-56e5c9cb1432afe53933789e"
      ]
  ]

extractResults2 =
  [ Record
      [ mkRecId "57856cdeed521f1f17040f30-56e60f7d1432afe539337b7e"
      , mkTxtField "feedId" "56e49e771432afe539336afb"
      , mkBoolField "read" False
      , mkTxtField "userId" "56e60f591432afe539337b7b"
      ]
  , Record
      [ mkRecId "57856ce9ed521f1f17040f34-56e5c9cb1432afe53933789e"
      , mkTxtField "feedId" "56e5afe91432afe539336d64"
      , mkBoolField "read" False
      , mkTxtField "userId" "56da18c8fa51ced05cd8db0a"
      ]
  ]
