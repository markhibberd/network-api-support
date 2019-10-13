{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Support.Tests
  (
    main
  , test
  ) where

import Data.Monoid
import Network.Api.Support
import Network.HTTP.Client
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "Network.Api.Support"
      [
        testProperty "Identity" prop_identity
      , testProperty "setHeader" prop_setHeader
      ]

prop_identity ::
  Int
  -> Bool
prop_identity n =
  id n == n

prop_setHeader :: Bool
prop_setHeader =
  requestHeaders (appEndo (setHeader ("a", "new value")) r1) == requestHeaders r2
  where
    r1 = defaultRequest { requestHeaders = [("a", "x"), ("a", "y"), ("b", "z")] }
    r2 = defaultRequest { requestHeaders = [("b", "z"), ("a", "new value")] }
