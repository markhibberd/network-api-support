module Network.Api.Support.Tests
  (
    main
  , test
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Network.Api.Support

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
      ]

prop_identity ::
  Int
  -> Bool
prop_identity n =
  bletch n == n

