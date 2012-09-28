module Main where

import qualified Network.Api.Support.Tests
import Test.Framework

main ::
  IO ()
main = 
  defaultMain tests 

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Network.Api.Support.Tests.test
      ]
  ]

