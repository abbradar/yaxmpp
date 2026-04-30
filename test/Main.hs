module Main (main) where

import Test.Tasty

import qualified AsyncFutureTest
import qualified AsyncMemoTest
import qualified CapsTest
import qualified FormTest
import qualified GenericRegistryTest
import qualified RegistryTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "All"
      [ testGroup
          "Data"
          [ GenericRegistryTest.tests
          , RegistryTest.tests
          ]
      , testGroup
          "Control"
          [ AsyncMemoTest.tests
          , AsyncFutureTest.tests
          ]
      , FormTest.tests
      , CapsTest.tests
      ]
