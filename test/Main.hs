module Main (main) where

import Test.Tasty

import qualified GenericRegistryTest
import qualified LazyOnceTest
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
      , LazyOnceTest.tests
      ]
