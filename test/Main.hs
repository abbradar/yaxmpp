module Main (main) where

import Test.Tasty

import qualified GenericRegistryTest
import qualified RegistryTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Data"
      [ GenericRegistryTest.tests
      , RegistryTest.tests
      ]
