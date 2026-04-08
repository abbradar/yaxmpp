module RegistryTest (tests) where

import Data.ClassBox (Unconstrained)
import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Registry as Reg
import Prelude hiding (lookup, null)

type TestReg = Reg.Registry Unconstrained

prop_lookupAfterInsert :: Int -> String -> Bool -> Property
prop_lookupAfterInsert x s b =
  let reg = Reg.insert x $ Reg.insert s $ Reg.insert b $ (Reg.empty :: TestReg)
   in Reg.lookup (Proxy :: Proxy Int) reg
        === Just x
        .&&. Reg.lookup (Proxy :: Proxy String) reg
        === Just s
        .&&. Reg.lookup (Proxy :: Proxy Bool) reg
        === Just b

prop_lookupMissing :: Int -> Property
prop_lookupMissing x =
  let reg = Reg.insert x (Reg.empty :: TestReg)
   in Reg.lookup (Proxy :: Proxy String) reg === Nothing

prop_insertOverwrites :: Int -> Int -> Property
prop_insertOverwrites x y =
  let reg = Reg.insert y $ Reg.insert x $ (Reg.empty :: TestReg)
   in Reg.lookup (Proxy :: Proxy Int) reg === Just y

prop_deleteRemoves :: Int -> String -> Property
prop_deleteRemoves x s =
  let reg = Reg.delete (Proxy :: Proxy Int) $ Reg.insert x $ Reg.insert s $ (Reg.empty :: TestReg)
   in Reg.lookup (Proxy :: Proxy Int) reg
        === Nothing
        .&&. Reg.lookup (Proxy :: Proxy String) reg
        === Just s

prop_tryInsertNewSucceeds :: Int -> Property
prop_tryInsertNewSucceeds x =
  case Reg.tryInsertNew x (Reg.empty :: TestReg) of
    Just reg -> Reg.lookup (Proxy :: Proxy Int) reg === Just x
    Nothing -> property False

prop_tryInsertNewFails :: Int -> Int -> Property
prop_tryInsertNewFails x y =
  let reg = Reg.insert x (Reg.empty :: TestReg)
   in property $ case Reg.tryInsertNew y reg of
        Nothing -> True
        Just _ -> False

prop_nullEmpty :: Property
prop_nullEmpty = property $ Reg.null (Reg.empty :: TestReg)

prop_nullNonEmpty :: Int -> Property
prop_nullNonEmpty x =
  property $ not $ Reg.null $ Reg.insert x (Reg.empty :: TestReg)

tests :: TestTree
tests =
  testGroup
    "Registry"
    [ testProperty "lookup after insert" prop_lookupAfterInsert
    , testProperty "lookup missing key" prop_lookupMissing
    , testProperty "insert overwrites" prop_insertOverwrites
    , testProperty "delete removes" prop_deleteRemoves
    , testProperty "tryInsertNew succeeds" prop_tryInsertNewSucceeds
    , testProperty "tryInsertNew fails on existing" prop_tryInsertNewFails
    , testProperty "null on empty" prop_nullEmpty
    , testProperty "null on non-empty" prop_nullNonEmpty
    ]
