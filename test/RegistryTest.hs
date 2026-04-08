{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module RegistryTest (tests) where

import Data.ClassBox (Unconstrained)
import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Registry as Reg
import Prelude

class (Show a) => ShowConstr a

instance (Show a) => ShowConstr a

type TestReg = Reg.Registry Unconstrained
type ShowTestReg = Reg.Registry Show

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

prop_showEmpty :: Property
prop_showEmpty =
  property $ show (Reg.empty :: ShowTestReg) == "Registry []"

prop_showNonEmpty :: Int -> Property
prop_showNonEmpty x =
  let reg = Reg.insert x (Reg.empty :: ShowTestReg)
   in property $ not $ Prelude.null $ show reg

prop_showMultiple :: Int -> String -> Property
prop_showMultiple x s =
  let reg = Reg.insert x $ Reg.insert s $ (Reg.empty :: ShowTestReg)
   in property $ not $ Prelude.null $ show reg

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
    , testProperty "show empty" prop_showEmpty
    , testProperty "show non-empty" prop_showNonEmpty
    , testProperty "show multiple" prop_showMultiple
    ]
