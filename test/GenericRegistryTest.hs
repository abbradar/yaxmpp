{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GenericRegistryTest (tests) where

import Data.Functor.Identity
import Data.Proxy
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.GenericRegistry as GR
import Prelude

type TestGR = GR.GenericRegistry Identity Typeable
type ShowTestGR = GR.GenericRegistry Identity Show

prop_lookupAfterInsert :: Int -> String -> Bool -> Property
prop_lookupAfterInsert x s b =
  let reg =
        GR.insert (Proxy :: Proxy Int) (Identity x) $
          GR.insert (Proxy :: Proxy String) (Identity s) $
            GR.insert (Proxy :: Proxy Bool) (Identity b) $
              GR.empty ::
          TestGR
   in GR.lookup (Proxy :: Proxy Int) reg
        === Just (Identity x)
        .&&. GR.lookup (Proxy :: Proxy String) reg
        === Just (Identity s)
        .&&. GR.lookup (Proxy :: Proxy Bool) reg
        === Just (Identity b)

prop_lookupMissing :: Int -> Property
prop_lookupMissing x =
  let reg = GR.insert (Proxy :: Proxy Int) (Identity x) (GR.empty :: TestGR)
   in GR.lookup (Proxy :: Proxy String) reg === Nothing

prop_insertOverwrites :: Int -> Int -> Property
prop_insertOverwrites x y =
  let reg =
        GR.insert (Proxy :: Proxy Int) (Identity y) $
          GR.insert (Proxy :: Proxy Int) (Identity x) $
            (GR.empty :: TestGR)
   in GR.lookup (Proxy :: Proxy Int) reg === Just (Identity y)

prop_deleteRemoves :: Int -> String -> Property
prop_deleteRemoves x s =
  let reg =
        GR.delete (Proxy :: Proxy Int) $
          GR.insert (Proxy :: Proxy Int) (Identity x) $
            GR.insert (Proxy :: Proxy String) (Identity s) $
              (GR.empty :: TestGR)
   in GR.lookup (Proxy :: Proxy Int) reg
        === Nothing
        .&&. GR.lookup (Proxy :: Proxy String) reg
        === Just (Identity s)

prop_tryInsertNewSucceeds :: Int -> Property
prop_tryInsertNewSucceeds x =
  let result = GR.tryInsertNew (Proxy :: Proxy Int) (Identity x) (GR.empty :: TestGR)
   in case result of
        Just reg -> GR.lookup (Proxy :: Proxy Int) reg === Just (Identity x)
        Nothing -> property False

prop_tryInsertNewFails :: Int -> Int -> Property
prop_tryInsertNewFails x y =
  let reg = GR.insert (Proxy :: Proxy Int) (Identity x) (GR.empty :: TestGR)
   in property $ case GR.tryInsertNew (Proxy :: Proxy Int) (Identity y) reg of
        Nothing -> True
        Just _ -> False

prop_nullEmpty :: Property
prop_nullEmpty = property $ GR.null (GR.empty :: TestGR)

prop_nullNonEmpty :: Int -> Property
prop_nullNonEmpty x =
  property $ not $ GR.null $ GR.insert (Proxy :: Proxy Int) (Identity x) (GR.empty :: TestGR)

prop_showEmpty :: Property
prop_showEmpty =
  property $ show (GR.empty :: ShowTestGR) == "GenericRegistry []"

prop_showNonEmpty :: Int -> Property
prop_showNonEmpty x =
  let reg = GR.insert (Proxy :: Proxy Int) (Identity x) (GR.empty :: ShowTestGR)
   in property $ not $ Prelude.null $ show reg

prop_showMultiple :: Int -> String -> Property
prop_showMultiple x s =
  let reg =
        GR.insert (Proxy :: Proxy Int) (Identity x) $
          GR.insert (Proxy :: Proxy String) (Identity s) $
            (GR.empty :: ShowTestGR)
   in property $ not $ Prelude.null $ show reg

tests :: TestTree
tests =
  testGroup
    "GenericRegistry"
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
