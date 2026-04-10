module LazyOnceTest (tests) where

import qualified Control.LazyOnce as LazyOnce
import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (SomeException)
import UnliftIO.Async
import UnliftIO.Exception (try)
import UnliftIO.IORef
import UnliftIO.MVar

tests :: TestTree
tests =
  testGroup
    "Control.LazyOnce"
    [ testCase "computes value on first get" $ do
        ref <- newIORef (0 :: Int)
        lazy <- LazyOnce.new (modifyIORef' ref (+ 1) >> return (42 :: Int))
        val <- LazyOnce.get lazy
        val @?= 42
        count <- readIORef ref
        count @?= 1
    , testCase "returns cached value on subsequent gets" $ do
        ref <- newIORef (0 :: Int)
        lazy <- LazyOnce.new (modifyIORef' ref (+ 1) >> return (42 :: Int))
        v1 <- LazyOnce.get lazy
        v2 <- LazyOnce.get lazy
        v3 <- LazyOnce.get lazy
        v1 @?= 42
        v2 @?= 42
        v3 @?= 42
        count <- readIORef ref
        count @?= 1
    , testCase "computes only once under concurrent access" $ do
        ref <- newIORef (0 :: Int)
        barrier <- newEmptyMVar
        lazy <- LazyOnce.new $ do
          modifyIORef' ref (+ 1)
          readMVar barrier
        -- Start several concurrent getters
        asyncs <- mapM (\_ -> async $ LazyOnce.get lazy) [1 :: Int .. 10]
        -- Let the computation complete
        putMVar barrier (99 :: Int)
        results <- mapM wait asyncs
        -- All should get the same value
        mapM_ (@?= 99) results
        count <- readIORef ref
        count @?= 1
    , testCase "restores action on exception" $ do
        ref <- newIORef (0 :: Int)
        lazy <- LazyOnce.new $ do
          n <- readIORef ref
          modifyIORef' ref (+ 1)
          if n == 0
            then error "first attempt fails"
            else return (n :: Int)
        -- First get should fail
        result1 <- try $ LazyOnce.get lazy
        case result1 of
          Left (_ :: SomeException) -> return ()
          Right _ -> assertFailure "expected exception"
        -- Second get should succeed with recomputation
        val <- LazyOnce.get lazy
        val @?= 1
        count <- readIORef ref
        count @?= 2
    ]
