module AsyncMemoTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.IORef
import UnliftIO.MVar

import qualified Control.AsyncMemo as AsyncMemo

tests :: TestTree
tests =
  testGroup
    "Control.AsyncMemo"
    [ testCase "synchronous action: handler fires with computed value" $ do
        runs <- newIORef (0 :: Int)
        m <- AsyncMemo.new $ \k -> do
          modifyIORef' runs (+ 1)
          k (42 :: Int)
        result <- newIORef Nothing
        AsyncMemo.get m (writeIORef result . Just)
        readIORef result >>= (@?= Just 42)
        readIORef runs >>= (@?= 1)
    , testCase "subsequent gets reuse cached result without re-running" $ do
        runs <- newIORef (0 :: Int)
        m <- AsyncMemo.new $ \k -> do
          modifyIORef' runs (+ 1)
          k (7 :: Int)
        results <- newIORef ([] :: [Int])
        let collect v = modifyIORef' results (v :)
        AsyncMemo.get m collect
        AsyncMemo.get m collect
        AsyncMemo.get m collect
        readIORef results >>= (@?= [7, 7, 7])
        readIORef runs >>= (@?= 1)
    , testCase "deferred action: gets queue and all fire on completion" $ do
        runs <- newIORef (0 :: Int)
        resolverSlot <- newEmptyMVar
        m <- AsyncMemo.new $ \k -> do
          modifyIORef' runs (+ 1)
          putMVar resolverSlot k
        results <- newIORef ([] :: [(Char, Int)])
        let collect tag v = modifyIORef' results ((tag, v) :)
        AsyncMemo.get m (collect 'a')
        AsyncMemo.get m (collect 'b')
        AsyncMemo.get m (collect 'c')
        readIORef results >>= (@?= [])
        k <- takeMVar resolverSlot
        k 99
        readIORef runs >>= (@?= 1)
        rs <- readIORef results
        -- All three handlers should have fired with 99 (order is implementation-defined,
        -- but we assert each tag appears exactly once with value 99).
        length rs @?= 3
        mapM_ (\tag -> filter ((== tag) . fst) rs @?= [(tag, 99)]) ("abc" :: String)
    , testCase "get after completion fires handler immediately" $ do
        m <- AsyncMemo.new $ \k -> k (1 :: Int)
        result1 <- newIORef Nothing
        AsyncMemo.get m (writeIORef result1 . Just)
        readIORef result1 >>= (@?= Just 1)
        result2 <- newIORef Nothing
        AsyncMemo.get m (writeIORef result2 . Just)
        readIORef result2 >>= (@?= Just 1)
    ]
