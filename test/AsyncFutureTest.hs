module AsyncFutureTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.IORef

import qualified Control.AsyncFuture as AsyncFuture

tests :: TestTree
tests =
  testGroup
    "Control.AsyncFuture"
    [ testCase "get before resolve: handler fires on resolve" $ do
        r <- AsyncFuture.new
        result <- newIORef Nothing
        AsyncFuture.get (AsyncFuture.future r) (writeIORef result . Just)
        readIORef result >>= (@?= Nothing)
        AsyncFuture.resolve r (42 :: Int)
        readIORef result >>= (@?= Just 42)
    , testCase "get after resolve: handler fires immediately" $ do
        r <- AsyncFuture.new
        AsyncFuture.resolve r (7 :: Int)
        result <- newIORef Nothing
        AsyncFuture.get (AsyncFuture.future r) (writeIORef result . Just)
        readIORef result >>= (@?= Just 7)
    , testCase "multiple queued handlers fire in registration order on resolve" $ do
        r <- AsyncFuture.new
        order <- newIORef ([] :: [(Int, Int)])
        let push tag v = modifyIORef' order ((tag, v) :)
        AsyncFuture.get (AsyncFuture.future r) (push 1)
        AsyncFuture.get (AsyncFuture.future r) (push 2)
        AsyncFuture.get (AsyncFuture.future r) (push 3)
        AsyncFuture.resolve r (100 :: Int)
        rs <- readIORef order
        -- handlers fire in registration order; we accumulated by prepending,
        -- so the recorded list is reversed.
        reverse rs @?= [(1, 100), (2, 100), (3, 100)]
    , testCase "second resolve is a no-op (first wins)" $ do
        r <- AsyncFuture.new
        AsyncFuture.resolve r (1 :: Int)
        AsyncFuture.resolve r 999
        result <- newIORef Nothing
        AsyncFuture.get (AsyncFuture.future r) (writeIORef result . Just)
        readIORef result >>= (@?= Just 1)
    , testCase "handler queued before resolve is not invoked twice on second resolve" $ do
        r <- AsyncFuture.new
        calls <- newIORef (0 :: Int)
        AsyncFuture.get (AsyncFuture.future r) (\(_ :: Int) -> modifyIORef' calls (+ 1))
        AsyncFuture.resolve r 1
        AsyncFuture.resolve r 2
        readIORef calls >>= (@?= 1)
    ]
