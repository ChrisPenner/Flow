{-# LANGUAGE DeriveFunctor #-}

module Arrows where

import Control.Arrow as Arr
import Control.Category as C
import Control.Concurrent.STM
import Control.Monad.State
import Data.Profunctor

data EventA a b =
  EventA (StateT [STM ()] STM (a -> STM (), STM (STM b)))
  deriving (Functor)

instance Profunctor EventA where
  lmap f (EventA m) = EventA $ (fmap (first (lmap f))) m
  rmap f (EventA m) = EventA $ fmap (fmap (fmap (fmap f))) m

instance Category EventA where
  id =
    EventA $ do
      tChan <- lift newBroadcastTChan
      return (writeTChan tChan, (dupTChan tChan >>= (return <<< readTChan)))
  EventA a . EventA b =
    EventA $ do
      (writeA, readA) <- a
      (writeB, readB) <- b
      getterB <- lift readB
      modify <<< (:) $ do
        b' <- getterB
        writeA b'
      return (writeB, readA)

instance Arrow EventA where
  arr f = fmap f C.id
  first (EventA m) =
    EventA $ do
      holdChan <- lift newBroadcastTChan
      (writer, reader) <- m
      return (transWrite holdChan writer, transRead holdChan reader)
    where
      transRead :: TChan b -> STM (STM a) -> STM (STM (a, b))
      transRead holdChan reader = do
        getter <- reader
        return $ do
          extra <- readTChan holdChan
          a <- getter
          return (a, extra)
      transWrite :: TChan o -> (a -> STM ()) -> (a, o) -> STM ()
      transWrite holdChan writer (a, extra) = do
        writer a
        writeTChan holdChan extra

scanE :: (b -> a -> b) -> b -> EventA a b
scanE f initial =
  EventA $ do
    inChan <- lift $ newBroadcastTChan
    outChan <- lift $ newBroadcastTChan
    lastVar <- lift $ newTVar initial
    modify ((:) (scan' inChan outChan lastVar))
    return (writeTChan inChan, return (dupTChan outChan >>= readTChan))
  where
    scan' inChan outChan lastVar = do
      lastVal <- readTVar lastVar
      a <- readTChan inChan
      let nextVal = f lastVal a
      writeTChan outChan nextVal
      writeTVar lastVar nextVal
