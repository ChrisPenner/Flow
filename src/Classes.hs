{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
module Classes where

import Control.Monad.Cont
import Data.Time
import Control.Concurrent

type EventGenerator m a = forall b. ((a -> m b) -> m b)
type Event m a = ContT () m a
type Behaviour m a = m a

eventStream :: EventGenerator m a -> Event m a
eventStream eventSpawner = ContT eventSpawner

inputLines :: MonadIO m => Event m String
inputLines = eventStream $ \sendEvent ->
  forever (liftIO getLine >>= sendEvent)

timer :: MonadIO m => Event m Int
timer = let counter n f = f n >> liftIO (threadDelay 1000000) >> counter (n + 1) f
         in eventStream $ counter 1

prog :: MonadIO m => Event m ()
prog = do
  a <- inputLines *> time
  b <- timer
  liftIO $ print (a, b)

time :: MonadIO m => Behaviour m String
time = show <$> liftIO getCurrentTime

loop :: IO ()
loop = flip runContT return $ prog
