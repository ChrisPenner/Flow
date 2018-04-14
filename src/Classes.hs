{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
module Classes where

import Control.Monad.Cont
import Data.Time

type EventGenerator m a = forall b. ((a -> m b) -> m b)
type Event m a = ContT () m a
type Behaviour m a = m a

eventStream :: EventGenerator m a -> Event m a
eventStream eventSpawner = ContT eventSpawner

lineEvent :: MonadIO m => Event m String
lineEvent = eventStream allLines

allLines :: MonadIO m => EventGenerator m String
allLines sendEvent = forever (liftIO getLine >>= sendEvent)

prog :: MonadIO m => Event m ()
prog = do
  a <- lineEvent *> time
  liftIO $ print a

time :: MonadIO m => Behaviour m String
time = show <$> liftIO getCurrentTime

loop :: IO ()
loop = flip runContT return $ prog
