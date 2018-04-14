{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
module Classes where

import Control.Monad.Cont
import Data.Time

-- type SendEvent = forall a b m. a -> m b
-- data Events m a = Events (ContT () m a)
type Event m a = ContT () m a
type Behaviour m a = m a

eventStream :: ((a -> m ()) -> m ()) -> Event m a
eventStream eventSpawner = ContT eventSpawner

lineEvent :: MonadIO m => Event m String
lineEvent = eventStream allLines

allLines :: MonadIO m => (String -> m ()) -> m ()
allLines sendEvent = forever (liftIO getLine >>= sendEvent)

prog :: MonadIO m => Event m ()
prog = do
  a <- lineEvent *> time
  liftIO $ print a

time :: MonadIO m => Behaviour m String
time = show <$> liftIO getCurrentTime

loop :: IO ()
loop = flip runContT return $ prog
