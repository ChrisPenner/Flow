{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
module Classes where

import Control.Monad.Cont
import Data.Time

-- type SendEvent = forall a b m. a -> m b
-- data Events m a = Events (ContT () m a)
type Event m a = m a
type Behaviour m a = m a

eventStream :: (MonadIO m, MonadCont m) => ((a -> m b) -> m a) -> Event m a
eventStream = callCC

lineEvent :: (MonadIO m, MonadCont m) => Event m String
lineEvent = eventStream allLines

allLines :: MonadIO m => (String -> m a) -> m a
allLines sendEvent = forever (liftIO getLine >>= sendEvent)

prog :: (MonadCont m, MonadIO m) => Event m ()
prog = do
  a <- lineEvent *> time
  liftIO $ print a

time :: MonadIO m => Behaviour m String
time = show <$> liftIO getCurrentTime

loop :: IO ()
loop = flip runContT return $ prog
