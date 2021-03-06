{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trans where

import Control.Arrow
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State
import Data.Foldable

data Event m a = Event
  { newGetter :: (m (m a))
  } deriving (Functor)

data Trigger m a = Trigger
  { fire :: (a -> m ())
  }

newtype Behaviour m a =
  Behaviour (m a)
  deriving (Functor, Applicative, Monad)

data NetworkState m = NetworkState
  { signalExit :: m ()
  , jobs :: [m ()]
  }

newtype Network m a =
  Network (StateT (NetworkState m) m a)
  deriving (Functor, Applicative, Monad, MonadState (NetworkState m))

deriving instance MonadIO m => MonadIO (Network m)

instance MonadTrans Network where
  lift = Network . lift

sample :: MonadIO m => Behaviour m a -> m a
sample (Behaviour ma) = ma

stepper :: (MonadIO m) => Event m a -> Behaviour m b -> Event m b
stepper evt (Behaviour samp) = mapEventM (const samp) evt

scanE :: (MonadIO m) => Event m a -> (b -> a -> b) -> b -> Event m b
scanE (Event evt) f initial =
  Event $ do
    getter <- evt
    accVar <- liftSTM $ newTVar initial
    return $ do
      acc <- liftSTM $ readTVar accVar
      next <- getter
      let newAcc = f acc next
      liftSTM $ writeTVar accVar newAcc
      return newAcc

mapEventM :: (MonadIO m) => (a -> m b) -> Event m a -> Event m b
mapEventM f (Event m) = Event (fmap (>>= f) m)

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

newEvent :: MonadIO m => Network m (Event m a, Trigger m a)
newEvent =
  liftSTM $ do
    broadcastChan <- newBroadcastTChan
    return (buildEvent broadcastChan, buildTrigger broadcastChan)
  where
    buildTrigger chan = Trigger (liftSTM . writeTChan chan)
    buildEvent chan =
      Event . liftSTM $ do
        newChan <- (dupTChan chan)
        return . liftSTM . readTChan $ newChan

eventFromTrigger :: MonadIO m => ((a -> m ()) -> m ()) -> Network m (Event m a)
eventFromTrigger handler = do
  (evt, trigger) <- newEvent
  runJob $ handler (fire trigger)
  return evt

runJob :: MonadIO m => m () -> Network m ()
runJob job = modify (\st@NetworkState {jobs = js} -> st {jobs = (job : js)})

react :: (MonadIO m) => Event m a -> (a -> m ()) -> Network m ()
react (Event eventM) f = do
  getter <- lift eventM
  runJob . forever $ do
    a <- getter
    f a

linesEvent :: MonadIO m => Network m (Event m String)
linesEvent =
  eventFromTrigger $ \trig ->
    forever $ do
      ln <- liftIO getLine
      trig ln
