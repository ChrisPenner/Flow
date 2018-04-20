{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language ScopedTypeVariables #-}

module Trans where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State
import Data.Foldable

data Event m a = Event
  { newGetter :: (m (m a))
  } deriving Functor

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

pair :: (MonadIO m, Show a) => Event m a -> Behaviour m b -> Event m b
pair evt (Behaviour samp) = mapEventM (const samp) evt

mapEventM :: (MonadIO m, Show a) => (a -> m b) -> Event m a -> Event m b
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
    buildEvent chan = Event . liftSTM $ do
      newChan <- (dupTChan chan)
      return . liftSTM . readTChan $ newChan


eventFromTrigger :: MonadIO m => ((a -> m ()) -> m ()) -> Network m (Event m a)
eventFromTrigger handler = do
  (evt, trigger) <- newEvent
  runJob $ handler (fire trigger)
  return evt

runJob :: MonadIO m => m () -> Network m ()
runJob job =
  modify (\st@NetworkState {jobs = js} -> st {jobs = (job : js)})

react :: (Show a, MonadIO m) => Event m a -> (a -> m ()) -> Network m ()
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

network :: Network IO ()
network = do
  exit <- gets signalExit
  evt <- linesEvent
  react evt $ \case
    ('q':_) -> exit
    l -> print l

runSimple :: MonadIO m => NetworkState m -> Network m () -> m (NetworkState m)
runSimple netState (Network m) = flip execStateT netState $ m

runNetwork :: forall m. MonadIO m => (m () -> IO ()) -> Network m () -> m ()
runNetwork toIO m = do
  exitVar <- liftIO $ (newTVarIO False)
  let initialNetworkState =
        NetworkState {signalExit = exitIO exitVar, jobs = []}
  NetworkState {jobs = js} <- runSimple initialNetworkState $ m
  asyncs <- runAll js
  waitFor exitVar
  cancelAll asyncs
  where
    waitFor exitVar = liftSTM $ (readTVar exitVar >>= check)
    exitIO :: MonadIO m => TVar Bool -> m ()
    exitIO exitVar = liftSTM $ writeTVar exitVar True
    runAll :: [m ()] -> m [Async ()]
    runAll = liftIO . mapM (async . toIO)
    cancelAll :: [Async a] -> m ()
    cancelAll asyncs = liftIO $ traverse_ cancel asyncs
