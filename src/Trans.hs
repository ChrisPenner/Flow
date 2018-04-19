{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trans where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State
import Data.Foldable

data Event m a = Event (m (m a))
  deriving Functor

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

newEvent :: MonadIO m => Network m (Event m a, Trigger m a)
newEvent =
  liftIO . atomically $ do
    broadcastChannel <- newBroadcastTChan
    let evt = Event (liftIO $ atomically (dupTChan broadcastChannel) >>= return . liftIO . atomically . readTChan)
    return (evt, Trigger (liftIO . atomically . writeTChan broadcastChannel))

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
  evt <- linesEvent
  exit <- gets signalExit
  react evt $ \case
    ('q':_) -> exit
    l -> print l

runSimple :: MonadIO m => NetworkState m -> Network m () -> m (NetworkState m)
runSimple netState (Network m) = flip execStateT netState $ m

runNetwork :: MonadIO m => (m () -> IO ()) -> Network m () -> m ()
runNetwork toIO m = do
  exitVar <- liftIO $ (newTVarIO False)
  let initialNetworkState =
        NetworkState {signalExit = exitIO exitVar, jobs = []}
  NetworkState {jobs = js} <- runSimple initialNetworkState $ m
  asyncs <- runAll js
  waitFor exitVar
  cancelAll asyncs
  where
    waitFor exitVar = liftIO . atomically $ (readTVar exitVar >>= check)
    exitIO :: MonadIO m => TVar Bool -> m ()
    exitIO exitVar = liftIO . atomically $ writeTVar exitVar True
    runAll = liftIO . mapM (async . toIO)
    cancelAll asyncs = liftIO $ traverse_ cancel asyncs
