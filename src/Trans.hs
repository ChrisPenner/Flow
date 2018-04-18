{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trans where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.State
import Data.Time

data Event a =
  forall b. Event (TChan b)
                  (b -> a)

instance Functor Event where
  fmap f (Event t g) = Event t (f . g)

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

pair :: MonadIO m => Event a -> Behaviour m b -> Network m (Event b)
pair evt (Behaviour samp) = mapEventM evt (const samp)

mapEventM :: MonadIO m => Event a -> (a -> m b) -> Network m (Event b)
mapEventM evt f = do
  (freshEvt, freshTrig) <- newEvent
  react evt $ \e -> f e >>= fire freshTrig
  return freshEvt

newEvent :: MonadIO m => Network m (Event a, Trigger m a)
newEvent =
  liftIO . atomically $ do
    broadcastChannel <- newBroadcastTChan
    let evt = Event broadcastChannel id
    return (evt, Trigger (liftIO . atomically . writeTChan broadcastChannel))

eventFromTrigger :: MonadIO m => ((a -> m ()) -> m ()) -> Network m (Event a)
eventFromTrigger handler = do
  (evt, trigger) <- newEvent
  runJob $ handler (fire trigger)
  return evt

runJob :: MonadIO m => m () -> Network m ()
runJob job = do
  modify (\st@NetworkState {jobs = js} -> st {jobs = (job : js)})

react :: MonadIO m => Event a -> (a -> m ()) -> Network m ()
react (Event bChan t) f = runJob handler
  where
    handler = do
      freshEventChannel <- liftIO . atomically $ dupTChan bChan
      forever $ do
        a <- liftIO . atomically $ readTChan freshEventChannel
        f (t a)

linesEvent :: MonadIO m => Network m (Event String)
linesEvent =
  eventFromTrigger $ \trig ->
    forever $ do
      ln <- liftIO getLine
      trig ln

network :: Network IO ()
network = do
  exit <- gets signalExit
  evt <- linesEvent
  let numbers = show . length <$> evt
      timeBehaviour = Behaviour getCurrentTime
  timeEvents <- pair evt timeBehaviour
  react timeEvents print
  react evt $ \case
    ('q':_) -> liftIO (print "exiting") >> exit
    l -> liftIO $ print l
  react numbers (liftIO . print)

runSimple :: MonadIO m => NetworkState m -> Network m () -> m (NetworkState m)
runSimple netState (Network m) = flip execStateT netState $ m

runNetwork :: MonadIO m => (m () -> IO ()) -> Network m () -> m ()
runNetwork toIO m = do
  exitVar <- liftIO $ newTVarIO False
  let initialNetworkState =
        NetworkState {signalExit = exitIO exitVar, jobs = []}
  NetworkState {jobs = js} <- runSimple initialNetworkState $ m
  allJobs <- liftIO $ runAll js
  liftIO . (`catch` handleInterrupt allJobs) $ do
    atomically $ (readTVar exitVar >>= check)
    mapM_ cancel allJobs
  where
    exitIO :: MonadIO m => TVar Bool -> m ()
    exitIO exitVar = liftIO . atomically $ writeTVar exitVar True
    runAll = mapM (async . toIO)
    handleInterrupt :: [Async ()] -> AsyncException -> IO ()
    handleInterrupt allJobs _ = mapM_ cancel allJobs
