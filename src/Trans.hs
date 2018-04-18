{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trans where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

data Event a =
  forall b. Event (TChan b)
                  (b -> a)

instance Functor Event where
  fmap f (Event t g) = Event t (f . g)

data Trigger a = Trigger
  { fire :: (a -> IO ())
  }

data NetworkState = NetworkState
  { signalExit :: IO ()
  , jobs :: TVar [Async ()]
  }

newtype Network m a =
  Network (ReaderT NetworkState m a)
  deriving (Functor, Applicative, Monad, MonadReader NetworkState)

deriving instance MonadIO m => MonadIO (Network m)

newEvent :: MonadIO m => Network m (Event a, Trigger a)
newEvent =
  liftIO . atomically $ do
    broadcastChannel <- newBroadcastTChan
    let evt = Event broadcastChannel id
    return (evt, Trigger (atomically . writeTChan broadcastChannel))

eventFromTrigger :: MonadIO m => ((a -> IO ()) -> IO ()) -> Network m (Event a)
eventFromTrigger handler = do
  (evt, trigger) <- newEvent
  runJob $ handler (fire trigger)
  return evt

runJob :: MonadIO m => IO () -> Network m ()
runJob job = do
  jobVar <- asks jobs
  liftIO $ do
    w <- async job
    atomically $ modifyTVar jobVar (w :)

react :: MonadIO m => Event a -> (a -> Network IO ()) -> Network m ()
react (Event bChan t) f = do
  networkState <- ask
  runJob $ handler networkState
  where
    handler networkState = do
      freshEventChannel <- liftIO . atomically $ dupTChan bChan
      liftIO . forever $ do
        a <- atomically $ readTChan freshEventChannel
        runSimple networkState $ f (t a)

linesEvent :: MonadIO m => Network m (Event String)
linesEvent =
  eventFromTrigger $ \trig ->
    forever $ do
      ln <- getLine
      trig ln

network :: MonadIO m => Network m ()
network = do
  evt <- linesEvent
  let numbers = show . length <$> evt
  react evt $ \case
    ('q':_) -> liftIO (print "exiting") >> exit
    l -> liftIO $ print l
  react numbers (liftIO . print)

exit :: MonadIO m => Network m ()
exit = asks signalExit >>= liftIO

runSimple :: MonadIO m => NetworkState -> Network m () -> m ()
runSimple exitIO (Network m) = flip runReaderT exitIO $ m

runNetwork :: MonadIO m => Network m () -> m ()
runNetwork m = do
  (jobListVar, exitVar) <- liftIO $ liftA2 (,) (newTVarIO []) (newTVarIO False)
  let initialNetworkState =
        NetworkState {signalExit = exitIO exitVar, jobs = jobListVar}
  runSimple initialNetworkState $ m
  liftIO . atomically $ (readTVar exitVar >>= check)
  liftIO $ readTVarIO jobListVar >>= mapM_ cancel
  where
    exitIO :: TVar Bool -> IO ()
    exitIO exitVar = atomically $ writeTVar exitVar True
