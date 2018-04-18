{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trans where

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
  { fire :: (a -> STM ())
  }

newtype Network m a =
  Network (ReaderT (IO ()) (StateT [Async ()] m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (IO ())
           , MonadState [Async ()]
           )

deriving instance MonadIO m => MonadIO (Network m)

newEvent :: STM (Event a, Trigger a)
newEvent = do
  broadcastChannel <- newBroadcastTChan
  let evt = Event broadcastChannel id
  return (evt, Trigger (writeTChan broadcastChannel))

respond :: MonadIO m => Event a -> (a -> IO ()) -> Network m ()
respond (Event bChan t) f = do
  w <- liftIO . async $ handler
  modify (w :)
  where
    handler = do
      freshEventChannel <- liftIO . atomically $ dupTChan bChan
      liftIO . forever $ do
        a <- atomically $ readTChan freshEventChannel
        f (t a)

linesEvent :: MonadIO m => Network m (Event String)
linesEvent = do
  (evt, trigger) <- liftIO . atomically $ newEvent
  w <- liftIO . async $ getLines trigger
  modify (w :)
  return evt
  where
    getLines trigger =
      forever $ do
        ln <- getLine
        atomically $ fire trigger ln

network :: MonadIO m => Network m ()
network = do
  evt <- linesEvent
  let numbers = show . length <$> evt
  exitIO <- ask
  respond evt $ \case
    ('q':_) -> exitIO >> liftIO (print "exiting?")
    l -> liftIO $ print l
  respond numbers print

exit :: MonadIO m => Network m ()
exit = ask >>= liftIO >> liftIO (print "EXITED")

runNetwork :: MonadIO m => Network m () -> m ()
runNetwork (Network m) = do
  waitExit <- liftIO $ newTVarIO False
  jobs <- flip execStateT [] . flip runReaderT (exitIO waitExit) $ m
  liftIO $ print "PAST!"
  liftIO (atomically $ readTVar waitExit >>= check)
  liftIO $ mapM_ cancel jobs
  where
    exitIO :: TVar Bool -> IO ()
    exitIO exitVar = atomically $ writeTVar exitVar True
