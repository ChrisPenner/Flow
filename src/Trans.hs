{-# language DeriveFunctor #-}
{-# language ExistentialQuantification #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
module Trans where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.State
import Control.Monad.Reader

data Event a = forall b. Event (TChan b) (b -> a)

instance Functor Event where
  fmap f (Event t g) = Event t (f . g)

data Trigger a = Trigger (a -> STM ())

newtype Network a = Network (ReaderT (IO ()) (StateT [Async ()] IO) a)
  deriving (Functor, Applicative, Monad, MonadReader (IO ()), MonadState [Async ()], MonadIO)

newEvent :: STM (Event a, Trigger a)
newEvent = do
  broadcastChannel <- newBroadcastTChan
  let evt = Event broadcastChannel id
  return (evt, Trigger (writeTChan broadcastChannel))

fire :: Trigger a -> a -> STM ()
fire (Trigger f) = f

respond :: Event a -> (a -> IO ()) -> Network ()
respond (Event bChan t) f = do
  w <- liftIO . async $ handler
  modify (w:)
    where
      handler = do
        freshEventChannel <- liftIO . atomically $ dupTChan bChan
        liftIO . forever $ do
          a <- atomically $ readTChan freshEventChannel
          f (t a)

linesEvent :: Network (Event String)
linesEvent = do
  (evt, trigger) <- liftIO . atomically $ newEvent
  w <- liftIO . async $ getLines trigger
  modify (w:)
  return evt
    where
      getLines trigger = forever $ do
        ln <- getLine
        atomically $ fire trigger ln

network :: Network ()
network = do
  evt <- linesEvent
  let numbers = show . length <$> evt
  exitIO <- ask
  respond evt $ \case
    ('q':_) -> exitIO >> liftIO (print "exiting?")
    l -> liftIO $ print l
  respond numbers print

exit :: Network ()
exit = ask >>= liftIO >> liftIO (print "EXITED")

runNetwork :: Network () -> IO ()
runNetwork (Network m) = do
  waitExit <- newTVarIO False
  jobs <- flip execStateT [] . flip runReaderT (exitIO waitExit) $ m
  print "PAST!"
  atomically $ readTVar waitExit >>= check
  mapM_ cancel jobs
    where
      exitIO :: TVar Bool -> IO ()
      exitIO exitVar = atomically $ writeTVar exitVar True
