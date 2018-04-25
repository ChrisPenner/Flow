{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network where

import Control.Arrow
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State
import Data.Foldable

import Arrows
import Trans

network :: Network IO ()
network = do
  exit <- gets signalExit
  evt <- linesEvent
  react (scanE (fmap length evt) (+) 0) print
  react evt $ \case
    ('q':_) -> exit
    l -> print l

arrNetwork' :: Network IO (Trigger IO String, Event IO Int)
arrNetwork' =
  runEventA $
  proc inp ->
  do num <- scanA (\ acc new -> acc + length new) 0 -< inp
     returnA -< num

arrNetwork :: Network IO ()
arrNetwork = do
  (trig, evt) <- arrNetwork'
  runJob $ getLine >>= fire trig
  react evt print

runSimple :: MonadIO m => NetworkState m -> Network m () -> m (NetworkState m)
runSimple netState (Network m) = flip execStateT netState $ m

runNetwork ::
     forall m. MonadIO m
  => (m () -> IO ())
  -> Network m ()
  -> m ()
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
