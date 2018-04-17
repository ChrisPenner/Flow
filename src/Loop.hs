{-# language RankNTypes #-}
module Loop where

-- import Control.Concurrent.Async
-- import Reactive.Banana
-- import Reactive.Banana.Frameworks
-- import Control.Monad

-- runBanana :: IO ()
-- runBanana = compile network >>= actuate

-- network :: MomentIO ()
-- network = do
--   inputLines <- lineEvents
--   reactimate $  print <$> inputLines
--   return ()

-- lineEvents :: MomentIO (Event String)
-- lineEvents = do
--   (events, fireLineEvent) <- newEvent
--   liftIO $ async (forever (getLine >>= fireLineEvent))
--   return events
