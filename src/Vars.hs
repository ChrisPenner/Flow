{-# language DeriveFunctor #-}
{-# language ScopedTypeVariables #-}
module Vars where

-- import Control.Concurrent.MVar
import Control.Concurrent.Async
-- import Control.Monad
import Data.Time

data Event m a = Event ((a -> m ()) -> m ())
  deriving Functor

data Behaviour m a = Behaviour (m a)

getLines :: Event IO String
getLines = Event go
  where
    go :: (String -> IO ()) -> IO ()
    go next = do
      l <- getLine
      case l of
        'q':_ -> return ()
        txt -> next txt >> go next

currentTime :: Behaviour IO String
currentTime = Behaviour (show <$> getCurrentTime)

listen :: Event IO a -> (a -> IO ()) -> IO (Async ())
listen (Event f) handler = async $ f handler

sample :: Behaviour m a -> m a
sample (Behaviour m) = m

-- pair :: forall a b. Event IO a -> Behaviour IO b -> Event IO b
-- pair (Event f) (Behaviour m) = Event go
  -- where
    -- go :: (b -> IO ()) -> IO ()
    -- go next = \n -> m >>= next

-- thing :: IO ()
-- thing = do
  -- let lengths = length <$> getLines
  -- let times = pair lengths currentTime
  -- job <- listen lengths print
  -- job <- listen times print
  -- wait job
