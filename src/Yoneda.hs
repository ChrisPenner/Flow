{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yoneda where

import Control.Arrow
import Control.Category as C
import Data.Profunctor as P
import Data.Profunctor.Yoneda

newtype EventA a b =
  EventA (Yoneda (->) a b)
  deriving (Profunctor, Strong)

instance Category EventA where
  id = EventA C.id
  (EventA a) . (EventA b) = EventA (a C.. b)

instance Arrow EventA where
  first = first'
  arr f = EventA (Yoneda go)
    where
      go xa by = by <<< f <<< xa
