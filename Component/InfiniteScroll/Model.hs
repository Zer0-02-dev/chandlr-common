module Common.Component.InfiniteScroll.Model where

import Miso (MisoString)
import Data.Set (Set)

import Common.Component.InfiniteScroll.Action

data Model = Model
    { label :: MisoString
    , loadedPages :: Int
    , ignoreSentinels :: Bool
    , exhausted :: Set SentinelPosition
    } deriving Eq
