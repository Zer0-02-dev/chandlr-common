module Common.Component.InfiniteScroll.Model where

import Miso (MisoString)

data Model = Model
    { label :: MisoString
    , loadedPages :: Int
    , ignoreSentinels :: Bool
    } deriving Eq
