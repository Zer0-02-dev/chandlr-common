{-# LANGUAGE OverloadedStrings #-}

module Common.Component.InfiniteScroll where

import Prelude hiding ((!!))
import Miso
    ( Component (mailbox)
    , component
    , vfrag
    , Effect
    , mountWithProps
    , View
    , MisoString
    , onCreatedWith
    , consoleLog
    , get
    , io_
    , startSub
    , asyncCallback1
    , toMisoString
    , mailChildren
    , checkMail
    , consoleError
    , modify
    , withProps
    )

import Miso.Html.Property (class_)
import Miso.Html
    ( div_
    )
import Miso.DSL
    ( jsg
    , (!!)
    , (!)
    , fromJSVal
    , new
    , (#)
    , setField
    , create
    )
import Miso.JSON (Value, encode)
import Control.Monad (void, unless)
import qualified Data.Set as Set

import Common.Component.InfiniteScroll.Model
import Common.Component.InfiniteScroll.Action


maxLoadedPages :: Int
maxLoadedPages = 3


app
    :: (Eq context, Eq m, Eq props)
    => Component context props m a
    -> MisoString
    -> Component context props Model Action
app innerComponent lbl = (component initialModel update (view innerComponent))
    { mailbox = handleMail
    }

    where
        initialModel = Model
            { label = lbl
            , loadedPages = 1
            , ignoreSentinels = False
            , exhausted = Set.empty
            }

        handleMail :: Value -> Maybe Action
        handleMail = checkMail ChildMessage OnErrorMessage


view :: (Eq context, Eq m, Eq props) => Component context props m a -> Model -> View context props Model Action
view innerComponent _ = withProps $ \props -> vfrag
  [ div_
      [ class_ "sentinel sentinel-top"
      , onCreatedWith $ RegisterSentinel Top
      ] []
  , mountWithProps props innerComponent
  , div_
      [ class_ "sentinel sentinel-bottom"
      , onCreatedWith $ RegisterSentinel Bottom
      ] []
  ]

update :: Action -> Effect parent props Model Action
update (RegisterSentinel pos domRef) = do
    model <- get
    let lbl = label model
    io_ $
        consoleLog $ "InfiniteScroll " <> lbl <> " - " <> toMisoString (show pos) <> " Sentinel Registered"

    let sink_ sink _ = do
            callback <- asyncCallback1 $ \entries -> do
                entry <- entries !! 0
                isIntersecting <- entry ! "isIntersecting" >>= fromJSVal
                case isIntersecting of
                    Just True -> sink $ ReachedTarget pos
                    _ -> return ()

            options <- create
            setField options "rootMargin" ("1100px" :: MisoString)
            setField options "threshold"  (0 :: Double)

            iObsC <- jsg "IntersectionObserver"
            iObs <- new iObsC (callback, options)
            void $ iObs # "observe" $ [ domRef ]

    startSub (lbl <> "-" <> toMisoString (show pos)) sink_

update (ReachedTarget pos) = do
    model <- get
    io_ $ consoleLog $ "InfiniteScroll REACHED " <> toMisoString (show pos) <> " sentinels "
        <> if ignoreSentinels model then "locked" else "unlocked"

    unless (ignoreSentinels model || Set.member pos (exhausted model)) $
        mailChildren $ Grow pos

update (ChildMessage (Loaded Bottom)) = do
    model <- get

    if loadedPages model == maxLoadedPages
    then do
        io_ $ consoleLog "ChildMessage (Loaded Bottom): Trim and lock sentinels"
        mailChildren $ Trim $ opposite Bottom
        modify $ \m -> m { ignoreSentinels = True }
    else
        modify $ \m -> m { loadedPages = loadedPages m + 1 }

update (ChildMessage Reset) =
    modify $ \m -> m { loadedPages = 1 }

update (ChildMessage AllClear) = do
    io_ $ consoleLog "ChildMessage AllClear: unlock sentinels"
    modify $ \m -> m { ignoreSentinels = False }

update (ChildMessage (Exhausted pos)) =
    modify $ \m -> m { exhausted = Set.insert pos (exhausted m) }

update (ChildMessage m) = io_ $ consoleError $ "Not Implemented " <> encode m

update (OnErrorMessage msg) =
    io_ $ consoleError ("InfiniteScroll Component OnErrorMessage decode failure: " <> toMisoString msg)


opposite :: SentinelPosition -> SentinelPosition
opposite Top    = Bottom
opposite Bottom = Top
