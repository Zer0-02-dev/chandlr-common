{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Common.Component.TimeControl where

import Control.Monad.IO.Class (liftIO)
import Miso
    ( View
    , consoleLog
    , Effect
    , Component
    , modify
    , io
    , io_
    , Topic
    , topic
    , publish
    )
import Miso.Html
    ( div_
    , input_
    , onInput
    , onChange
    )
import Miso.Html.Property
    ( class_
    , step_
    , min_
    , max_
    , type_
    , value_
    )

import qualified Miso as M
import Miso.String (MisoString, toMisoString, fromMisoString)
import Data.Time.Clock
  ( UTCTime (..)
  , getCurrentTime
  , diffUTCTime
  , addUTCTime
  , secondsToDiffTime
  )
import Data.Time.Calendar (fromGregorian)
import Common.FrontEnd.Types (InitCtxRef)
import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)


data Time
  = SlideInput MisoString
  | SlideChange MisoString
  | Publish Message
  deriving Show

data Model = Model
  { whereAt :: Integer
  , atNow     :: Bool
  } deriving Eq

type TimeControl parent props = Component parent props Model Time

data Message = Message
    { msgAtNow :: Bool -- now or earlier
    , utcTime :: UTCTime
    }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


timeControlTopic :: Topic Message
timeControlTopic = topic "time-control"

view :: Model -> View context props Model Time
view m =
    div_
        [ class_ "time-control"
        ]
        [ input_
            [ class_ "time-slider"
            , type_ "range"
            , min_ "-500"
            , max_ "0"
            , step_ "1"
            , value_ $ toMisoString $ show (whereAt m)
            , onInput SlideInput
            , onChange SlideChange
            ]
        ]

update
    :: Time
    -> Effect parent props Model Time
update (SlideInput nstr) = io_ $
  consoleLog $ "Input: " <> nstr

update (SlideChange nstr) = do
  modify (\model ->  model { whereAt = n })

  io $ do
    consoleLog $ "Change: " <> nstr

    now <- liftIO getCurrentTime

    let (isNow, newTime) = interpolateTimeHours n now

    return $ Publish $ Message isNow newTime

  where
    n :: Integer
    n = read $ fromMisoString nstr

update (Publish m) = io_ $ publish timeControlTopic m

earliest :: UTCTime
--earliest = UTCTime (fromGregorian 2020 12 20) (secondsToDiffTime 82643)
earliest = UTCTime (fromGregorian 2020 12 20) (secondsToDiffTime 82644)


-- Linear interpolation function using hours
-- the Boolean means is it right now
interpolateTimeHours :: Integer -> UTCTime -> (Bool, UTCTime)
interpolateTimeHours n currentTime
  | n == 0 = (True, currentTime)
  | n == -500 = (False, earliest)
  | otherwise = (False,
      addUTCTime (fromIntegral hoursToAdjust * secondsInHour) currentTime)

  where
    -- Calculate the total number of hours between the current time and the target date
    totalHours = diffUTCTime currentTime earliest / secondsInHour

    -- Calculate the number of hours to adjust based on linear interpolation
    hoursToAdjust :: Integer
    hoursToAdjust = round $ totalHours * (fromIntegral n / 500.0)

    -- One hour in seconds
    secondsInHour = 3600

app
    :: InitCtxRef
    -> TimeControl parent props
app _ = M.Component
    { M.model            = Model 0 True
    , M.hydrateModel     = Nothing
    , M.update           = update
    , M.view             = view
    , M.subs             = []
    , M.styles           = []
    , M.mountPoint       = Nothing
    , M.logLevel         = M.DebugAll
    , M.scripts          = []
    , M.mailbox          = const Nothing
    , M.eventPropagation = False
    , M.mount            = Nothing
    , M.unmount          = Nothing
    , M.onPropsChanged   = Nothing
    , M.useContext       = False
    }
