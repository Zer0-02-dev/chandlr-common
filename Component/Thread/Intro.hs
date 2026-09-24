{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Common.Component.Thread.Intro where

import Miso
  ( View
  , text
  , textProp
  )
import Miso.Html
  ( a_
  , span_
  , time_
  )
import Miso.Html.Property
  ( href_
  , class_
  , title_
  )

import qualified Data.Map as Map
import Miso.String (MisoString, toMisoString)
import Data.Foldable (toList)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Common.Network.PostType (Post)
import qualified Common.Network.PostType as Post
import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import Common.Network.BoardType (Board)
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import Common.Network.ThreadType (Thread)
import Common.Parsing.BodyParser (Backlinks)


formatUTC :: UTCTime -> MisoString
formatUTC time = toMisoString $
    formatTime defaultTimeLocale "%Y-%m-%d (%a) %T" time


intro :: Site -> Board -> Thread -> Post -> Backlinks -> UTCTime -> View ctx props model a
intro site board thread post backlinks current_time = span_
  [ class_ "intro" ]
  ( subject ++
    [ " "
    , span_
        [ class_ "name" ] [ text name ]
    -- TODO: Add flags (don't have that data in the db yet)
    , " "
    , time_
        [ textProp "datetime" $ toMisoString $ show $ creation_time
        , title_ $ toMisoString $ timeAgo current_time creation_time
        ] [ text $ formatUTC creation_time ]
    , " "
    , a_
        [ class_ "post_no"
        , href_ $ toMisoString $ post_url <> "#" <> b_post_id
        ] [ "No." ]
    , a_
        [ class_ "post_no"
        , href_ $ toMisoString $ post_url <> "#q" <> b_post_id
        ] [ text $ toMisoString $ b_post_id ]
    ]
    ++ mentions
  )

  where
    post_url :: MisoString
    post_url
        =  "/" <> (Site.name site)
        <> "/" <> (Board.pathpart board)
        <> "/" <> (toMisoString $ show $ Thread.board_thread_id thread)

    creation_time :: UTCTime
    creation_time = Post.creation_time post

    subject :: [ View ctx props model a ]
    subject = map (mkSubject . toMisoString) $ toList $ Post.subject post

    name :: MisoString
    name = maybe "Anonymous" toMisoString $ Post.name post

    mkSubject :: MisoString -> View ctx props model a
    mkSubject s = span_
      [ class_ "subject" ]
      [ text s ]

    b_post_id :: MisoString
    b_post_id = toMisoString $ show $ Post.board_post_id post

    mentions :: [ View ctx props model a ]
    mentions =
        case Map.lookup (Post.board_post_id post) backlinks of
            Nothing -> []
            Just [] -> []
            Just xs -> span_
                [ class_ "mentioned unimportant" ]
                (map mention xs)
                : []

    mention :: Post -> View ctx props model a
    mention p =
        a_
            [ href_ $ "#" <> bpid
            ]
            [ text $ ">>" <> bpid ]

        where
            bpid :: MisoString
            bpid = toMisoString $ show $ Post.board_post_id p


-- Convert UTCTime to a human-readable string
timeAgo :: UTCTime -> UTCTime -> String
timeAgo currentTime pastTime =
    let diff = realToFrac $ diffUTCTime currentTime pastTime
    in humanReadableTimeDiff diff


-- Helper function to correctly format singular and plural units
formatTimeUnit :: (Integral a, Show a) => a -> String -> String
formatTimeUnit value unit
    | value == 1 = show value ++ " " ++ unit ++ " ago"
    | otherwise  = show value ++ " " ++ unit ++ "s ago"


-- Convert time difference in seconds to a human-readable format
humanReadableTimeDiff :: Double -> String
humanReadableTimeDiff diff
    | diff < 60 = "Just now"
    | diff < 3600 = formatTimeUnit (truncate (diff / 60) :: Int) "minute"
    | diff < 86400 = formatTimeUnit (truncate (diff / 3600) :: Int) "hour"
    | diff < 604800 = formatTimeUnit (truncate (diff / 86400) :: Int) "day"
    | diff < 2592000 = formatTimeUnit (truncate (diff / 604800) :: Int) "week"
    | diff < 31536000 = formatTimeUnit (truncate (diff / 2592000) :: Int) "month"
    | otherwise = formatTimeUnit (truncate (diff / 31536000) :: Int) "year"
