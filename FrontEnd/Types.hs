{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Types where

import GHC.Generics
import Miso.JSON
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Miso (URI (uriQueryString))
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.Thread.Model as Thread
import Common.Network.SiteType (Site)
import Common.FrontEnd.JSONSettings (JSONSettings)

data InitialDataPayload = InitialDataPayload
    { timestamp :: UTCTime
    , initialData :: InitialData
    , sitesAndBoards :: [ Site ]
    } deriving (Eq, Generic, FromJSON)

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site [ Thread.PostWithBody ]
    | Nil
    deriving (Eq, Generic)

instance FromJSON InitialData where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString
        case tag of
            "CatalogData"  -> CatalogData <$> m .: "contents"
            "SearchData"   -> SearchData  <$> m .: "contents"
            "ThreadData"   -> do
                (site, posts) <- m .: "contents"
                pure $ ThreadData site posts
            "Nil"          -> pure Nil
            _              -> fail "Unknown InitialData tag"
    parseJSON _ = fail "Expected Object for InitialData"

data AppInitCtx = AppInitCtx
    { hydrate :: Bool
    , init_board_selection :: Maybe (Set.Set Int)
    , init_uri :: URI
    , init_settings :: JSONSettings
    , init_payload :: InitialDataPayload
    } deriving Eq

type InitCtxRef = IORef AppInitCtx

data MessagesFromChildren
    = MsgClientMounted
    | MsgThreadViewMounted
    deriving Eq

instance ToJSON MessagesFromChildren where
    toJSON MsgClientMounted     = String "MsgClientMounted"
    toJSON MsgThreadViewMounted = String "MsgThreadViewMounted"

instance FromJSON MessagesFromChildren where
    parseJSON (String "MsgClientMounted") = pure MsgClientMounted
    parseJSON (String "MsgThreadViewMounted") = pure MsgThreadViewMounted
    parseJSON _ = fail "Expected JSON String for MessagesFromChildren deserialization"

data Time = Now UTCTime | Then UTCTime
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


timeFromInitialPayload :: AppInitCtx -> Time
timeFromInitialPayload ctx =
    (
        if Map.member "t" (uriQueryString uri)
        then
            Then
        else
            Now
    ) (timestamp p)

    where
        p = init_payload ctx
        uri = init_uri ctx


data DeletePostResults = DeletePostResults
    { sites :: [ Site ]
    , noticer_success_fail_counts :: (Int, Int)
    } deriving (Generic, Eq, ToJSON, FromJSON)
