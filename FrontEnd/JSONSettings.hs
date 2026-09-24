{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Common.FrontEnd.JSONSettings where

import Miso
    ( View
    )
import Miso.String (MisoString, toMisoString)
import Miso.Html
    ( meta_
    )
import Miso.Html.Property
    ( name_
    , content_
    )
import GHC.Generics
import Miso.JSON (FromJSON)
#ifndef FRONT_END
import qualified Common.Server.JSONSettings as S
import Miso.String (fromMisoString, pack)
#endif

data JSONSettings = JSONSettings
    { postgrest_url :: MisoString
    , jwt :: String
    , postgrest_fetch_count :: Int
    , media_root :: MisoString
    , media_root_path :: MisoString
    , static_serve_path :: String
    , static_serve_url_root :: String
    , admin :: Bool
    , spam_noticer_url :: String
    } deriving (Show, Eq, Generic)

instance FromJSON JSONSettings


asHtml :: JSONSettings -> [ View ctx props model action ]
asHtml settings =
    [ meta "postgrest-url" (toMisoString $ postgrest_url settings)
    , meta "postgrest-fetch-count" (toMisoString $ postgrest_fetch_count settings)
    , meta "media-root" (toMisoString $ media_root settings)
    , meta "admin" (if admin settings then "True" else "False")
    , meta "hydrate" "True"
    ]

    where
        meta name value = meta_ [ name_ name, content_ value ]

#ifndef FRONT_END
clientSettings :: JSONSettings -> S.JSONSettings
clientSettings (JSONSettings {..}) = S.JSONSettings
    { S.postgrest_url = fromMisoString postgrest_url
    , S.jwt = pack jwt
    , S.backup_read_root = undefined
    , S.media_root_path = fromMisoString media_root_path
    , S.site_name = undefined
    , S.site_url = undefined
    }
#endif
