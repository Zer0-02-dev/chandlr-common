{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Common.Cookies where

import Web.Cookie (parseCookies)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Servant.API
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Integer.Conversion (textToInteger)
import Common.BitField (intsFromBitField)

newtype CookieJar = CookieJar { unCookies :: Map.Map Text Text }
    deriving (Show, Eq)

instance FromHttpApiData CookieJar where
    parseHeader bs = Right $ CookieJar $ Map.fromList
        [ (TE.decodeUtf8 k, TE.decodeUtf8 v)
        | (k, v) <- parseCookies bs
        ]

    parseUrlPiece = const $ Left "CookieJar cannot be parsed from a URL piece"


type family WithCookie api where
    -- Recurse through alternative routes
    WithCookie (a :<|> b) = WithCookie a :<|> WithCookie b
    
    -- Recurse through route modifiers (Capture, QueryParam, etc.)
    WithCookie (a :> b)   = a :> WithCookie b
    
    -- Inject the cookie header at the actual endpoints (leaves)
    WithCookie leaf       = Header "Cookie" CookieJar :> leaf


getBoardIdsFromCookie :: CookieJar -> Maybe (Set Int)
getBoardIdsFromCookie (CookieJar cookies) =
    ((intsFromBitField . textToInteger) <$> (Map.lookup "b" cookies)) >>=
        \s -> if Set.null s then Nothing else Just s
