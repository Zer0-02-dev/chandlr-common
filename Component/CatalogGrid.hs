{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Common.Component.CatalogGrid
    ( Model
    , Action (..)
    , Props (..)
    , view
    , update
    , app
    , GridComponent
    , OutMessage (..)
    , catalogOutTopic
    ) where

import Data.Maybe (maybeToList)
import Data.Either (fromRight)
import Miso
    ( View
    , Effect
    , text
    -- , rawHtml
    , onWithOptions
    , defaultOptions
    , Options (_preventDefault)
    , Attribute
    , emptyDecoder
    , publish
    , io_
    , consoleLog
    , key_
    , vfrag
    , withProps
    )
import Miso.Html
    ( div_
    , img_
    , a_
    , b_
    , span_
    , p_
    )
import Miso.Html.Property
    ( src_
    , title_
    , class_
    , href_
    , data_
    )
import Miso.String (toMisoString, fromMisoString, MisoString, intercalate)
import qualified Miso as M

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Parsing.EmbedParser (extractVideoId)
import Common.Component.CatalogGrid.GridTypes
import qualified Common.Network.SiteType as Site
import qualified Common.Component.BodyRender as Body
import qualified Common.AttachmentType as At


app :: (Foldable f) => GridComponent context f
app =
    M.Component
        { M.model = ()
        , M.hydrateModel = Nothing
        , M.update = update
        , M.view = view
        , M.subs = []
        , M.styles = []
        , M.mountPoint = Nothing
        , M.logLevel = M.DebugAll
        , M.scripts = []
        , M.mailbox = const Nothing
        , M.eventPropagation = False
        , M.mount = Nothing
        , M.unmount = Nothing
        , M.onPropsChanged = Nothing
        , M.useContext = False
        }


-- Custom event handler with preventDefault set to True
onClick_ :: a -> Attribute m a
onClick_ action = onWithOptions M.BUBBLE defaultOptions { _preventDefault = True } "click" emptyDecoder (const $ const $ const action)


update :: Action -> Effect parent props Model Action
update (ThreadSelected post) = do
    io_ $ do
        consoleLog $ "ThreadSelected - " <> toMisoString (show $ CatalogPost.thread_id post)
        publish catalogOutTopic $ SelectThread post


view
  :: (Foldable f) => Model -> View context (Props f) Model Action
view _ = withProps $ \props -> vfrag
      (foldMap ((: []) . (gridItem props)) (display_items props))


gridItem :: Props f -> CatalogPost -> View context (Props f) model Action
gridItem props post =
    div_
        [ class_ "thread grid-li grid-size-small"
        , data_ "site" (CatalogPost.site_name post)
        , key_ ("thread#" <> show (CatalogPost.thread_id post))
        ]
        [ a_
            [ href_ thread_url
            , onClick_ (ThreadSelected post)
            ]
            [ img_ $
                [ class_ "thread-image"
                , src_ thumb_url
                , title_ ( toMisoString $ show $ CatalogPost.bump_time post )
                ] ++ dimension
            ]
        , div_
            [ class_ "replies" ]
            (
              [ div_
                  [ class_ "meta" ]
                  [ "R: "
                  , b_ [][ text post_count_str ]
                  ]
              ] ++ (intro subject) ++ body
            )
        ]

  where
    subject :: [ View ctx props model a ]
    subject = map (text . toMisoString) $ maybeToList $ CatalogPost.subject post

    intro :: [ View ctx props model a ] -> [ View ctx props model a ]
    intro [] = []
    intro x = (: []) $ p_
        [ class_ "intro" ]
        [ span_
            [ class_ "subject" ]
            x
        ]

    body :: [ View ctx props model a ]
    -- body = map (rawHtml . toMisoString) $ maybeToList $ CatalogPost.body post
    body =
        let site = Site.fromCatalogPost post
        in
            concatMap ((Body.render site) . snd) (Body.getPostWithBodies site)

    post_count_str :: MisoString
    post_count_str = toMisoString $ (CatalogPost.estimated_post_count post) - 1

    embed_url :: Maybe String
    embed_url =
        (CatalogPost.embed post) >>= Just . (fromRight "") . extractVideoId . fromMisoString

    thumb_url :: MisoString
    thumb_url  =
        case embed_url of
            Nothing ->
                case mthumb_path of
                    -- TODO: what about embeds!?
                    Nothing -> "/static/default_thumbnail.png"
                    Just thumb_path -> (media_root props) <> (toMisoString thumb_path)
            Just u -> "https://leftychan.net/vi/" <> toMisoString u <> "/0.jpg"

    mthumb_path :: Maybe MisoString
    mthumb_path = do
        file_name <- CatalogPost.file_name post
        thumb_ext <- CatalogPost.file_thumb_extension post

        return $
            "/" <> CatalogPost.site_name post
            <> "/" <> CatalogPost.pathpart post
            <> "/" <> (toMisoString $ show $ CatalogPost.board_thread_id post)
            <> "/thumbnail_" <> file_name
            <> "." <> thumb_ext

    thread_url :: MisoString
    thread_url = toMisoString $ intercalate "/" $ map fromMisoString $
      [ CatalogPost.site_name post
      , CatalogPost.pathpart post
      , toMisoString $ show $ CatalogPost.board_thread_id post
      ]


    dimension :: [ Attribute m a ]
    dimension = maybe []
        ( \res ->
            [ data_ "width" $ toMisoString $ At.width res
            , data_ "height" $ toMisoString $ At.height res
            ]
        )
        (CatalogPost.file_resolution post)
