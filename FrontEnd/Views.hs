{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module Common.FrontEnd.Views
    ( catalogView
    , searchView
    , threadView
    , boardView
    , page404
    ) where

import Miso
    ( View
    , text
    , key_
    , mount_
    , mountWithProps
    , vfrag
    )
import Miso.Html.Property (class_)
import Miso.Html
    ( h1_
    , time_
    , div_
    , p_
    )
import Miso.String (MisoString, toMisoString)
import qualified Miso.String as Str
import Data.Text (Text)

import Common.FrontEnd.Model
import Common.FrontEnd.Action (Action (..))
import qualified Common.Component.Search as Search
import qualified Common.Component.Catalog as Catalog
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.Thread as Thread
import qualified Common.Component.Thread.Model as Thread
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Routes (BoardThreadId)
import qualified Network.Client as Client
#ifndef FRONT_END
import qualified Common.Network.ClientTypes as Client -- needed for server-side rendering
#endif
import Common.FrontEnd.Types (InitCtxRef)
import qualified Common.Admin.Component.DeleteIllegalPost as DIP
import qualified Common.Component.NavigationBar.View as Nav
import qualified Common.Component.NavigationBar as Nav
import qualified Common.Component.NavigationBar.Model as Nav
import qualified Common.Component.InfiniteScroll as Inf
import Common.Cookies (CookieJar)

timeControl :: Eq context => InitCtxRef -> View context props model Action
timeControl ctxRef = vfrag [ mount_ $ TC.app ctxRef ]

grid :: Eq context => InitCtxRef -> Catalog.Props -> View context props model action
grid ctxRef props =
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ mountWithProps props $ Inf.app (Catalog.app ctxRef) "catalog"
            ]
        ]


search :: Eq context => View context props model Action
search = div_ [ key_ ("search" :: MisoString) ] [ mount_ Search.app ]


pageWrapperWithDefaults :: Eq context => InitCtxRef -> Model -> View context props model Action -> View context props model Action
pageWrapperWithDefaults ctxRef m inner_content =
    vfrag
        [ mountWithProps clientProps Client.app
        , mountWithProps (Thread.Props (admin m) (media_root_ m)) DIP.app
        -- , pre_ [] [ text $ "between_pages: " <> if between_pages then "True" else "False" ]
        , mountWithProps (Nav.Props (all_sites_and_boards m) (current_uri m)) $ Nav.app ctxRef
        , div_ [ class_ "page-inner-content" ] [ inner_content ]
        , Nav.supportingSvgs
        ]

    where
        clientProps :: Client.Props
        clientProps = Client.Props (pg_api_root m)


commonCatalogView :: Eq context => InitCtxRef -> Model -> View context props model Action
commonCatalogView ctxRef m = pageWrapperWithDefaults ctxRef m $ vfrag $
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text $ page_title m ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , timeControl ctxRef
    , search
    ]
    ++ if hydrated m || client_mounted m
    then
        [ grid ctxRef (gridPropsFromModel m) ]
    else []

catalogView
    :: Eq context
    => InitCtxRef
    -> Maybe String
    -> Maybe CookieJar
    -> Model
    -> View context props model Action
catalogView ctxRef _ _ = commonCatalogView ctxRef

boardView
    :: Eq context
    => InitCtxRef
    -> a
    -> a
    -> Maybe CookieJar
    -> Model
    -> View context props model Action
boardView ctxRef _ _ _ m = commonCatalogView ctxRef m

searchView
    :: Eq context
    => InitCtxRef
    -> Maybe String
    -> Maybe CookieJar
    -> Model
    -> View context props model Action
searchView ctxRef _ _ m = pageWrapperWithDefaults ctxRef m $ vfrag
    [ div_
        [ class_ "page_heading" ]
        (
            h1_ [] [ text "Search" ]
            :
            if Str.null term then
                []
            else
                [ p_ [] [ text term ] ]
        )
    , search
    , div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ mountWithProps gridProps Grid.app
            ]
        ]
    ]

    where
        term = search_term m
        gridProps = Grid.Props (search_results m) (media_root_ m)


threadView
    :: Eq context
    => InitCtxRef
    -> Text
    -> Text
    -> BoardThreadId
    -> Maybe CookieJar
    -> Model
    -> View context props model Action
threadView ctxRef _site_name _board_pathpart _board_thread_id _cookies m =
    pageWrapperWithDefaults ctxRef m $ vfrag
        [ mountWithProps
            (Thread.Props (admin m) (media_root_ m))
            (Thread.app ctxRef)
        ]


page404 :: View context props model Action
page404 = h1_ [] [ text "404 Not Found" ]


gridPropsFromModel :: Model -> Catalog.Props
gridPropsFromModel m = Catalog.Props
    { Catalog.mediaRoot = media_root_ m
    , Catalog.currentTime = current_time m
    , Catalog.selectedBoards = selected_boards m
    , Catalog.fetchCount = client_fetch_count m
    }
