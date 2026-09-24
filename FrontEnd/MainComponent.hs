{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.MainComponent where

import Miso
    ( App
    , View
    , LogLevel (DebugAll)
    , uriSub
    , URI
    , emptyURI
    )
import qualified Miso as M
import Miso.JSON (Value)
import Miso.String (toMisoString, MisoString)
import Data.Proxy
import Servant.API hiding (URI)
import Servant.Miso.Router (route)
import Data.IORef (readIORef)
import Data.List.NonEmpty (toList)
import qualified Data.Set as Set

import qualified Common.FrontEnd.JSONSettings as Settings
import Common.Network.CatalogPostType (CatalogPost)
import Common.Component.TimeControl (earliest)
import Common.FrontEnd.Routes
import Common.FrontEnd.Model
import Common.FrontEnd.Views
import Common.FrontEnd.Action
import Common.FrontEnd.Types
import Common.Utils (pageTypeFromURI, PageType (..))
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
#if defined(FRONT_END)
import Common.FrontEnd.Update
#endif

type MainComponent = App Model Action

app :: InitCtxRef -> MainComponent
app ctxRef =
    M.Component
        { M.model         = emptyModel
#if defined(FRONT_END)
        , M.update        = mainUpdate
#else
        , M.update        = undefined
#endif
        , M.hydrateModel  = Just $ initializeModel ctxRef
        , M.view          = mainView ctxRef
        , M.subs          = [ uriSub ChangeURI ]
        , M.styles = []
        , M.mountPoint    = Nothing
        , M.logLevel      = DebugAll
        , M.scripts = []
        , M.mailbox = handleMail
        , M.eventPropagation = False
        , M.mount = Just (Initialize ctxRef)
        , M.unmount = Nothing
        , M.onPropsChanged = Nothing
        , M.useContext = False
        }

    where
        emptyModel :: Model
        emptyModel = Model
            { current_uri = emptyURI
            , media_root_ = ""
            , current_time = Then earliest
            , search_term = ""
            , page_title = "Overboard Catalog"

            , on_client_mounted_initial_actions = []
                -- ^ For no hydration, this will have the initial_action and a getSitesAndBoards message. Otherwise (upon hydration) it's set to empty

            , thread_message = Nothing
            , pg_api_root = ""
            , client_fetch_count = 0
            , search_results = []
            , between_pages = False
            , admin = False
            , client_mounted = False
            , search_mounted = False
            , search_message = Nothing
            , all_sites_and_boards = []
            , hydrated = False
            , sites_and_boards_loaded = False
            , selected_boards = Nothing
            }

        handleMail :: Value -> Maybe Action
        handleMail = M.checkMail actionFromChildMessage OnErrorMessage
            where
                actionFromChildMessage :: MessagesFromChildren -> Action
                actionFromChildMessage MsgClientMounted = ClientMounted
                actionFromChildMessage MsgThreadViewMounted = ThreadViewMounted


initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
    putStrLn "MainComponent initializeModel"
    ctx <- readIORef ctxRef

    let settings = init_settings ctx
    putStrLn $ "MainComponent initializeModel settings: " <> show settings
    let initialPayload = init_payload ctx
    let uri = init_uri ctx
        selectedBoards = (\boardIds ->
            [ board
            | site <- sitesAndBoards initialPayload
            , board <- toList (Site.boards site)
            , Set.member (Board.board_id board) boardIds
            ]) <$> init_board_selection ctx

    return
          Model
              { current_uri = init_uri ctx
              , media_root_ = toMisoString $ Settings.media_root settings
              , current_time = timeFromInitialPayload ctx
              , search_term = searchTermFromUri uri
              , page_title = "Overboard Catalog"
              , on_client_mounted_initial_actions = []
              , thread_message = Nothing
              , pg_api_root = toMisoString $ Settings.postgrest_url settings
              , client_fetch_count = Settings.postgrest_fetch_count settings
              , search_results = initialSearchResults $ initialData initialPayload
              , between_pages = False
              , admin = Settings.admin settings
              -- , initialized = True
              , client_mounted = False
              , search_mounted = False
              , search_message = Nothing
              , all_sites_and_boards = sitesAndBoards initialPayload
              , hydrated = hydrate ctx
              , sites_and_boards_loaded = True
              , selected_boards = selectedBoards
              }

    where
        searchTermFromUri :: URI -> MisoString
        searchTermFromUri u =
            case pageTypeFromURI u of
                Search (Just q) -> toMisoString  q
                _ -> ""

        initialSearchResults :: InitialData -> [ CatalogPost ]
        initialSearchResults (SearchData posts) = posts
        initialSearchResults _ = []


mainView
    :: Eq context
    => InitCtxRef
    -> Model
    -> View context () Model Action
mainView ctxRef model = mainView_
    where
        mainView_ = either (const page404) id $
            route
                (Proxy :: Proxy (Route (View context props model Action)))
                handlers
                current_uri
                model

        handlers
            =    catalogView ctxRef
            :<|> threadView  ctxRef
            :<|> boardView   ctxRef
            :<|> searchView  ctxRef
