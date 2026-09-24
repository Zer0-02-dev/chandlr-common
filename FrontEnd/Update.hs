{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Update where

import Miso
    ( URI (..)
    , Effect
    , publish
    , consoleLog
    , consoleError
    , io_
    , io
    , get
    , modify
    , issue
    , subscribe
    , View
    , Topic
    , topic
    )
import Miso.Subscription.History (replaceURI, pushURI)
import Servant.Miso.Router (route)
import Miso.String (MisoString, toMisoString)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Either (fromRight)
import Servant.API hiding (URI)
import Network.URI (unEscapeString)
import Data.IORef (modifyIORef, readIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)

import Common.FrontEnd.Action
import Common.FrontEnd.Model
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.Thread as Thread
import qualified Common.Utils as Utils
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import Common.Network.SiteType (fromCatalogPost, Site)
import qualified Common.Network.BoardType as Board
import JSFFI.MisoFFI (encodeURIComponent)
import Common.FrontEnd.Routes
import qualified Common.FrontEnd.Types as T
import qualified Common.Network.CatalogPostType as CatPost
import qualified Common.Component.TimeControl as TC
import qualified Common.FrontEnd.JSONSettings as Settings
import Common.Parsing.FlexibleJsonResponseParser as Flx
import qualified Common.Component.NavigationBar as NavB

-- import JSFFI.Profile (sectionEnd, toJSString, displayTotals)

pattern Sender :: Client.ReturnTopicName
pattern Sender = "main"

pattern SenderThread :: Client.ReturnTopicName
pattern SenderThread = "main-thread"

pattern SitesAndBoards :: Client.ReturnTopicName
pattern SitesAndBoards = "main-sites_and_boards"


mainUpdate :: Action -> Effect context () Model Action
mainUpdate NoAction = return ()
mainUpdate (Initialize ctxRef) = do
    subscribe clientThreadReturnTopic (ClientResponse SenderThread) OnErrorMessage
    subscribe clientSitesAndBoardsTopic (ClientResponse SitesAndBoards) OnErrorMessage
    subscribe Grid.catalogOutTopic GridMessage OnErrorMessage
    subscribe Search.searchOutTopic SearchResults OnErrorMessage
    subscribe TC.timeControlTopic (GoToTime False . timeFromTimeMessage) OnErrorMessage
    subscribe NavB.navigationBarTopic actionFromNavBarMessage OnErrorMessage

    model <- get

    io_ $ do
        consoleLog "MainComponent Initialize action"
        consoleLog $ "MainComponent pg_api_root: " <> pg_api_root model

    io $ do
        ctx <- liftIO $ readIORef ctxRef
        if T.hydrate ctx then
            if client_mounted model then
                return ClientMounted
            else
                return NoAction
        else do
            consoleLog "hydrate off, calling InitNoHydration"
            return $ InitNoHydration ctx

    -- collect garbage
    io_ $ liftIO $ modifyIORef ctxRef
        ( \ctx@T.AppInitCtx {..} -> ctx
            { T.init_payload =
                init_payload { T.initialData = T.Nil }
            }
        )

    where
        clientThreadReturnTopic :: Topic Client.MessageOut
        clientThreadReturnTopic = topic SenderThread

        clientSitesAndBoardsTopic :: Topic Client.MessageOut
        clientSitesAndBoardsTopic = topic SitesAndBoards

        timeFromTimeMessage :: TC.Message -> T.Time
        timeFromTimeMessage (TC.Message True  t) = T.Now t
        timeFromTimeMessage (TC.Message False t) = T.Then t

        actionFromNavBarMessage :: NavB.OutMessage -> Action
        actionFromNavBarMessage NavB.GoToCatalog = GoBackToCatalog
        actionFromNavBarMessage (NavB.SelectedBoardsChanged boards) =
            ReloadGridWithBoards boards


mainUpdate (InitNoHydration ctx) = do
    io_ $ consoleLog $ "InitNoHydration - initializing model from settings: "
        <> toMisoString (show settings)

    modify $ \m -> m
        { current_uri = uri
        , media_root_ = toMisoString $ Settings.media_root settings
        , current_time = current_time_
        , search_term = searchTermFromUri uri
        , on_client_mounted_initial_actions = [ InitAllSitesAndBoards ]
        , thread_message = Nothing
        , pg_api_root = toMisoString $ Settings.postgrest_url settings
        , client_fetch_count = Settings.postgrest_fetch_count settings
        , between_pages = False
        , admin = Settings.admin settings
        }

    model <- get

    when (client_mounted model) $ issue ClientMounted

    where
        uri = T.init_uri ctx
        settings = T.init_settings ctx
        current_time_ = T.timeFromInitialPayload ctx

        searchTermFromUri :: URI -> MisoString
        searchTermFromUri u =
            case Utils.pageTypeFromURI u of
                Utils.Search (Just q) -> toMisoString  q
                _ -> ""

-- This may also come from the mailbox, see MainComponent handleMail
mainUpdate ClientMounted = do
    io_ $ consoleLog "MainComponent - ClientMounted"
    model <- get

    mapM_ issue $ on_client_mounted_initial_actions model

    modify $ \m -> m
        { on_client_mounted_initial_actions = []
        , client_mounted = True
        }

mainUpdate InitAllSitesAndBoards = do
    io_ $ do
        consoleLog "Update - InitAllSitesAndBoards"
        publish Client.clientInTopic (SitesAndBoards, Client.LoadAllSitesAndBoards)

mainUpdate ClientUnmounted = io_ $ consoleLog "Http Client Unmounted!"

mainUpdate CatalogViewMounted = do
    io_ $ do
        consoleLog "CatalogViewMounted"
        -- liftIO $ sectionEnd $ toJSString "pageLoad"
        -- liftIO $ displayTotals

mainUpdate ThreadViewMounted = do
    io_ $ do
        consoleLog "ThreadViewMounted"
        -- liftIO $ sectionEnd $ toJSString "pageLoad"
        -- liftIO $ displayTotals

    model <- get

    io_ $ maybe
        (consoleLog "No thread_message available for sending in Main Model")
        (publish Thread.threadTopic)
        (thread_message model)

mainUpdate (GridMessage (Grid.SelectThread catalog_post)) = do
    io_ $ consoleLog "GridMessage!"

    modify
        ( \m -> m
            { thread_message = Just $
                Thread.RenderSite (media_root_ m) (fromCatalogPost catalog_post)
            , between_pages = True
            }
        )

    issue $ GetThread $ mkGetThread catalog_post

    model <- get

    io_ $ do
        consoleLog $ "calling pushURI on " <> toMisoString (show (new_current_uri model))
        pushURI $ new_current_uri model

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = CatPost.site_name catalog_post
                    </> CatPost.pathpart catalog_post
                    </> toMisoString (show $ CatPost.board_thread_id catalog_post)
            , uriFragment = ""
            , uriQueryString = Map.empty
            }


mainUpdate (OnErrorMessage msg) =
    io_ $ consoleError ("Main Component OnErrorMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse SenderThread (Client.ReturnResult result)) = do
    io_ $ consoleLog $ SenderThread <> " - Has result. Storing result in model."

    Utils.helper result $ \sites -> do
        modify
            ( \m -> m
                { thread_message = Just $
                    Thread.RenderSite (media_root_ m) (head sites)
                }
            )
        issue ThreadViewMounted

mainUpdate (ClientResponse SitesAndBoards (Client.ReturnResult result)) = do
    io_ $ consoleLog $ SitesAndBoards <> " - Has result. Storing result in model."

    Utils.helper result $ \sites ->
        modify (\m -> m
            { all_sites_and_boards = sitesFromSSites sites
            , sites_and_boards_loaded = True
            })

mainUpdate (ClientResponse _ (Client.ReturnResult _)) = return ()

mainUpdate (GoToTime r time) = do
    modify (\m -> m { current_time = time, between_pages = True })
    model <- get

    io_ $ do
        consoleLog "GoToTime Now"
        consoleLog $ toMisoString $ show (map Board.board_id <$> selected_boards model)

        -- publish
        --     Client.clientInTopic
        --     ( SenderLatest
        --     , Client.FetchLatest t (map Board.board_id <$> selected_boards model)
        --     )

            -- here we need to inform Catalog to reload the catalog
            -- technically we can use props for this and watch for changed props in catalog
            -- or should I send Catalog a message?
            --      Catalog changes when:
            --          - Time changes
            --          - selected_boards changes
            --          - page scroll (but that comes from a message)

        consoleLog $ "calling replaceURI on " <> toMisoString (show (new_current_uri model))
        (if r then replaceURI else pushURI) $ new_current_uri model

    where
        new_current_uri :: Model -> URI
        new_current_uri m =
            case time of
                T.Now _ ->
                    (current_uri m)
                        { uriQueryString = Map.empty
                        , uriFragment = ""
                        }
                T.Then t ->
                    (current_uri m)
                        { uriQueryString = Map.fromList [ ("t", Just $ toMisoString $ show t) ]
                        }

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "GetThread thread " <> (toMisoString $ show board_thread_id)

    modify (\m -> m { between_pages = True })

    io_ $ publish Client.clientInTopic (SenderThread, Client.GetThread Client.GetThreadArgs {..})

mainUpdate (ChangeURI uri) = do
    modify (\m -> m { current_uri = uri })
    io_ $ consoleLog $ "ChangeURI! " <> (toMisoString $ show uri)
    model <- get

    if not $ between_pages model then do
        io_ $ consoleLog $ "Not between pages, issuing initialAction, between_pages: " <> (toMisoString $ show $ (between_pages model))
        issue $ initialActionFromRoute model uri
    else do
        io_ $ consoleLog "Between pages."
        modify (\m -> m { between_pages = False })

mainUpdate (SearchResults Search.Mounted) = do
    io_ $ consoleLog "main Update - Search mounted"
    model <- get

    case search_message model of
        Nothing -> do
            io_ $ consoleLog "no messages to send to search component."
            modify (\m -> m { search_mounted = True })

        Just x -> do
            io_ $ do
                consoleLog "sending queued up message to search component"
                publish Search.searchInTopic x
            modify (\m -> m { search_mounted = True, search_message = Nothing })

mainUpdate (SearchResults Search.UnMounted) = do
    io_ $ consoleLog "main Update - Search unmounted"
    modify (\m -> m { search_mounted = False })

mainUpdate (SearchResults (Search.SearchResults (intendPushUri, searchTerm, results))) = do
    io_ $ consoleLog $ "MainComponent - SearchResults. intendPushUri: " <> (toMisoString $ show intendPushUri) <> ", searchTerm: " <> searchTerm <> ", number of results: " <> (toMisoString $ show $ length results)
    modify (\m -> m
        { search_results = results
        , between_pages = intendPushUri
        , search_term = searchTerm })

    model <- get

    when intendPushUri $ io_ $ do
        consoleLog $ "Old URI:" <> (toMisoString $ show $ current_uri model) <> " searchTerm: " <> searchTerm
        searchTermURIComponent <- encodeURIComponent searchTerm
        let new_uri = newCurrentURI model searchTermURIComponent
        consoleLog $ "SearchResults pushURI new uri: " <> (toMisoString $ show new_uri)
        pushURI new_uri

    where
        newCurrentURI :: Model -> MisoString -> URI
        newCurrentURI m searchTermURIComponent = (current_uri m)
            { uriPath = "search"
            , uriQueryString = Map.singleton
                "q"
                $ Just searchTermURIComponent
            }

mainUpdate (NotifySearch (b, searchTerm)) = do
    io_ $ consoleLog $ "NotifySearch " <> searchTerm
    modify (\m -> m { search_term = searchTerm })

    model <- get

    let msg = (b, searchTerm)

    when (search_mounted model) $
        io_ $ do
            consoleLog "NotifySearch - publishing message to searchInTopic"
            publish Search.searchInTopic msg

    modify (\m -> m { search_message = Just msg })

mainUpdate GoBackToCatalog = do
    io_ $ consoleLog "GoBackToCatalog"

    model <- get

    modify $ \m -> m { current_uri = let c = current_uri m in c { Miso.uriPath = "" } }

    case current_time model of
        T.Now _ -> do
            io $ do
                consoleLog "GoBackToCatalog Now"
                t <- getCurrentTime
                pure $ GoToTime False $ T.Now t
        x -> do
            io_ $ consoleLog "GoBackToCatalog Then"
            issue $ GoToTime False x

mainUpdate (ReloadGridWithBoards boards) = do
    io_ $ consoleLog "ReloadGridWithBoards"
    modify $ \m -> m { selected_boards = Just boards }
    model <- get
    issue $ GoToTime True (current_time model)

(</>) :: MisoString -> MisoString -> MisoString
(</>) a b = a <> "/" <> b


initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = fromRight NoAction routing_result
    where
        routing_result =
            route
                (Proxy :: Proxy (Route (View context props Model Action)))
                handlers
                (const uri)
                model

        handlers = h_latest :<|> h_thread :<|> h_board :<|> h_search

        h_latest :: Maybe String -> c -> Model -> Action
        h_latest Nothing _ m = GoToTime True $ current_time m
        h_latest (Just t) _ _ = GoToTime True $ T.Then $ read t

        h_thread :: Text -> Text -> BoardThreadId -> c -> Model -> Action
        h_thread website board_pathpart board_thread_id _ _ =
            GetThread Client.GetThreadArgs
                { Client.website = toMisoString website
                , Client.board_pathpart = toMisoString board_pathpart
                , Client.board_thread_id = board_thread_id
                }

        h_search :: Maybe String -> c -> Model -> Action
        h_search Nothing _ m = GoToTime True $ current_time m
        h_search (Just search_query) _ _ = NotifySearch (False, unescaped_search_query)
            where
                unescaped_search_query =
                    toMisoString $ unEscapeString $ search_query

        h_board :: Text -> Text -> c -> Model -> Action
        h_board _website _board_pathpart _ _ = undefined


siteFromSSite :: Flx.SSite -> Site
siteFromSSite (SSite s) = s


sitesFromSSites :: [ Flx.SSite ] -> [ Site ]
sitesFromSSites = map siteFromSSite
