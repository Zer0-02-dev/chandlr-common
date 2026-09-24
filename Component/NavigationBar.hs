{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module Common.Component.NavigationBar where

import Miso
    ( Component
    , component
    , Effect
    , io_
    , View
    , modify
    , vfrag
    , URI
    , Topic
    , topic
    , publish
    , hydrateModel
    , getProps
    , withProps
    )

import Miso.JSON (FromJSON, ToJSON)
import qualified Data.Set as Set
import GHC.Generics
import Data.IORef (readIORef)

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.View
import Common.Component.NavigationBar.Model
import Common.Component.NavigationBar.NavMenu
import qualified Common.Network.BoardType as Board
import Common.Utils
import qualified Common.FrontEnd.Types as T
import Data.List.NonEmpty (toList)
import qualified Common.Network.SiteType as Site
#ifdef FRONT_END
import JSFFI.MisoFFI (deleteCookie, setCookie)
import Common.BitField
import Control.Monad (void)
import Miso
    ( Component (mount)
    , consoleLog
    , issue
    , toMisoString
    , io
    , get
    )
import Utils (getSelectedBoardIdsFromCookie, boardsSelCookieName)
#endif

initialModel :: Model
initialModel = Model
  { menuState = Closed
  , currentSites = CurrentSites Set.empty
  , selectedBoards = Set.empty
  , allBoardsSelected = True
  , hydrate = False
  }

app :: T.InitCtxRef -> Component context Props Model Action
#ifndef FRONT_END
app ctxRef = (component initialModel undefined view)
    { hydrateModel = Just $ initializeModel ctxRef }
#else
app ctxRef = (component initialModel update view)
    { mount = Just Initialize
    , hydrateModel = Just $ initializeModel ctxRef
    }

update :: Action -> Effect context Props Model Action
update Initialize = do
    model <- get

    if not (hydrate model)
    then
        io $ do
            consoleLog "NavigationBar Initialize"
            mBoardIds <- getSelectedBoardIdsFromCookie

            case mBoardIds of
                Nothing -> return Noop
                Just b -> return $ InitSelectedSites b
    else
        return ()

update Noop = return ()

update (InitSelectedSites boardIds) = do
    props <- getProps

    modify $ \m ->
        setModelBoardSelection (sitesAndBoards props) m boardIds

update ClickSites = do
    io_ $ consoleLog "Choose Sites Clicked!"
    changeMenuStateOrNavigate ChooseSites

update ClickBoards = do
    io_ $ consoleLog "Choose Boards Clicked!"
    changeMenuStateOrNavigate ChooseBoards

update SubmitMenuChoice = do
    io_ $ consoleLog "Submit Menu Choice!"
    modify $ \m -> m { menuState = Closed }

update CancelMenu = do
    io_ $ consoleLog "CloseMenu!"
    modify $ \m -> m { menuState = Closed }

update (ToggleSite s) = do
    io_ $ consoleLog "toggle site!"
    model <- get
    props <- getProps
    case currentSites model of
        All -> do
            let
                allSites = Set.fromList $ sitesAndBoards props
                withoutS = Set.delete s allSites

            modify $ \m -> m
                { currentSites = CurrentSites withoutS
                , allBoardsSelected = False
                }
            issue $ RemoveFromSite s

        CurrentSites selectedSites ->
            if Set.member s selectedSites
            then do
                let withoutS = Set.delete s selectedSites
                modify $ \m -> m
                    { currentSites = CurrentSites withoutS
                    , allBoardsSelected = False
                    }
                issue $ RemoveFromSite s
            else do
                let withS = Set.insert s selectedSites
                modify $ \m -> m
                    { currentSites = CurrentSites withS
                    , allBoardsSelected = False
                    }
                issue $ AddFromSite s

update (ToggleBoard b) = do
    io_ $ consoleLog $ "toggle board" <> Board.pathpart b

    modify $ \m ->
        if Set.member b (selectedBoards m)
        then
            m
                { selectedBoards = Set.delete b (selectedBoards m)
                , allBoardsSelected = False
                }
        else
            m
                { selectedBoards = Set.insert b (selectedBoards m)
                , allBoardsSelected = False -- this is kinda lazy, we should check if all the boards are actually selected maybe
                }

    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update (AddFromSite s) = do
    io_ $ consoleLog $ "AddFromSite " <> Site.name s
    modify $ \m -> m
        { selectedBoards = selectedBoards m
            `Set.union` Set.fromList (toList $ Site.boards s)
        , allBoardsSelected = False
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update (RemoveFromSite s) = do
    io_ $ consoleLog $ "RemoveFromSite " <> Site.name s
    modify $ \m -> m
        { selectedBoards = selectedBoards m
            `Set.difference` Set.fromList (toList $ Site.boards s)
        , allBoardsSelected = False
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update SelectAllSites = do
    props <- getProps

    modify $ \m -> m
        { currentSites = CurrentSites (Set.fromList $ sitesAndBoards props)
        , selectedBoards = Set.fromList $
            concatMap (toList . Site.boards) $ sitesAndBoards props
        , allBoardsSelected = True
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update SelectNoSites = do
  modify $ \m -> m
      { currentSites = emptyCurrentSites
      , selectedBoards = Set.empty
      }
  issue ReloadCatalogGridBecauseSelectedBoardsChanged

update ReloadCatalogGridBecauseSelectedBoardsChanged = do
    model <- get

    io_ $ do
        consoleLog "ReloadCatalogGridBecauseSelectedBoardsChanged"
        publish navigationBarTopic
            (SelectedBoardsChanged $ Set.toList $ selectedBoards model)

        if allBoardsSelected model
        then
            void $ deleteCookie boardsSelCookieName
        else
            let cookieval = toMisoString $ show $
                    bitFieldFromInts $ Set.map Board.board_id $ selectedBoards model
            in void $ setCookie boardsSelCookieName cookieval
#endif

setModelBoardSelection :: [ Site.Site ] -> Model -> Set.Set Int -> Model
setModelBoardSelection sitesAndBoards_ m boardIds =
    let
        (sites, boards) =
            foldr step (Set.empty, Set.empty) sitesAndBoards_
    in
        m
            { currentSites = CurrentSites sites
            , selectedBoards = boards
            , allBoardsSelected = False
            }

    where
        step s (ss, bs) =
            let ms = filter
                    ((`Set.member` boardIds) . Board.board_id)
                    (toList $ Site.boards s)
            in if null ms
                then (ss, bs)
                else
                    ( Set.insert s ss
                    , bs `Set.union` Set.fromList ms
                    )


initializeModel :: T.InitCtxRef -> IO Model
initializeModel ctxRef = do
    ctx <- readIORef ctxRef

    let
        sitesAndBoards_ = T.sitesAndBoards $ T.init_payload ctx

        model = initialModel { hydrate = T.hydrate ctx }

    return $ case T.init_board_selection ctx of
        Nothing -> model
        Just boardIds -> setModelBoardSelection sitesAndBoards_ model boardIds


view :: Model -> View context Props Model Action
view m = withProps $ \p -> vfrag
    [ navmenu p m
    , navbar p m
    ]

shouldNavigateBackToCatalog :: URI -> Bool
shouldNavigateBackToCatalog u
    | pageTypeFromURI u == Catalog = False
    | otherwise                    = True


changeMenuStateOrNavigate :: MenuState -> Effect parent Props Model Action
changeMenuStateOrNavigate newstate = do
    props <- getProps

    if shouldNavigateBackToCatalog (currentUri props)
    then
        io_ $ publish navigationBarTopic GoToCatalog
    else
        modify $ \m -> m { menuState = newstate }


data OutMessage
    = GoToCatalog
    | SelectedBoardsChanged [ Board.Board ]
    deriving (Generic, FromJSON, ToJSON)

navigationBarTopic :: Topic OutMessage
navigationBarTopic = topic "navigation-bar"
