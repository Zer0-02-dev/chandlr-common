module Common.FrontEnd.Action where

import Miso.String (MisoString)
import Miso (URI)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import Common.Network.BoardType (Board)
import Common.FrontEnd.Types (InitCtxRef, AppInitCtx, Time)
import qualified Common.Component.Catalog as Ctlg

data Action
    = GetThread Client.GetThreadArgs
    | GridMessage Grid.OutMessage
    | ClientResponse Client.ReturnTopicName Client.MessageOut
    | OnErrorMessage MisoString
    | GoToTime { replaceOrPush :: Bool, newTime :: Time }
    | ChangeURI URI
    | SearchResults Search.MessageOut
    | NotifySearch Search.MessageIn
    | ClientMounted
    | ClientUnmounted
    | ThreadViewMounted
    | CatalogViewMounted
    | NoAction
    | Initialize InitCtxRef
    | InitNoHydration AppInitCtx
    | InitAllSitesAndBoards
    | GoBackToCatalog
    | ReloadGridWithBoards [ Board ]
    | CatalogPaginationMsg Ctlg.CatalogPaginationMessage
    deriving Eq


mkGetThread :: CatalogPost -> Client.GetThreadArgs
mkGetThread post = Client.GetThreadArgs
    { Client.website = CatalogPost.site_name post
    , Client.board_pathpart = CatalogPost.pathpart post
    , Client.board_thread_id = CatalogPost.board_thread_id post
    }
