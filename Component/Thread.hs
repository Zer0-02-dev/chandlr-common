{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.Thread
( Model (..)
, PostWithBody
, Action (..)
, update
, view
, app
, ThreadComponent
, Message (..)
, threadTopic
) where

import Prelude hiding (head)
import Miso
  ( View
  , Effect
  , text
  , Component
  , io
  , io_
  , consoleError
  , consoleLog
  , subscribe
  , publish
  , get
  , mailParent
  , withProps
  )
import Miso.Html
  ( div_
  , h1_
  , h2_
  )
import Miso.Html.Property
  ( class_
  )
import qualified Miso as M
import Data.List.NonEmpty (head)
import Miso.String (toMisoString, MisoString)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.IORef (readIORef)
import qualified Data.Set as Set

import qualified Common.Network.SiteType as Site
import Common.Network.PostType (Post (post_id))
import qualified Common.Network.BoardType as Board
import Common.Component.Thread.Model
import Common.Parsing.BodyParser
import qualified Common.Component.BodyRender as Body
import Common.Component.Thread.Types
--import qualified Common.FrontEnd.JSONSettings as Settings
import Common.FrontEnd.Types
import qualified Common.Admin.Component.DeleteIllegalPost as DIP
import Common.Admin.DeleteBtn (deleteBtn)
import Common.Component.PostViews (op, reply)

import Miso.JSON (encode)

type ThreadComponent context = Component context Props Model Action

app :: InitCtxRef -> ThreadComponent context
app ctxRef = M.Component
    { M.model = emptyModel
    , M.hydrateModel = Just $ initializeModel ctxRef
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.styles = []
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.eventPropagation = False
    , M.mount = Just Initialize
    , M.unmount = Nothing
    , M.onPropsChanged = Nothing
    , M.useContext = False
    }

initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
  ctx <- readIORef ctxRef

  -- let settings = init_settings ctx
  let initialPayload = init_payload ctx

  case initialPayload of
    (InitialDataPayload t (ThreadData s pwbs) _) ->
          return Model
            { site = s
            -- , media_root = Settings.media_root settings
            , post_bodies = pwbs
            , current_time = t
            -- , admin = Settings.admin settings
            }

    _ -> return emptyModel


update :: Action -> Effect parent props Model Action
update Initialize = do
    subscribe threadTopic OnMessage OnMessageError
    mailParent MsgThreadViewMounted

update (OnMessage (RenderSite _ s)) = do
    modify (\m -> m { site = s })

    io $ do
        consoleLog "Thread - received RenderSite message"
        let pwbs = Body.getPostWithBodies s
        now <- liftIO $ getCurrentTime
        return $ UpdatePostBodies now pwbs

update (OnMessage (PostDeleted deletedPostIds)) =
    modify (\m@Model {..} -> m { post_bodies = filter ff post_bodies })

    where
        deletedSet = Set.fromList deletedPostIds

        ff :: PostWithBody -> Bool
        ff (p, _) = post_id p `Set.notMember` deletedSet

update (OnMessageError msg) =
    io_ $ consoleError ("Thread Component Message decode failure: " <> toMisoString msg)

update (UpdatePostBodies t pwbs) = do
    io_ $ consoleLog $ "Thread - update UpdatePostBodies case: " <> toMisoString (length pwbs) <> " " <> encode t
    modify (\m -> m { post_bodies = pwbs, current_time = t })

update (OnDeleteBtn pwb) = do
    io_ $ consoleLog "OnDeleteBtn"
    model <- get
    io_ $ publish DIP.deleteIllegalPostInTopic $ DIP.InMessage model { post_bodies = [ pwb ] }


view :: Model -> View context Props Model Action
view = withProps . view_

view_ :: Model -> Props -> View context Props Model Action
view_ m props =
  div_
    []
    [ h1_ [] [ text title ]
    , div_
        [ class_ "thread" ]
        (  op_post (post_bodies m)
        ++ map (reply (deleteBtn_ props) props m backlinks) (drop 1 (post_bodies m))
        )
    ]

    where
        backlinks :: Backlinks
        backlinks = collectBacklinks (post_bodies m)

        op_post :: [ PostWithBody ] -> [ View context Props model Action ]
        op_post [] = [ h2_ [] [ "There's nothing here" ] ]
        op_post ((p, _):_) = op (deleteBtn_ props) props m p backlinks

        title :: MisoString
        title = toMisoString $ Site.name (site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


deleteBtn_
    :: Props
    -> PostWithBody
    -> [ View context Props model Action ]
deleteBtn_ props p
  | admin props = [ deleteBtn (OnDeleteBtn p) ]
  | otherwise = []
