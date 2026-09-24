{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Admin.Component.DeleteIllegalPost where

import Miso
    ( topic
    , Topic
    , Effect
    , MisoString
    , text
    , View
    , toMisoString
    , vfrag
    , withProps
    )

import qualified Miso as M
import Miso.Html.Property (class_)
import Miso.Html.Element
    ( div_
    , p_
    , span_
    , pre_
    )
import GHC.Generics (Generic)
import Miso.JSON (FromJSON, ToJSON)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as L

import qualified Common.Component.Modal as Modal
import qualified Common.Component.Thread.Model as T
import qualified Common.Network.PostType as P
import Common.Component.PostViews (op, reply)
import qualified Common.Network.ClientTypes as Client
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as B
import qualified Common.Network.ThreadType as T
import Common.FrontEnd.Types (DeletePostResults (..))

#ifdef FRONT_END
import qualified Common.Component.Thread.Types as TT
import qualified Common.Utils as Utils
import JSFFI.MisoFFI
    ( freezeBodyScrolling
    , unFreezeBodyScrolling
    )
import Miso
    ( consoleError
    , consoleLog
    , get
    , io_
    , modify
    , publish
    , subscribe
    )
#endif

data Model = Model
    { threadData :: Maybe T.Model
    , deleteRequestResult :: Maybe (Either MisoString DeletePostResults)
    , busy :: Bool
    }
    deriving Eq

data Action
    = Initialize
    | OnMessageIn InMessage
    | OnErrorMessage MisoString
    | ClientResponse Client.MessageOut
    | Submit
    | Cancel

newtype InMessage = InMessage T.Model
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

deleteIllegalPostInTopic :: Topic InMessage
deleteIllegalPostInTopic = topic "deleteIllegal-in"

pattern ReturnTopic :: Client.ReturnTopicName
pattern ReturnTopic = "delete-illegal-post-results"

type DeleteIllegalPostComponent context props = M.Component context props Model Action

initialModel :: Model
initialModel = Model Nothing Nothing False

app :: DeleteIllegalPostComponent context T.Props
app = M.Component
    { M.model = initialModel
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
    , M.mount = Just Initialize
    , M.unmount = Nothing
    , M.onPropsChanged = Nothing
    , M.useContext = False
    }


update :: Action -> Effect parent props Model Action
#ifdef FRONT_END
update Initialize = do
    io_ $ consoleLog "DeleteIllegalPostComponent Init"
    subscribe deleteIllegalPostInTopic OnMessageIn OnErrorMessage
    subscribe clientReturnTopic ClientResponse OnErrorMessage

    where
        clientReturnTopic :: Topic Client.MessageOut
        clientReturnTopic = topic ReturnTopic

update (OnMessageIn (InMessage x)) = do
    io_ $ consoleLog "DeleteIllegalPostComponent received a message!"
    modify
        ( \m -> m
            { threadData = Just x
            , deleteRequestResult = Nothing
            , busy = False
            }
        )
    io_ freezeBodyScrolling

update (ClientResponse (Client.ReturnResult httpResult)) = do
    Utils.helperE httpResult saveDeleteResult saveDeleteErrorResult

    where
        saveDeleteResult :: DeletePostResults -> Effect context props Model Action
        saveDeleteResult results@DeletePostResults { sites = sites_ } = do
            io_ $ consoleLog $ toMisoString $ show sites_
            modify
                ( \m -> m
                    { deleteRequestResult = Just $ Right results
                    , busy = False
                    }
                )
            io_ $ publish
                TT.threadTopic $
                TT.PostDeleted $ map P.post_id $ postsFromSites sites_

        saveDeleteErrorResult :: MisoString -> Effect context props Model Action
        saveDeleteErrorResult errMsg = do
            io_ $ consoleError errMsg
            modify
                ( \m -> m
                    { deleteRequestResult = Just $ Left errMsg
                    , busy = False
                    }
                )

update (OnErrorMessage e) = io_ $ consoleError e

update Cancel = do
    modify (\m -> m { threadData = Nothing } )
    io_ unFreezeBodyScrolling

update Submit = do
    model <- get

    case busy model of
        True -> return ()
        False ->
            case threadData model of
                Nothing ->
                    io_ $ consoleLog "Error: DeleteIllegalPostComponent has nothing to post!"

                Just threadModel -> do
                    io_ $ do
                        consoleLog "DeleteIllegalPostComponent Submit!"

                        publish Client.clientInTopic
                            ( ReturnTopic
                            , Client.DeleteIllegalPost $
                                Client.DeleteIllegalPostArgs $
                                P.post_id $
                                post threadModel
                            )

                    modify (\m -> m { busy = True })

    where
        post :: T.Model -> P.Post
        post m = fst $ head $ T.post_bodies m

#else
update = undefined
#endif

view :: Model -> View context T.Props model Action
view m = vfrag hide

    where
        hide
            | isJust $ threadData m = [ div_ [ class_ "modal-dialog" ] render ]
            | otherwise = []

        render =
            case threadData m of
                Nothing -> []
                Just x ->
                    [ Modal.view
                        ( Modal.Model
                            { Modal.cancel = Cancel
                            , Modal.submit = Submit
                            , Modal.content = content x
                            , Modal.title = "Delete post and attachments?"
                            , Modal.action = "Delete"
                            }
                        )
                    ]


        statusMessage :: Model -> P.Post -> View context props model action
        statusMessage Model { deleteRequestResult = Nothing } post =
            div_
                [ class_ "warning-message" ]
                [ div_ [ class_ "warning-icon" ] [ "⚠" ]
                , p_ []
                    [ "This will delete "
                    , a post
                    , span_
                        [ class_ "warning-message__strong-word" ]
                        [ "No."
                        , text $ toMisoString $ show $ P.board_post_id post
                        ]
                    , " as well as any other post containing the same attachments as this post:"
                    ]
                ]

        statusMessage Model { deleteRequestResult = Just (Left errMsg) } _ =
            div_
                [ class_ "warning-message warning-message--error" ]
                [ div_ [ class_ "warning-icon" ] [ "⚠" ]
                , p_ []
                    [ "The server responded with an error:"
                    , pre_
                        [ class_ "warning-message__error-message" ]
                        [ text errMsg ]
                    ]
                ]

        statusMessage Model
            { deleteRequestResult = Just (Right (DeletePostResults sites_ (noticerSuccess, noticerFail)))
            } _ =
            div_
                [ class_ "warning-message warning-message--success" ]
                [ p_ [] [ successMessage ]
                , p_ [] [ noticerSummary ]
                ]

            where
                successMessage
                    | null threads =
                            text $ "Successfully removed "
                                <> toMisoString (show $ length posts)
                                <> " posts and " <> toMisoString (show $ length attachments)
                                <> " attachments."
                    | otherwise =
                            text $ "Successfully removed "
                                <> toMisoString (show $ length threads)
                                <> " threads."

                noticerSummary
                    | noticerFail == 0 =
                        if noticerSuccess > 0
                        then
                            text "All offending posts saved to SpamNoticer"
                        else
                            text "SpamNoticer wasn't used."
                    | otherwise = text $ "Only " <> toMisoString noticerSuccess
                        <> " out of " <> (toMisoString $ noticerSuccess + noticerFail)
                        <> " attachments were successfully uploaded to SpamNoticer. Tell your sysadmin to check logs"

                posts = postsFromSites sites_

                attachments = concatMap P.attachments posts

                threads = filter (\x -> P.local_idx x == 1) posts


        content :: T.Model -> View context T.Props model Action
        content threadModel@T.Model { T.post_bodies = (pwb@(post, _):_) } =
            withProps $ \props ->
            div_ [ class_ "modal-dialog__content" ]
                [ statusMessage m post
                , div_ [ class_ "modal-dialog__post-preview" ]
                    (
                        if isOp post
                        then
                            op (const []) props threadModel post Map.empty
                            ++ [ div_ [ class_ "clearfix" ] [] ]
                        else
                            [ reply (const []) props threadModel Map.empty pwb ]
                    )
                ]

        content T.Model { T.post_bodies = [] } =
            div_
                [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "warning-message" ]
                    [ p_ [] [ "Invalid state - no post information!" ]
                    ]
                ]


        isOp :: P.Post -> Bool
        isOp post = P.local_idx post == 1


        a post
            | isOp post = "thread "
            | otherwise = "post "


postsFromSites :: [ Site.Site ] -> [ P.Post ]
postsFromSites
    = concatMap (L.toList . T.posts)
    . concatMap B.threads
    . concatMap (L.toList . Site.boards)
