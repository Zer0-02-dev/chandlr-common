{-# LANGUAGE OverloadedStrings #-}

module Common.Component.PostViews where

import Prelude hiding (head)
import Miso
  ( View
  , text
  , toMisoString, key_
  )
import Miso.Html
  ( div_
  )
import Miso.Html.Property
  ( class_
  , id_
  , classList_
  )
import Data.List.NonEmpty (head)
import qualified Data.List as L

import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import Common.Network.PostType (Post)
import qualified Common.Network.PostType as Post
import qualified Common.Network.BoardType as Board
import Common.Network.BoardType (Board)
import Common.Network.ThreadType (Thread)
import Common.Component.Thread.Files (files)
import Common.Component.Thread.Intro (intro)
import Common.Component.Thread.Embed (embed)
import Common.Component.Thread.Model
import Common.Parsing.BodyParser
import qualified Common.Component.BodyRender as Body


op
    :: (PostWithBody -> [ View ctx props model action ])
    -> Props
    -> Model
    -> Post
    -> Backlinks
    -> [ View ctx props model action ]
op introExtras p m op_post backlinks =
    [ files_or_embed_view
    , div_
        [ classList_
            [ ("post", True)
            , ("op", True)
            , ("post-with-admin", admin p)
            , ("multifile", multi op_post)
            ]
        , id_ (toMisoString $ show $ Post.board_post_id op_post)
        , key_ ("post#" <> show (Post.post_id op_post))
        ]
        ( intro site_ board thread op_post backlinks (current_time m)
        : introExtras (L.head $ post_bodies m)
        ++
        [ div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
        )
    ]

    where
        files_or_embed_view :: View ctx props model a
        files_or_embed_view =
          case Post.embed op_post of
            Just _ -> embed op_post
            Nothing -> files (media_root p) site_ op_post


        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = L.head $ Board.threads board

        body :: [ PostWithBody ] -> [ View ctx props model a ]
        body [] = []
        body (x:_) = Body.render site_ $ snd x


multi :: Post -> Bool
multi post = length (Post.attachments post) > 1


reply
  :: (PostWithBody -> [ View context Props model action ])
  -> Props
  -> Model
  -> Backlinks
  -> PostWithBody
  -> View context Props model action
reply introExtras p m backlinks pwb@(post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    , key_ $ "post#" <> show (Post.post_id post)
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        [ classList_
            [
            ("post", True)
            , ("reply", True)
            , ("post-with-admin", admin p)
            , ("multifile", multi post)
            ]
        ]
        ( intro site_ board thread post backlinks (current_time m)
        : introExtras pwb
        ++
        [ files_or_embed_view
        , div_
            [ class_ "body" ]
            (Body.render site_ parts)
        ]
        )
    ]

    where
        files_or_embed_view :: View ctx props model action
        files_or_embed_view =
          case Post.embed post of
            Just _ -> embed post
            Nothing -> files (media_root p) site_ post

        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = L.head $ Board.threads board
