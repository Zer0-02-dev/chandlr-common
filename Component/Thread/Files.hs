{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.Thread.Files where

import Prelude hiding (take)
import Miso
  ( View
  , Attribute
  , text
  )
import Miso.Html
  ( div_
  , a_
  , img_
  , span_
  , p_
  )
import Miso.Html.Property
  ( href_
  , class_
  , title_
  , alt_
  , src_
  , loading_
  , download_
  , target_
  , data_
  , classList_
  )
import qualified Data.List.NonEmpty as L
import Miso.String (MisoString, append, toMisoString, take)
import qualified Miso.String as Str

import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType as Post
import Common.Network.PostType (Post)
import Common.AttachmentType (Attachment, Dimension (..))
import qualified Common.AttachmentType as Attachment
import Common.Network.Units (bytesToHumanReadable)
import Data.Maybe (fromMaybe)

max_thumbnail_width :: Int
max_thumbnail_width = 255

max_thumbnail_height :: Int
max_thumbnail_height = 255

max_original_filename_display_length :: Int
max_original_filename_display_length = 25

files :: MisoString -> Site -> Post -> View ctx props model a
files media_root site post = div_
  [ class_ "files" ]
  ( map (file media_root site multi) attachments )

  where
    multi = length attachments > 1

    attachments = Post.attachments post

file :: MisoString -> Site -> Bool -> Attachment -> View ctx props model a
file media_root site multifile a = div_
  [ classList_ [ ("file", True), ("multifile", multifile) ] ]
  [ p_
      [ class_ "fileinfo" ]
      [ span_ [] [ "File: " ]
      , a_
          [ href_ file_url
          ] [ text $ toMisoString board_filename]
      , text " "
      , span_
          [ class_ "details" ]
          [ text $ "(" `append` size `append` res_str
          , a_
              [ download_ orig_file_name
              , href_ file_url
              , title_ $ "Save as original filename (" `append` orig_file_name `append` ")"
              ] [ text filename_text ]
          , ")"
          ]
      ]
  , a_
      [ href_ file_url
      , target_ "blank_"
      ]
      [ img_
          (
            [ class_ "post-image"
            , loading_ "lazy"
            , src_ thumb_url
            , alt_ ""
            ] ++ size_style_attr
          )
      ]
  ]

  where
    orig_file_name :: MisoString
    orig_file_name = toMisoString fname

    size :: MisoString
    size = toMisoString $
      bytesToHumanReadable (Attachment.file_size_bytes a) True

    res_str :: MisoString
    res_str = maybe "" show_dimension $ Attachment.resolution a

    show_dimension :: Attachment.Dimension -> MisoString
    show_dimension Attachment.Dimension {..} = toMisoString $
        ", " ++ show width ++ "x" ++ show height ++ ", "

    filename_text :: MisoString
    filename_text
      | (Str.length fname) > max_original_filename_display_length =
          toMisoString (take max_original_filename_display_length fname)
          `append` "…" `append` toMisoString file_ext
      | otherwise = toMisoString fname

    fname :: MisoString
    fname = fromMaybe board_filename $ Attachment.original_filename a

    file_ext :: MisoString
    file_ext = maybe "" ("." <>) $ Attachment.file_extension a

    board_filename :: MisoString
    board_filename = Attachment.board_filename a <> file_ext

    thumb_url :: MisoString
    thumb_url = img_url_path
      `append` "/thumbnail_" `append` toMisoString (Attachment.board_filename a)
      `append` toMisoString  (maybe "" ("." <>) $ Attachment.thumb_extension a)

    file_url :: MisoString
    file_url = img_url_path
      `append` "/" `append` toMisoString (Attachment.board_filename a)
      `append` toMisoString  file_ext

    img_url_path :: MisoString
    img_url_path
      = media_root
      `append` "/" `append` toMisoString (Site.name site)
      `append` "/" `append` toMisoString (Board.pathpart board)
      `append` "/" `append` toMisoString
        (show $ Thread.board_thread_id
            (head (Board.threads board)))

    board :: Board.Board
    board = L.head $ Site.boards site

    size_style_attr :: [ Attribute m a ]
    size_style_attr = concatMap (mk_size_style_attr . thumb_dimensions) $ Attachment.resolution a

    mk_size_style_attr :: Dimension -> [ Attribute m a ]
    mk_size_style_attr Dimension {..} =
        [ data_ "width" (toPx width)
        , data_ "height" (toPx height)
        ]

    toPx :: Int -> MisoString
    toPx i = toMisoString (show i) `append` "px"

    thumb_dimensions :: Dimension -> Dimension
    thumb_dimensions Dimension {..}
      | width > height = Dimension mw (round $ fromIntegral mw / fromIntegral width * (fromIntegral height :: Double))
      | otherwise = Dimension (round $ fromIntegral mh / fromIntegral height * (fromIntegral width :: Double)) mh
      where
          mw = min max_thumbnail_width width
          mh = min max_thumbnail_height height
