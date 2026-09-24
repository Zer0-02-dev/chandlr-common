{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.BodyRender where

import Miso
    ( text
    , View
    )
import Miso.Html
    ( a_
    , br_
    , span_
    , strong_
    , u_
    , em_
    , s_
    , small_
    , pre_
    )
import Miso.Html.Property
    ( href_
    , target_
    , class_
    )
import Miso.String (toMisoString, MisoString, replace)
import System.FilePath ((</>))
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as L

import Common.Parsing.PostPartType (PostPart (..))
import Common.Parsing.QuoteLinkParser
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType as Post
import Common.Component.Thread.Model (PostWithBody)
import Common.Parsing.BodyParser (parsePostBody)

{-
 - This is the inverse of parsePostBody from BodyParser except
 - that the output is a miso View and not Text. It might be
 - worth trying to use quickcheck to verify the bijection between
 - Text and DOM by creating random PostParts and seeing if 
 - ((parsePostBody . render) parts == parts)
 -
 - (is there an easy way to render a miso View?, that's what's missing
 - a f :: View a -> Text)
 -}
render :: Site.Site -> [ PostPart ] -> [ View ctx props model a ]
render = map . renderPostPart

renderPostPart :: Site.Site -> PostPart -> View ctx props model a
renderPostPart _ (SimpleText txt) = text $ getRidOfCarriageReturn $ toMisoString txt
renderPostPart _ (PostedUrl u) =
    a_
        [ href_ u
        , target_ "_blank"
        ]
        [ text u ]

renderPostPart _ Skip = br_ []

renderPostPart site (Quote parse_result) = elems parse_result
    where
        elems :: Either UrlParseError ParsedURL -> View ctx props model a
        elems (Left err) =
            a_
                []
                [ text $ toMisoString err ]

        elems (Right p) =
            case full_url p of
                Nothing ->
                    a_
                        [ href_ $ "/" <> site_name <> "/" <> linked_board <> "/" ]
                        [ text $ ">>>/" <> linked_board <> "/" ]

                Just u ->
                    if current_board /= linked_board
                    then
                        a_
                            [ href_ u ]
                            [ text $ ">>>/" <> linked_board <> "/" <> post_id ]
                    else
                        a_
                            [ href_ u ]
                            $
                            (text $ ">>" <> post_id)
                            :
                            if pid == op_id
                            then [ small_ [] [ " (OP)" ] ]
                            else []

            where
                linked_board = toMisoString $ boardName p

                pid = fromJust $ postId p

                post_id = toMisoString $ show pid

                current_board = toMisoString $ Board.pathpart $ L.head $ Site.boards site

                op_id = Post.board_post_id $ L.head $ Thread.posts $ head $ Board.threads $ L.head $ Site.boards site


        full_url :: ParsedURL -> Maybe MisoString
        full_url ParsedURL {..} = do
            tid <- threadId
            pid <- postId

            return $ "/" <> site_name <> "/" <> (toMisoString $ boardName </> show tid ++ "#" ++ show pid)

        site_name = toMisoString $ Site.name site

        -- cases of urls:
        -- url:
        -- /b/res/1.html#2
        -- if on different board:
        -- >>/b/2
        -- if on same board or same thread:
        -- >>2
        --
        -- url:
        -- /b/index.html
        -- if only board:
        -- >>>/b/

renderPostPart site (GreenText parts) =
    span_ [ class_ "quote" ] (render site parts)

renderPostPart site (OrangeText parts) =
    span_ [ class_ "orangeQuote" ] (render site parts)

renderPostPart site (RedText parts) =
    span_ [ class_ "heading" ] (render site parts)

renderPostPart site (Spoiler parts) =
    span_ [ class_ "spoiler" ] (render site parts)

renderPostPart site (Bold parts) =
    strong_ [] (render site parts)

renderPostPart site (Underlined parts) =
    u_ [] (render site parts)

renderPostPart site (Italics parts) =
    em_ [] (render site parts)

renderPostPart site (Strikethrough parts) =
    s_ [] (render site parts)

renderPostPart site (Code parts) =
    pre_ [ class_ "code" ] (render site parts)


getPostWithBodies :: Site.Site -> [ PostWithBody ]
getPostWithBodies s = zip posts bodies

    where
        bodies :: [[ PostPart ]]
        bodies = map (getBody . Post.body) posts

        getBody :: Maybe MisoString -> [ PostPart ]
        getBody Nothing = []
        getBody (Just b) = parsePostBody b

        posts :: [ Post.Post ]
        posts = L.toList $ Thread.posts $ head $ Board.threads $ L.head $ Site.boards s


getRidOfCarriageReturn :: MisoString -> MisoString
getRidOfCarriageReturn = (replace "\r" "\n") . (replace "\r\n" "\n")
