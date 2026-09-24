{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.NavMenu where

import Miso
    ( View
    , MisoString
    , text
    , vfrag
    , toMisoString
    , Attribute
    )
import Miso.Html hiding (style_)
import Miso.Html.Property hiding (label_)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as L

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Component.Modal as Modal
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board

navmenu :: Props -> Model -> View context Props Model Action
navmenu _ Model { menuState = Closed } = vfrag []
navmenu props m = div_ [ class_ "modal-dialog" ]
    [ Modal.view
        Modal.Model
            { Modal.cancel = CancelMenu
            , Modal.submit = case menuState m of
                ChooseSites -> ClickBoards
                _           -> SubmitMenuChoice
            , Modal.content = content props m
            , Modal.title = title m
            , Modal.action = "Apply"
            }
    ]
    where
        content :: Props -> Model -> View context Props Model Action
        content _ Model { menuState = Closed } = vfrag []
        content p m_@Model { menuState = ChooseBoards } = div_
                [ class_ "modal-dialog__content" ]
                [ chooseBoards p m_ ]
        content p m_@Model { menuState = ChooseSites } = div_
                [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "modal-dialog__inline-content" ] allOrNoneSites
                , chooseSites p m_
                ]

        title :: Model -> MisoString
        title Model { menuState = ChooseSites } = "Choose Sites"
        title Model { menuState = ChooseBoards } = "Choose Boards"
        title _ = ""


allOrNoneSites :: [ View context props model Action ]
allOrNoneSites =
    [ div_
        [ class_ "button"
        , onClick SelectAllSites
        ] [ span_ [] [ "All" ] ]
    , div_
        [ class_ "button"
        , onClick SelectNoSites
        ] [ span_ [] [ "None" ] ]
    ]

allOrNoneBoards :: Site.Site -> [ View context props model Action ]
allOrNoneBoards s =
    [ div_ [ class_ "button", onClick $ AddFromSite s ] [ span_ [] [ "All" ] ]
    , div_ [ class_ "button", onClick $ RemoveFromSite s ] [ span_ [] [ "None" ] ]
    ]


chooseBoards ::  Props -> Model -> View context Props Model Action
chooseBoards props model
    | currentSites model == emptyCurrentSites =
        button_
            [ class_ "modal-dialog__button"
            , onClick ClickSites
            ] [ text "Select sites" ]
    | otherwise = vfrag $
        [ siteBoardsSection model (site, L.toList (Site.boards site))
        | site <- sites
        ]

    where
        sites :: [ Site.Site ]
        sites =
            case currentSites model of
                All -> sitesAndBoards props
                CurrentSites selectedSites -> Set.toList selectedSites

        siteBoardsSection :: Model -> (Site.Site, [ Board.Board ]) -> View context props model Action
        siteBoardsSection m (s, bs) = vfrag
            [ div_ [ class_ "modal-dialog__inline-content" ]
                (h2_ [] [ text $ Site.name s ] : allOrNoneBoards s)
            , div_ [ class_ "modal-dialog__grid-column-content" ] (map (pickBoard m) bs)
            ]

        pickBoard :: Model -> Board.Board -> View context props model Action
        pickBoard
            m b = div_
                classes
                [ input_
                    [ name_ "include-board"
                    , id_ ident
                    , type_ "checkbox"
                    , onChange $ const $ ToggleBoard b
                    , checked_ selected
                    ]
                , label_
                    [ for_ ident ]
                    [ text $ "/" <> Board.pathpart b <> "/"
                    ]
                ]

            where
                ident :: MisoString
                ident = "board-" <> toMisoString (Board.board_id b)

                selected :: Bool
                selected = Set.member b (selectedBoards m)

                classes :: [ Attribute m a ]
                classes
                    | selected = class_ "site-choice__selected" : boardChoiceClass
                    | otherwise = boardChoiceClass

                boardChoiceClass :: [ Attribute m a ]
                boardChoiceClass = [ class_ "site-choice", class_ "site-choice__board" ]


chooseSites :: Props -> Model -> View context Props Model Action
chooseSites props model = vfrag $ map pickSite (sitesAndBoards props)
    where
        pickSite :: Site.Site -> View context props model Action
        pickSite s =
            div_
                classes
                [ input_
                    [ name_ "include-site"
                    , id_ ident
                    , type_ "checkbox"
                    , onChange $ const $ ToggleSite s
                    , checked_ selected
                    ]
                , label_ [ for_ ident ] [ text $ Site.name s ]
                ]

            where
                ident :: MisoString
                ident = "site-" <> toMisoString (Site.site_id s)

                selected :: Bool
                selected =
                    case currentSites model of
                        All -> True
                        CurrentSites sitesSet -> Set.member s sitesSet

                classes :: [ Attribute m a ]
                classes
                    | selected = class_ "site-choice__selected" : siteChoiceClass
                    | otherwise = siteChoiceClass

                siteChoiceClass :: [ Attribute m a ]
                siteChoiceClass = [ class_ "site-choice" ]
