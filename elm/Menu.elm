module Menu exposing
    (Model
    , Msg(..)
    , Menu
    , Link
    , contentMenu
    , viewMenu
    , updateModel
    )

import Html as H
import Html.Attributes as A
import Html.Events as HE


type alias Model =
    { menuContent : Menu
    , menuVisisble : Bool }

type alias Menu = List Section

type alias Section =
    { label: String
    , links: List Link
    }

type alias Link =
    { label: String
    , target: String }

contentMenu : Menu
contentMenu =
    [ { label = "Timeseries"
      , links = [ { label = "Catalog"
                  , target = "/tssearch" }
                , { label = "Quick View"
                  , target = "/tsview" }
                , { label = "Delete"
                  , target = "/tsdelete" }
                ]
      }
    , { label = "Formula"
      , links = [ { label = "Documentation"
                  , target = "/tsformula/operators" }
                , { label = "Catalog"
                  , target = "/formulas" }
                , { label = "Create"
                  , target = "/tsformula" }
                , { label = "Update batch"
                  , target = "/addformulas" }
                , { label = "Setup cache"
                  , target = "/formulacache" }
                ]
      }
    , { label = "Monitoring"
      , links = [ { label = "Tasks"
                  , target = "/tasks/" }
                , { label = "Series import"
                  , target = "/tswatch/" }
                ]
      }
    ]

type Msg =
    ToggleMenu

-- update

updateModel msg model =
    case msg of
        ToggleMenu ->{ model | menuVisisble = not model.menuVisisble}

-- view
-- Note: the view function must NOT be Typed
--       in this module for modularity purpose

displayContent content =
    H.ul
        []
        ( List.map
            ( \ section -> H.li
                            []
                            [ H.text section.label
                            , displayLinks section.links ] )
            content )

displayLinks links =
        H.ul
        []
        ( List.map
            (\ link -> H.li
                            []
                            [ H.a
                                [ A.href link.target ]
                                [ H.text link.label ] ] )
            links )

viewMenu model msgBuilder =
    H.div
        []
        [ H.button
            [ HE.onClick (msgBuilder ToggleMenu)
            , A.title "show/hide nav"]
            [ H.text "☰" ]
        , H.div
            [ A.hidden ( not model.menuVisisble ) ]
            [ displayContent model.menuContent ]
        ]