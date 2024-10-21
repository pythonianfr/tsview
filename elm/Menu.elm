port module Menu exposing
    ( Model
    , Msg(..)
    , buildCmd
    , getMenu
    , getPro
    , initmenu
    , loadMenuData
    , viewMenu
    , updateModel
    )
import Http
import Dict exposing (Dict)
import Html as H
import Html exposing
    ( Html
    , Attribute
    )
import Html.Attributes as A
import Html.Events as HE
import Json.Decode as JD
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Icons exposing (Icon, buildSvg)


port saveMenuData : MenuData -> Cmd msg
port loadMenuData : (String -> msg) -> Sub msg


type alias MenuData =
    { menuIsOpen: Bool }


type alias Model =
    { menuContent : Menu
    , menuModeText : Bool
    , selected: Maybe String
    , icones: Dict String Icon
    , isPro: Bool
    }


type alias Menu = List Section


type alias Section =
    { label: String
    , icone: String
    , links: List Link
    }


type alias Link =
    { label: String
    , icone: String
    , target: String
    , id: String
    , profeature: Bool
    }


type Msg =
    ToggleMenu
    | LoadMenuData String
    | GotMenu (Result Http.Error Menu)
    | GotIcons (Result Http.Error (Dict String Icon))
    | GotPro ( Result Http.Error String )


initmenu self =
    { menuContent = []
    , menuModeText = False
    , icones = Dict.empty
    , selected = Just self
    , isPro = True
    }


-- decoders

menuDecoder: Decoder Menu
menuDecoder = JD.list decodeSection


decodeSection: Decoder Section
decodeSection =
    JD.map3 Section
       ( JD.field "label" JD.string )
       ( JD.field "icone" JD.string )
       ( JD.field "links" ( JD.list decodeLink ) )


decodeLink: Decoder Link
decodeLink =
     JD.succeed Link
        |> required "label" JD.string
        |> required "icone" JD.string
        |> required "target" JD.string
        |> required "id" JD.string
        |> optional "profeature" JD.bool False


getMenu: String -> ((Result Http.Error Menu) -> msg) -> Cmd msg
getMenu baseUrl msgBuilder =
    Http.get
        { url = baseUrl ++ "menu"
        , expect = Http.expectJson msgBuilder menuDecoder
        }

getPro: String -> ((Result Http.Error String) -> msg) -> Cmd msg
getPro baseUrl msgBuilder =
    Http.get
        { url = baseUrl ++ "ispro"
        , expect = Http.expectString msgBuilder
        }


menuDataDecoder: Decoder MenuData
menuDataDecoder =
    JD.map MenuData
        ( JD.field "menuIsOpen" JD.bool )

-- update

updateModel: Msg -> Model -> Model
updateModel msg model =
    case msg of
        ToggleMenu -> { model | menuModeText = not model.menuModeText }
        LoadMenuData content ->
            case JD.decodeString menuDataDecoder content of
                Ok data -> { model | menuModeText = data.menuIsOpen }
                Err _ -> { model | menuModeText = False }
        GotMenu (Ok content) -> { model | menuContent = content }
        GotMenu (Err error) ->  model
        GotIcons (Ok content) -> { model | icones = content }
        GotIcons (Err error) ->  model
        GotPro (Ok _) -> { model | isPro = True }
        GotPro (Err _) -> { model | isPro = False }



buildCmd: Msg -> Model -> Cmd msg
buildCmd msg model =
    case msg of
        ToggleMenu -> saveMenuData (MenuData (not model.menuModeText))
        _ -> Cmd.none

-- view

filterPro: Bool -> Link -> Bool
filterPro isPro link =
    if isPro
        then True
        else not link.profeature


displayTextSection: (Dict String Icon) -> Menu -> Maybe String -> Bool -> Html msg
displayTextSection icones content selected isPro =
    let
        format section =
            H.li
                [ A.class "section"]
                [ H.div
                      [ A.class "section-svg" ]
                      [ buildSvg icones section.icone
                      , H.p
                          [ A.class "label"]
                          [ H.text section.label ]
                      ]
                , displayTextLinks icones section.links selected isPro
                ]
    in
    H.ul [] <| List.map format content


displayTextLinks: (Dict String Icon) -> List Link -> Maybe String -> Bool -> Html msg
displayTextLinks icones links selected isPro =
    let
        format link =
            H.li
                [ A.class "link"
                , classSelect link.id selected
                ]
                [ H.a
                      [ A.href link.target
                      , A.class "full-link"
                      ]
                      [ buildSvg icones link.icone
                      , H.p
                          [ A.class "label" ]
                          [ H.text link.label ]
                      ]
                ]
    in
    H.ul [] <| List.map
                format
                ( List.filter ( filterPro isPro ) links )


displayIconeContent: (Dict String Icon) -> Menu -> Maybe String -> Bool -> Html msg
displayIconeContent icones content selected isPro =
    let
        format section =
            H.li
                [ A.class "section"]
                [ H.div
                      [ A.class "section-svg"
                      , A.title section.label
                      ]
                      [ buildSvg icones section.icone ]
                , displayIconeLinks icones section.links selected isPro
                ]
    in
    H.ul [] <| List.map format content


displayIconeLinks: (Dict String Icon) -> List Link -> Maybe String -> Bool -> Html msg
displayIconeLinks icones links selected isPro =
    let
        format link =
            H.li
                [ A.class "link"
                , classSelect link.id selected
                ]
                [ H.a
                      [ A.href link.target
                      , A.title link.label
                      ]
                      [ buildSvg icones link.icone ]
                ]
    in
    H.ul [] <| List.map
                format
                ( List.filter ( filterPro isPro ) links )


classSelect: String -> Maybe String -> Attribute msg
classSelect label selected =
    case selected of
        Nothing -> A.class ""
        Just select ->
            if select == label
                then A.class "selected"
                else A.class ""


displaSwitchButton: Dict String Icon -> Bool -> Html msg
displaSwitchButton icones toCollpase =
     if toCollpase
        then buildSvg icones "bi bi-arrows-collapse-vertical"
        else buildSvg icones "bi bi-arrows-expand-vertical"


viewMenu: Model -> (Msg -> msg) -> Html msg
viewMenu model msgBuilder =
    H.div
        [ A.class "container-menu"
            , if model.menuModeText
              then A.class "container-with-text"
              else A.class "container-with-icon"]
        [ H.div
            [ A.class "menu-refinery"
            , if model.menuModeText
              then A.class "menu-with-text"
              else A.class "menu-with-icon"
            ]
            [ H.div
                  []
                  [ H.div
                        [ HE.onClick (msgBuilder ToggleMenu)
                        , A.class "switch-display"
                        , A.title "show/hide text"]
                        [ displaSwitchButton model.icones model.menuModeText ]
                  , if model.menuModeText
                    then displayTextSection model.icones model.menuContent model.selected model.isPro
                    else displayIconeContent model.icones model.menuContent model.selected model.isPro
                  ]
            ]
        ]