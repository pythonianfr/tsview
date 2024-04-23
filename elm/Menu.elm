port module Menu exposing
    ( Model
    , Msg(..)
    , buildCmd
    , getMenu
    , getIcons
    , initmenu
    , loadMenuData
    , viewMenu
    , updateModel
    )
import Http
import Dict exposing (Dict)
import Svg exposing (svg, path)
import Svg.Attributes exposing
    ( d
    , fill
    , fillRule
    , viewBox
    )
import Html as H
import Html exposing
    ( Html
    , Attribute
    )
import Html.Attributes as A
import Html.Events as HE
import Json.Decode as JD
import Json.Decode exposing (Decoder)


port saveMenuData : MenuData -> Cmd msg
port loadMenuData : (String -> msg) -> Sub msg


type alias MenuData =
    { menuIsOpen: Bool }


type alias Model =
    { menuContent : Menu
    , menuModeText : Bool
    , selected: Maybe String
    , icones: Dict String Icon
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
    }


type alias Icon = List Path

type alias Path =
    { d: String
    , fillRule: Maybe String
    }


type Msg =
    ToggleMenu
    | LoadMenuData String
    | GotMenu (Result Http.Error Menu)
    | GotIcons (Result Http.Error (Dict String Icon))


initmenu self =
    { menuContent = []
    , menuModeText = False
    , icones = Dict.empty
    , selected = Just self
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
    JD.map4 Link
        ( JD.field "label" JD.string )
        ( JD.field "icone" JD.string )
        ( JD.field "target" JD.string )
        ( JD.field "id" JD.string )


getMenu: String -> ((Result Http.Error Menu) -> msg) -> Cmd msg
getMenu baseUrl msgBuilder =
    Http.get
        { url = baseUrl ++ "menu"
        , expect = Http.expectJson msgBuilder menuDecoder
        }


getIcons: String -> ((Result Http.Error (Dict String Icon)) -> msg) -> Cmd msg
getIcons baseUrl msgBuilder =
    Http.get
        { url = baseUrl ++ "icons"
        , expect = Http.expectJson msgBuilder iconesDecoder
        }


iconesDecoder: Decoder (Dict String Icon)
iconesDecoder = JD.dict decodeIcone


decodeIcone: Decoder Icon
decodeIcone = JD.list decodePath


decodePath: Decoder Path
decodePath =
    JD.map2 Path
        ( JD.field "d" JD.string )
        ( JD.maybe (JD.field "fillRule" JD.string ) )


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


buildCmd: Msg -> Model -> Cmd msg
buildCmd msg model =
    case msg of
        ToggleMenu -> saveMenuData (MenuData (not model.menuModeText))
        LoadMenuData _ -> Cmd.none
        GotMenu (Ok content) -> Cmd.none
        GotMenu (Err error) ->  Cmd.none
        GotIcons (Ok content) -> Cmd.none
        GotIcons (Err error) ->  Cmd.none

-- view

displayTextSection: (Dict String Icon) -> Menu -> Maybe String -> Html msg
displayTextSection icones content selected =
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
                , displayTextLinks icones section.links selected
                ]
    in
    H.ul [] <| List.map format content


displayTextLinks: (Dict String Icon) -> List Link -> Maybe String -> Html msg
displayTextLinks icones links selected =
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
    H.ul [] <| List.map format links


displayIconeContent: (Dict String Icon) -> Menu -> Maybe String -> Html msg
displayIconeContent icones content selected =
    let
        format section =
            H.li
                [ A.class "section"]
                [ H.div
                      [ A.class "section-svg"
                      , A.title section.label
                      ]
                      [ buildSvg icones section.icone ]
                , displayIconeLinks icones section.links selected
                ]
    in
    H.ul [] <| List.map format content


displayIconeLinks: (Dict String Icon) -> List Link -> Maybe String -> Html msg
displayIconeLinks icones links selected =
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
    H.ul [] <| List.map format links


classSelect: String -> Maybe String -> Attribute msg
classSelect label selected =
    case selected of
        Nothing -> A.class ""
        Just select ->
            if select == label
                then A.class "selected"
                else A.class ""


buildSvg: Dict String Icon -> String -> Html msg
buildSvg icones iconeName =
    let
        icone =
            Maybe.withDefault [] <| Dict.get iconeName icones
    in
    svg
        [ viewBox "0 0 16 16"
        , fill "currentColor"
        ]
        <| List.map  (\ipath -> buildSvgPath ipath) icone


buildSvgPath: Path -> Html msg
buildSvgPath ipath =
    case ipath.fillRule of
        Nothing -> path [ d ipath.d ] []
        Just rule -> path [ fillRule rule, d ipath.d ] []


displaSwitchButton: Dict String Icon -> Bool -> Html msg
displaSwitchButton icones toCollpase =
     if toCollpase
        then buildSvg icones "bi bi-arrows-collapse-vertical"
        else buildSvg icones "bi bi-arrows-expand-vertical"


viewMenu: Model -> (Msg -> msg) -> Html msg
viewMenu model msgBuilder =
    H.div
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
                then displayTextSection model.icones model.menuContent model.selected
                else displayIconeContent model.icones model.menuContent model.selected
              ]
        ]
