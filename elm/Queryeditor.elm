module Queryeditor exposing ( main )

import Maybe.Extra as Maybe

import Browser
import Dict
import Filter exposing
    ( FilterNode(..)
    , Value(..)
    , fromlisp
    , parse
    , serialize
    )
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import Lisp
import Metadata as M
import Url.Builder as UB
import Util as U
import HtmlExtra as HEX

import Editor.UI.Widget as Widget


type alias Series =
    { name : String
    , imeta : Maybe M.StdMetadata
    , meta : Maybe M.StdMetadata
    , source : String
    , kind : String
    }


type alias BasketFormula =
    { code : String
    , node : FilterNode
    }


type alias Model =
    { baseurl : String
    -- all errors
    , errors : List String
    -- baskets
    , baskets : List String
    -- current basket
    , name : Maybe String
    , basket : Maybe BasketFormula
    , savedBasket : Maybe BasketFormula
    -- creation
    , creating : Bool
    , newname : String
    -- results
    , series : List Series
    -- editor
    , editorExpanded : Bool
    , editorWidget : Widget.Model
    }


type Msg
    = GotBasketNames (Result Http.Error String)
    | SelectedBasket (Maybe String)
    | GotBasketDefinition String (Result Http.Error String)
    | SaveBasket
    | SavedBasket (Result Http.Error ())
    | Remove
    | RemovedBasket (Result Http.Error ())
    | NewName String
    | GotSeries (Result Http.Error String)
    | DoExpand
    | WidgetMsg Widget.Msg


getbaskets : Model -> Cmd Msg
getbaskets model =
    Http.get
        { expect = Http.expectString GotBasketNames
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "baskets" ] [ ]
        }

getbasket : Model -> String -> Cmd Msg
getbasket model name =
    Http.get
        { expect = Http.expectString (GotBasketDefinition name)
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "basket-definition" ]
                [ UB.string "name" name ]
        }

removebasket : Model -> String -> Cmd Msg
removebasket model name = Http.request
    { method = "DELETE"
    , body = Http.jsonBody <| JE.object
             [ ("name", JE.string name )
             ]
    , headers = [ ]
    , timeout = Nothing
    , tracker = Nothing
    , url = UB.crossOrigin model.baseurl [ "api",  "series", "basket" ] [ ]
    , expect = Http.expectWhatever <| RemovedBasket
    }

savebasket : Model -> String -> BasketFormula -> Cmd Msg
savebasket model name {node} = Http.request
    { method = "PUT"
    , body = Http.jsonBody <| JE.object
         [ ("name", JE.string name )
         , ("query" , JE.string <| Lisp.serialize <| serialize node )
         ]
    , headers = [ ]
    , timeout = Nothing
    , tracker = Nothing
    , url = UB.crossOrigin model.baseurl [ "api",  "series", "basket" ] [ ]
    , expect = Http.expectWhatever SavedBasket
    }

tryfilter : Model -> BasketFormula -> Cmd Msg
tryfilter model {node} = Http.get
    { expect = Http.expectString GotSeries
    , url = UB.crossOrigin
        model.baseurl
        [ "api",  "series", "find" ]
        [ UB.string "query" <| Lisp.serialize <| serialize node ]
    }

seriesdecoder : JD.Decoder Series
seriesdecoder =
    JD.map5 Series
        (JD.field "name" JD.string)
        (JD.field "imeta" (JD.succeed Nothing))
        (JD.field "meta" (JD.succeed Nothing))
        (JD.field "source" JD.string)
        (JD.field "kind" JD.string)


serieslistdecoder =
    JD.list seriesdecoder


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GotBasketNames (Ok rawbaskets) ->
            case JD.decodeString (JD.list JD.string) rawbaskets of
                Ok baskets ->
                    let
                        name =
                            Maybe.or (model.name) (List.head baskets)
                    in
                    ( { model
                          | baskets = baskets
                          , name = name
                      }
                    , Maybe.unwrap
                        (U.sendCmd SelectedBasket Nothing)
                        (getbasket model)
                        name
                    )

                Err err ->
                    doerr "getbasketnames decode" <| JD.errorToString err

        GotBasketNames (Err err) ->
            doerr "getbasketnames http" <| U.unwraperror err

        SelectedBasket Nothing ->
            let
                (wid, cmd) = Widget.setFormula Nothing model.editorWidget
            in
            ( { model
                | name = Nothing
                , creating = True
                , basket = Nothing
                , savedBasket = Nothing
                , newname = ""
                , editorExpanded = True
                , editorWidget = wid
                , series = []
              }
            , Cmd.map WidgetMsg cmd
            )

        SelectedBasket (Just name) ->
            ( { model
                | name = Just name
                , creating = False
              }
            , getbasket model name
            )

        GotBasketDefinition name (Ok rawbasket) ->
            case JD.decodeString JD.string rawbasket of
                Ok def ->
                    case fromlisp def of
                        Ok parsed ->
                            let
                                basketormula =
                                    { code = def, node = parsed }

                                (wid, cmd) = Widget.setFormula
                                    (Just basketormula.code)
                                    model.editorWidget
                            in
                            ( { model
                                  | basket = Just basketormula
                                  , savedBasket = Just basketormula
                                  , newname = name
                                  , editorExpanded = False
                                  , editorWidget = wid
                              }
                            , Cmd.batch
                                [ tryfilter model basketormula
                                , Cmd.map WidgetMsg cmd
                                ]
                            )
                        Err err -> doerr "parse basket" err
                Err err ->
                    doerr "getbasketdefinition decode" <| JD.errorToString err

        GotBasketDefinition _ (Err err) ->
            doerr "getbasketdefinition http" <| U.unwraperror err


        SaveBasket ->
            ( model
            , Maybe.unwrap
                Cmd.none
                (savebasket model model.newname)
                model.basket
            )

        SavedBasket (Ok _) ->
            ( { model
                | name = Just model.newname
                , savedBasket = model.basket
                , creating = False
              }
            , getbaskets model
            )

        SavedBasket (Err err) ->
            doerr "savebasket http" <| U.unwraperror err

        Remove ->
            ( model
            , Maybe.unwrap Cmd.none (removebasket model) model.name
            )

        RemovedBasket (Ok _) ->
            ( { model
                | name = Nothing
              }
            , getbaskets model
            )

        RemovedBasket (Err err) ->
            doerr "removebasket http" <| U.unwraperror err

        NewName name ->
            U.nocmd { model | newname = name }

        GotSeries (Ok things) ->
            case JD.decodeString serieslistdecoder things of
                Ok series ->
                    U.nocmd { model | series = series }

                Err err ->
                    doerr "getseries decode" <| JD.errorToString err

        GotSeries (Err err) ->
            doerr "getseries http" <| U.unwraperror err

        DoExpand ->
            U.nocmd { model | editorExpanded = not model.editorExpanded }

        WidgetMsg (Widget.NewFormula Nothing) ->
            ( { model
                | basket = Nothing
                , series = []
              }
            , Cmd.none
            )

        WidgetMsg (Widget.NewFormula (Just code)) ->
            case fromlisp code of
                Ok parsed ->
                    let
                        basketormula =
                            { code = code, node = parsed }
                    in
                    ( { model | basket = Just basketormula }
                    , tryfilter model basketormula
                    )
                Err err -> doerr "parse basked" err

        WidgetMsg x -> Tuple.mapBoth
            (\m -> { model | editorWidget = m })
            (Cmd.map WidgetMsg)
            (Widget.update x model.editorWidget)


hasEditedFormula : Model -> Bool
hasEditedFormula {basket, savedBasket, creating} =
    Maybe.map2
        (\current saved -> current.node /= saved.node)
        basket
        savedBasket
        |> Maybe.withDefault (False || creating)


strvalue: Value -> String
strvalue val =
    case val of
        Str v -> v
        Number n -> String.fromFloat n


viewsingleton: String -> H.Html Msg
viewsingleton name =
    H.ul [ HA.class "tree" ] [ H.li [] [ H.text name ] ]


viewstrparam: String -> String -> H.Html Msg
viewstrparam name param =
    H.ul [ HA.class "tree" ] [ H.li [] [ H.text <| name ++ " " ++ param ] ]


viewtwoparams: String -> String -> Value -> H.Html Msg
viewtwoparams name param1 param2 =
    H.ul [ HA.class "tree" ]
        [ H.li
              []
              [ H.text <| name ++ " " ++ param1 ++ " " ++ (strvalue param2) ]
        ]


viewterm: FilterNode -> H.Html Msg
viewterm term  =
    case term of
        TzAware ->
            viewsingleton "tzaware"

        Everything ->
            viewsingleton "everything"

        Formula ->
            viewsingleton "formula"

        ByCache ->
            viewsingleton "cache"

        FormulaContents contents ->
            viewstrparam "formulacontents " contents

        ByName name ->
            viewstrparam "byname " name

        BySource name ->
            viewstrparam "bysource " name

        ByCachePolicy name ->
            viewstrparam "cachepolicy " name

        ByMetakey key ->
            viewstrparam "metakey" key

        ByMetaITem key val ->
            viewtwoparams "bymetaitem " key val

        ByInternalMetaitem key val ->
            viewtwoparams "bymetaitem " key val

        Eq key val ->
            viewtwoparams "=" key val

        Lt key val ->
            viewtwoparams "<" key val

        Lte key val ->
            viewtwoparams "<=" key val

        Gt key val ->
            viewtwoparams ">" key val

        Gte key val ->
            viewtwoparams ">=" key val

        Not thing ->
            H.ul [ HA.class "tree" ]
                [ H.li [] [ H.summary [] [ H.text "not", viewterm thing ] ] ]

        And things ->
            H.ul [ HA.class "tree" ]
                [ H.li []
                      ([ H.summary [] [ H.text "and" ] ]
                           ++ (List.map viewterm things))
                ]

        Or things ->
            H.ul [ HA.class "tree" ]
                [ H.li []
                      ([ H.summary [] [ H.text "or" ] ]
                           ++ (List.map viewterm things))
                ]


viewseries : Model -> Html Msg
viewseries model =
    let
        item elt =
            let
                kind =
                    elt.kind
                sources =
                    elt.source
            in
            H.li
                [ HA.class "list-group-item p-1" ]
                [ H.span
                      [ HA.class <|
                            case kind of
                                "formula" -> "badge badge-success"
                                _ -> "badge badge-secondary"
                      ]
                      [ H.text kind ]
                , H.span [ ] [ H.text " " ]
                , H.a
                    [ HA.href <| UB.crossOrigin model.baseurl [ "tsinfo" ]
                          [ UB.string "name" elt.name ]
                    ]
                    [ H.text elt.name ]
                , H.span
                      -- alas, Chrome does not know `inline-end`
                    [ HA.style "float" "right" ]
                    [ H.span [] [ H.text " " ]
                    , H.span
                        [ HA.class "badge badge-info" ]
                        [ H.text elt.source ]
                    ]
                ]

        filteredslice =
            List.take 1000 model.series

        items =
            List.map item filteredslice

        nbseries =
            List.length model.series

        shown =
            List.length items

    in
    H.div []
        [ H.text <| "This basket yields " ++ (String.fromInt nbseries) ++ " series."
        , H.ul
            [ HA.class "list-group list-group-flush" ]
            items
        ]


viewerrors model =
    let
        viewitem item =
            H.text item
    in
    H.div [] <| List.map viewitem model.errors


viewedition : Model -> List (Html Msg)
viewedition model =
    let
        unpacksbasket basketname =
            JD.succeed <| SelectedBasket <| if basketname == "" then
                Nothing
            else
                Just basketname

        basketoption basketname =
            let
                txt =
                    if basketname == "" then
                        U.fromCharCode 127381
                    else
                        basketname
            in H.option
                [ HA.value basketname
                , HEX.attributeMaybe
                    (\name -> HA.selected (name == basketname))
                    model.name
                ]
                [ H.text txt ]

        basketslist =
            H.select
                  [ HA.name "basket"
                  , HE.on "change" (JD.andThen unpacksbasket HE.targetValue)
                  , HA.class "w-75"
                  ]
                  <| List.map basketoption ("" :: model.baskets)

        removeButton =
            H.button
                [ HA.type_ "button"
                , HA.class "btn btn-info ml-2"
                , HE.onClick Remove
                , HA.disabled model.creating
                ]
                [ H.text "Remove" ]

        saveButton =
            let
                sameName = Maybe.unwrap
                    False
                    (\x -> x == model.newname)
                    model.name

                goodname =
                    (model.newname /= "") &&
                    (not <| List.member model.newname model.baskets)

                testDis = not goodname || Maybe.isNothing model.basket

                (disabled, txt) =
                    if sameName then (False, "Save") else (testDis, "Save As")
            in
            H.button
                [ HA.class "btn btn-primary ml-2"
                , HA.disabled disabled
                , HE.onClick SaveBasket
                ]
                [ H.text txt ]

        saveEntry =
            H.input
                [ HA.attribute "type" "text"
                , HA.class "w-75"
                , HA.placeholder "new basket name"
                , HA.value model.newname
                , HE.onInput NewName
                ]
                [ ]

        saveLine =
            H.div [ HA.class "container-fluid mt-2" ] [ saveEntry, saveButton ]
        triangleCode =
            if model.editorExpanded then 9660 else 9654
    in
    [ H.h1 [ HA.class "page-title" ] [ H.text "Baskets" ]
    , H.div
        [ HA.class "container-fluid" ]
        [ H.div [ HA.class "container-fluid" ] [ basketslist, removeButton ]
        , HEX.viewIf (hasEditedFormula model) saveLine
        ]
    , H.div
        [ HA.class "mt-4" ]
        [ H.h5
            [ HE.onClick DoExpand ]
            [ H.text (U.fromCharCode triangleCode),  H.text " Basket Editor" ]
        , HEX.viewIf
            model.editorExpanded
            (Widget.view model.editorWidget |> H.map WidgetMsg)
        ]
    , H.div
        [ HA.class "mt-4" ]
        [ H.h5
            []
            [ H.text "Basket"
            , H.text (" " ++ model.newname)
            , HEX.viewIf (hasEditedFormula model) (H.text " *")
            ]
        , HEX.viewMaybe (.node >> viewterm) model.basket
        , viewseries model
        ]
    , viewerrors model
    ]



view : Model -> Html Msg
view model =
    H.div
        [ ]
        [ H.div
            [ HA.class "main-content" ]
            [ H.div [ HA.class "baskets", HA.style "margin" ".5em" ] <|
                viewedition model
            ]]


type alias Input =
    { baseurl : String
    , jsonSpec : JD.Value
    }


init : Input -> ( Model, Cmd Msg )
init { baseurl, jsonSpec } =
   let
        (widget, widgetCmd) = Widget.init
            { urlPrefix = baseurl
            , jsonSpec = jsonSpec
            , formulaCode = Nothing
            , returnTypeStr = "query"
            }
   in
   { baseurl = baseurl
   , errors = []
   , baskets = []
   , name = Nothing
   , basket = Nothing
   , savedBasket = Nothing
   , creating = False
   , newname = ""
   , series = []
   , editorExpanded = False
   , editorWidget = widget
   }
   |> \model ->
       ( model
       , Cmd.batch [ Cmd.map WidgetMsg widgetCmd, getbaskets model ]
       )

main : Program Input Model Msg
main =
       let
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
