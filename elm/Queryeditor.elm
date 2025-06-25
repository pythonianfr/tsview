module Queryeditor exposing ( main )

import Array
import AssocList as Assoc
import Browser
import Dict
import Editor.Type as ET
import Editor.UI.Widget as Widget
import Editor.UI.Tree as UITree
import Either
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
import HtmlExtra as HEX
import Http
import Info as I
import Json.Decode as JD
import Json.Encode as JE
import Lisp
import Maybe.Extra as Maybe
import Metadata as M
import Series as S
import Task
import Url.Builder as UB
import Util as U


type alias BasketFormula =
    { code : String
    , node : FilterNode
    }


type Mode = BasketList | BasketEdition


type alias Model =
    { baseurl : String
    , canwrite : Bool
    , mode: Mode
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
    , series : List S.Series
    -- editor
    , editorExpanded : Bool
    , editorWidget : Widget.Model
    , removing : Bool
    -- renaming
    , renaming : Bool
    }


type Msg
    = GetPermissions (Result Http.Error String)
    | GotBasketNames (Result Http.Error String)
    | SelectedBasket (Maybe String)
    | ExitEdition
    | Create
    | GotBasketDefinition String (Result Http.Error String)
    | SaveBasket
    | SavedBasket (Result Http.Error ())
    | Rename
    | RenameBasket
    | Remove
    | CancelRemove
    | ConfirmRemove
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


serieslistdecoder =
    JD.list S.seriesdecoder


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GetPermissions (Ok rawperm) ->
            case JD.decodeString JD.bool rawperm of
                Ok perms ->
                   U.nocmd { model | canwrite = perms }
                Err err ->
                    doerr "getpermissions decode" <| JD.errorToString err

        GetPermissions (Err err) ->
            doerr "getpermissions http" <| U.unwraperror err

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

        Create ->
            ( { model | mode = BasketEdition }
            , Task.perform identity <| Task.succeed (SelectedBasket Nothing)
            )

        SelectedBasket Nothing ->
            let
                (wid, cmd) = Widget.setFormula Nothing model.editorWidget

                editor = model.editorWidget.savedModel.editionTree.editor

                byAndCmd =
                    Assoc.get
                        (ET.returnTypeFromString editor.returnTypeStr)
                        editor.spec
                    |> Maybe.andThen (Assoc.get "by.and")
                    |> Maybe.withDefault ET.voidOperator
                    |> UITree.SelectOperator
                    |> UITree.EditEntry
                    |> UITree.EditNode Array.empty
                    |> U.sendCmd Widget.EditionTreeMsg
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
            , Cmd.map WidgetMsg <| Cmd.batch [ cmd, byAndCmd ]
            )

        SelectedBasket (Just name) ->
            ( { model
                  | name = Just name
                  , creating = False
                  , mode = BasketEdition
              }
            , getbasket model name
            )

        ExitEdition ->
            U.nocmd { model | mode = BasketList }

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
                            , Cmd.map WidgetMsg cmd
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
            U.nocmd { model | removing = True }

        CancelRemove ->
            U.nocmd { model | removing = False }

        ConfirmRemove ->
            ( { model | removing = False }
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

        Rename ->
            U.nocmd { model
                        | renaming = True
                        , newname = case model.name of
                                        Nothing -> ""
                                        Just name -> name
                    }
        RenameBasket ->
            ( model
            , Cmd.batch
                   [ Maybe.unwrap Cmd.none (savebasket model model.newname) model.basket
                   , Maybe.unwrap Cmd.none (removebasket model) model.name
                   ]
            )

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

        exitbutton =
            H.button [ HA.type_ "button"
                     , HA.class "btn btn-primary"
                     , HE.onClick ExitEdition
                     ] [ H.text "exit" ]

        renamebutton =
            H.button [ HA.type_ "button"
                     , HA.class "btn btn-warning"
                     , HE.onClick Rename
                     ] [ H.text "rename" ]

        removeButton =
            if model.removing then
                H.span [ ]
                    [ H.button
                        [ HA.type_ "button"
                        , HA.class "btn btn-warning"
                        , HE.onClick CancelRemove
                        ]
                        [ H.text "Cancel" ]
                    , H.span [] [ H.text " " ]
                    , H.button
                        [ HA.type_ "button"
                        , HA.class "btn btn-danger"
                        , HE.onClick ConfirmRemove
                        ]
                        [ H.text "Confirm" ]
                    ]
            else
                if model.canwrite then
                H.button
                    [ HA.type_ "button"
                    , HA.class "btn btn-danger ml-2"
                    , HE.onClick Remove
                    , HA.disabled model.creating
                    ]
                    [ H.text "delete" ]
                else H.span [] []

        saveButton event =
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
                    if model.renaming then
                        if sameName then (True, "Save") else (False, "Save")
                    else
                        if sameName then (False, "Save") else (testDis, "Save As")
            in
            if model.canwrite then
            H.button
                [ HA.class "btn btn-primary ml-2"
                , HA.disabled disabled
                , HE.onClick event
                ]
                [ H.text txt ]
            else H.span [] []

        saveEntry =
            H.input
                [ HA.attribute "type" "text"
                , HA.class "w-75"
                , HA.placeholder "new basket name"
                , HA.value model.newname
                , HE.onInput NewName
                ]
                [ ]

        saveLine event =
            if model.canwrite then
                H.div [ HA.class "container-fluid mt-2" ] [ saveEntry, saveButton event ]
            else H.span [] []

        triangleCode =
            if model.editorExpanded then 9660 else 9654
            -- BLACK DOWN-POINTING TRIANGLE else BLACK RIGHT-POINTING TRIANGLE
    in
    [ H.h1 [ HA.class "page-title" ] [ H.text "Baskets" ]
    , H.div
        [ ]
        [ H.div [ ] [ exitbutton, renamebutton, removeButton ]
        , HEX.viewIf (hasEditedFormula model) (saveLine SaveBasket)
        , HEX.viewIf model.renaming (saveLine RenameBasket)
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
        , viewseries model
        ]
    , viewerrors model
    ]


viewbasketlist model =
    let
        viewbasketitem item =
            H.li [ HA.class "list-group-item p-1" ]
                [ H.span
                      [ HE.onClick <| SelectedBasket (Just item) ]
                      [ H.text item ]
                ]

        numbaskets =
            List.length model.baskets

        basketsmsg =
            case numbaskets of
                0 -> "No basket. You should start one ! "
                1 -> "One basket."
                _ -> "There are " ++ (String.fromInt numbaskets) ++ " baskets."
    in
    [ H.h1 [ HA.class "page-title" ] [ H.text "Baskets" ]
    , H.p [ ]
        [ H.text basketsmsg
        , H.text " "
        , H.button [ HA.type_ "button"
                   , HA.class "btn btn-primary"
                   , HE.onClick Create
                   ] [ H.text "Create a basket" ]
        ]
    , H.ul
          [ HA.class "list-group list-group-flush" ]
          <| List.map viewbasketitem model.baskets
    ]



view : Model -> Html Msg
view model =
    H.div
        [ ]
        [ H.div
            [ HA.class "main-content formula_editor" ]
            [ H.div [ HA.class "baskets", HA.style "margin" ".5em" ] <|
                case model.mode of
                    BasketList -> viewbasketlist model
                    BasketEdition -> viewedition model
            ]]


type alias Input =
    { baseurl : String
    , jsonSpec : JD.Value
    , basketName : JD.Value
    }


init : Input -> ( Model, Cmd Msg )
init { baseurl, jsonSpec, basketName } =
   let
        (widget, widgetCmd) = Widget.init
            { urlPrefix = baseurl
            , jsonSpec = jsonSpec
            , formulaCode = Nothing
            , returnTypeStr = "query"
            }
   in
   { baseurl = baseurl
   , canwrite = False
   , mode = BasketList
   , errors = []
   , baskets = []
   , name = JD.decodeValue (JD.maybe JD.string) basketName
        |> Either.fromResult
        |> Either.toMaybe
        |> Maybe.join
   , basket = Nothing
   , savedBasket = Nothing
   , creating = False
   , newname = ""
   , series = []
   , editorExpanded = False
   , editorWidget = widget
   , removing = False
   , renaming = False
   }
   |> \model ->
       ( model
       , Cmd.batch [ I.getwriteperms baseurl GetPermissions
                   , Cmd.map WidgetMsg widgetCmd
                   , getbaskets model
                   ]
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
