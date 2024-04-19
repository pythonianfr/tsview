module Queryeditor exposing ( main )

import TsView.AceEditor as AceEditor
import Browser
import Dict
import Filter exposing
    ( FilterNode(..)
    , Value(..)
    , fromlisp
    , parse
    , serialize
    )
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import Lisp
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    -- all errors
    , errors : List String
    -- baskets
    , baskets : List String
    -- current basket
    , name : Maybe String
    , basket : Maybe String
    , edited : Maybe FilterNode
    , editing : Maybe FilterNode
    -- creation
    , creating : Bool
    , newname : String
    }


type Msg
    = GotBasketNames (Result Http.Error String)
    | SelectedBasket String
    | GotBasketDefinition (Result Http.Error String)
    | AceEditorMsg AceEditor.Msg
    | SaveBasket Bool
    | SavedBasket Bool (Result Http.Error ())
    | Create
    | NewName String
    | CancelCreation


getbaskets model =
    Http.get
        { expect = Http.expectString GotBasketNames
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "baskets" ] [ ]
        }


getbasket model name =
    Http.get
        { expect = Http.expectString GotBasketDefinition
        , url = UB.crossOrigin model.baseurl
                [ "api",  "series", "basket-definition" ]
                [ UB.string "name" name ]
        }


savebasket model name editing creation =
    case editing of
        Nothing ->
            Cmd.none
        Just code ->
            Http.request
                { method = "PUT"
                , body = Http.jsonBody <| JE.object
                         [ ("name", JE.string name )
                         , ("query" , JE.string <| Lisp.serialize <| serialize code )
                         ]
                , headers = [ ]
                , timeout = Nothing
                , tracker = Nothing
                , url = UB.crossOrigin model.baseurl
                        [ "api",  "series", "basket" ] [ ]
                , expect = Http.expectWhatever <| SavedBasket creation
                }


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
                        first =
                            List.head baskets
                    in
                    ( { model
                          | baskets = baskets
                          , name = first
                      }
                    , case first of
                          Nothing -> Cmd.none
                          Just head -> getbasket model head
                    )

                Err err ->
                    doerr "getbasketnames decode" <| JD.errorToString err

        GotBasketNames (Err err) ->
            doerr "getbasketnames http" <| U.unwraperror err

        SelectedBasket name ->
            ( { model | name = Just name }
            , getbasket model name
            )

        GotBasketDefinition (Ok rawbasket) ->
            case JD.decodeString JD.string rawbasket of
                Ok def ->
                    case fromlisp def of
                        Ok parsed ->
                            U.nocmd { model
                                        | basket = Just def
                                        , edited = Just parsed
                                        , editing = Just parsed
                                    }
                        Err err -> doerr "parse basket" err
                Err err ->
                    doerr "getbasketdefinition decode" <| JD.errorToString err

        GotBasketDefinition (Err err) ->
            doerr "getbasketdefinition http" <| U.unwraperror err

        AceEditorMsg (AceEditor.Edited code) ->
            case fromlisp code of
                Ok root ->
                    U.nocmd { model | editing = Just root }
                Err _ ->
                    U.nocmd model

        SaveBasket creating ->
            let
                name =
                    case creating of
                        True -> model.newname
                        _ ->  (Maybe.withDefault "" model.name)
            in
            ( model
            , savebasket model name model.editing creating
            )

        SavedBasket creating (Ok _) ->
            let
                cmd =
                    case creating of
                        True ->
                            getbaskets model
                        _ ->
                            Cmd.none
                newmodel =
                    case creating of
                        True ->
                            { model
                                | basket = Just model.newname
                                , creating = False
                            }
                        _ ->
                            model
            in
            ( { newmodel | edited = model.editing }
            , cmd
            )

        SavedBasket _ (Err err) ->
            doerr "savebasket http" <| U.unwraperror err

        Create ->
            U.nocmd { model
                        | creating = True
                        , basket = Nothing
                        , editing = Nothing
                        , edited = Nothing
                    }

        NewName name ->
            U.nocmd { model | newname = name }

        CancelCreation ->
            ( { model
                  | creating = False
                  , editing = Nothing
                  , edited = Nothing
              }
            , getbasket model <| Maybe.withDefault "" model.name
            )


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


viewtree tree =
    case tree of
        Nothing -> H.span [] []
        Just edited ->
            viewterm edited


editorHeight =
    HA.attribute "style" "--min-height-editor: 36vh"


vieweditor model =
    let
        code =
            case model.basket of
                Nothing -> ""
                Just thing -> thing
        goodname =
            case model.creating of
                True ->
                    (model.newname /= "") && (not <| List.member model.newname model.baskets)
                _ ->
                    True
    in
    H.div
        [ HA.class "code_left"
        ,  editorHeight
        ]
        [ AceEditor.edit AceEditor.default code True |> H.map AceEditorMsg
        , case model.editing of
              Nothing -> H.span [] []
              Just lastvalidedit ->
                  H.div
                      []
                      [ H.text "Last valid state:"
                      , viewtree model.editing
                      , H.button
                          [ HA.class "btn btn-primary"
                          , HA.disabled <| (not goodname) || (model.editing == model.edited)
                          , HE.onClick <| SaveBasket model.creating
                          ]
                          [ H.text "Save" ]
                      ]
        ]


viewerrors model =
    let
        viewitem item =
            H.text item
    in
    H.div [] <| List.map viewitem model.errors


viewedition model =
    let
        unpacksbasket basketname =
            JD.succeed <| SelectedBasket basketname

        basketoption basketname =
            H.option
                [ HA.value basketname ]
                [ H.text basketname ]

        basketslist =
            [ H.select
                  [ HA.name "basket"
                  , HE.on "change" (JD.andThen unpacksbasket HE.targetValue)
                  , HA.class "form-control"
                  ]
                  <| List.map basketoption model.baskets
            ]

    in
    [ H.h1 [] [ H.text "Baskets" ]
    , H.div [] basketslist
    , viewtree model.edited
    , vieweditor model
    , H.button
        [ HA.class "btn btn-info"
        , HE.onClick Create
        ]
        [ H.text "Create" ]
    , viewerrors model
    ]


viewcreation model =
    [ H.h1 [] [ H.text "Baskets" ]
    , viewtree model.edited
    , vieweditor model
    , H.button
        [ HA.class "btn btn-info"
        , HE.onClick CancelCreation
        ]
        [ H.text "Cancel" ]
    ,  H.input [ HA.attribute "type" "text"
               , HA.class "form-control"
               , HA.placeholder "new basket name"
               , HA.value model.newname
               , HE.onInput NewName
               ] [ ]
    , viewerrors model
    ]


view model =
    H.div [ HA.style "margin" ".5em" ] <|
        if model.creating
        then viewcreation model
        else viewedition model


type alias Input =
    { baseurl : String }


main : Program Input Model Msg
main =
       let
           init input =
               let
                   model =
                       { baseurl = input.baseurl
                       , errors = []
                       , baskets = []
                       , name = Nothing
                       , basket = Nothing
                       , edited = Nothing
                       , editing = Nothing
                       , creating = False
                       , newname = ""
                       }
               in
               ( model, getbaskets model )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
