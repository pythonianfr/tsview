module Settings exposing (main)

import Array
import Browser

import Json.Decode as JD
import Json.Encode as JE
import Html exposing (
    Html
    , div
    , table
    , header
    , th
    , tr
    , td
    , input
    , button
    , br
    , text)
import Html.Attributes exposing (
    class
    , hidden
    , value)
import Html.Events exposing (
    onInput
    , onClick )

import Http
import List

type alias Model =
    { baseUrl: String
    , horizons : Array.Array Record
    , toDelete: Array.Array Record
    , message : String }


type alias FromServer =
    { label: String
    , from: String
    , to: String
    , id: Int
    }

type alias Record =
    { label: String
    , from: String
    , to: String
    , id: Int
    , action: Action
    }


type Action =
    Update
    | Create
    | Delete


actionName: Action -> String
actionName action =
    case action of
        Update -> "update"
        Create -> "create"
        Delete -> "delete"


extendRec: FromServer -> Record
extendRec rec =
    { id = rec.id
    , from = rec.from
    , to = rec.to
    , label = rec.label
    , action = Update
    }


catalogDecoder: JD.Decoder ( List Record )
catalogDecoder =
    ( JD.list
        ( JD.map
            extendRec
                ( JD.map4 FromServer
                    (JD.field "label" JD.string)
                    (JD.field "fromdate" JD.string)
                    (JD.field "todate" JD.string)
                    (JD.field "id" JD.int)
            )
        )
    )


recordEncode: Record -> JE.Value
recordEncode record =
    JE.object
        [( "label", JE.string record.label )
        , ( "fromdate", JE.string record.from )
        , ( "todate", JE.string record.to )
        , ( "action", JE.string "update" )
        , ( "id", JE.int record.id)
        , ("action", JE.string ( actionName record.action ))
        ]


catalogEncode: Model -> JE.Value
catalogEncode model =
    JE.list
        recordEncode
        (( Array.toList model.horizons )
        ++ ( Array.toList model.toDelete ))


type Msg =
    GotHorizons ( Result Http.Error ( List Record ))
    | UserInput (Int, String) String
    | AddRow
    | Remove Int
    | Up Int
    | Down Int
    | Save
    | Saved ( Result Http.Error ())



getHorirzons: Model -> Cmd Msg
getHorirzons model =
    Http.get
        { url = model.baseUrl ++ "list-horizons"
        , expect = Http.expectJson GotHorizons catalogDecoder
        }


saveHorizons: Model -> Cmd Msg
saveHorizons model =
    Http.post
    { url = model.baseUrl ++ "replace-horizons"
    , body = Http.jsonBody
                ( catalogEncode model )
    , expect = Http.expectWhatever Saved
    }


update: Msg -> Model -> ( Model, (Cmd Msg) )
update msg model =
    case msg of
        GotHorizons (Ok stuffs) ->
            ({ model | horizons = Array.fromList stuffs }
            , Cmd.none)

        GotHorizons (Err emsg) ->
            ( model, Cmd.none )

        UserInput ( index, name ) value ->
            ( { model | horizons = updateFromUser
                                        model.horizons
                                        index
                                        name
                                        value }
            , Cmd.none )
        AddRow ->
            ( {model | horizons =  addRow model.horizons }
            , Cmd.none )

        Up index-> ( { model | horizons = permut model.horizons index ( index - 1 )}
                    , Cmd.none )
        Down index-> ( { model | horizons = permut model.horizons index ( index + 1 )}
                    , Cmd.none )
        Remove index-> ( isolateDelete { model | horizons = updateFromUser
                                                    model.horizons
                                                    index
                                                    "action"
                                                    "delete"
                                       }
                       , Cmd.none )

        Save -> ( model, saveHorizons model )

        Saved (Ok _) -> ( { model | message = "New definitions saved" }
                        , Cmd.none)
        Saved (Err _) -> ( { model | message = "Error while saving" }
                        , Cmd.none)


permut: Array.Array a -> Int -> Int -> Array.Array a
permut array i1 i2 =
    let temp1 = Array.get i1 array
        temp2 = Array.get i2 array
    in
    case temp1 of
        Nothing -> array
        Just tmp1 ->
            case temp2 of
                Nothing -> array
                Just tmp2 ->
                    (Array.set
                        i1
                        tmp2
                        (Array.set
                            i2
                            tmp1
                            array))


isolateDelete: Model -> Model
isolateDelete model =
    let activeRecords = List.filter
                            (\ rec -> rec.action /= Delete)
                            ( Array.toList model.horizons )
        deletedRecords = List.filter
                            (\ rec -> rec.action == Delete)
                            ( Array.toList model.horizons )
    in
        { model | horizons = Array.fromList activeRecords
                , toDelete = Array.fromList deletedRecords
        }


addRow: Array.Array Record -> Array.Array Record
addRow records =
     Array.push
        { label = ""
        , from = ""
        , to = ""
        , id = -1
        , action = Create
        }
        records


updateFromUser: Array.Array Record -> Int -> String -> String -> Array.Array Record
updateFromUser horizons index name value =
   Array.fromList
       ( List.map
            ( mutateHorizon index name value )
            ( Array.toIndexedList horizons ))


mutateHorizon: Int -> String -> String -> ( Int,  Record ) -> Record
mutateHorizon targetIndex name targetValue ( index,  record ) =
       if targetIndex == index
       then mutateRecord name record targetValue
       else record


mutateRecord: String -> Record -> String -> Record
mutateRecord name record value =
    if name == "label"
    then { record | label = value}
    else
        if name == "from"
        then { record | from = value}
        else
            if name == "to"
            then { record | to = value}
            else
                if name == "action"
                    then if (value == "delete")
                         then { record | action = Delete }
                         else record
                    else record


viewRows: Model -> List ( Html Msg )
viewRows model =
    List.indexedMap
        viewRow
        ( Array.toList model.horizons )


viewRow: Int -> Record -> Html Msg
viewRow index record =
    tr
        [ ]
        [ td
            []
            [ input
                [ value record.label
                , onInput ( UserInput ( index, "label" ))]
                []
            ]
        , td
             []
             [  input
                [ value record.from
                , onInput ( UserInput ( index, "from" ))]
                []
            ]
        , td
            []
            [ input
                [ value record.to
                , onInput ( UserInput ( index, "to" ))]
                []
            ]
        , td
            []
            [ button [onClick (Up index)] [text "↑"]
            , button [onClick (Down index)] [text "↓"]
            , button [onClick (Remove index)] [text "❌"]]
        ]

viewHeader: Html Msg
viewHeader =
    tr  []
        [ th [] [text "label"]
        , th [] [text "from"]
        , th [] [text "to"]
        , th [] []
        ]

view: Model -> Html Msg
view model =
    div
        []
        [ table
            [ ]
            ( [ viewHeader ]
            ++ ( viewRows model ))
        , button
            [ class "btn btn-primary"
            , onClick AddRow ]
            [ text "+" ]
        , br [] []
        , button
            [ class "btn btn-primary"
            , onClick Save ]
            [ text "Update" ]
        , div
            []
            [ text model.message ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init: String -> ( Model, ( Cmd Msg ))
init baseUrl =
    let newModel = { baseUrl = baseUrl
                   , horizons = Array.empty
                   , toDelete = Array.empty
                   , message = "" }
   in
    ( newModel
    , getHorirzons newModel
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }