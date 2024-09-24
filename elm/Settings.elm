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
    , text)
import Html.Attributes exposing (
    class
    , value)
import Html.Events exposing (
    onInput
    , onClick )

import Http
import List

type alias Model =
    { baseUrl: String
    , horizons : Array.Array Record
    , message : String }

type alias Record =
    { label: String
    , from: String
    , to: String
    , rank: Int
    , id: Int
    }

catalogDecoder: JD.Decoder ( List Record )
catalogDecoder =
    JD.list
        ( JD.map5 Record
            (JD.field "label" JD.string)
            (JD.field "fromdate" JD.string)
            (JD.field "todate" JD.string)
            (JD.field "id" JD.int)
            (JD.field "rank" JD.int)
        )

recordEncode: Record -> JE.Value
recordEncode record =
    JE.object
        [( "label", JE.string record.label )
        , ( "fromdate", JE.string record.from )
        , ( "todate", JE.string record.to )
        , ( "action", JE.string "update" )
        , ( "id", JE.int record.id)
        ]

catalogEncode: ( List Record ) -> JE.Value
catalogEncode records =
    JE.list
        recordEncode
        records


type Msg =
    GotHorizons ( Result Http.Error ( List Record ))
    | UserInput (Int, String) String
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
                ( catalogEncode
                    ( Array.toList model.horizons ))
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

        Save -> ( model, saveHorizons model )

        Saved (Ok _) -> ( { model | message = "New definitions saved" }
                        , Cmd.none)
        Saved (Err _) -> ( { model | message = "Error while saving" }
                        , Cmd.none)


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
            else record


viewRows: Model -> List ( Html Msg )
viewRows model =
    List.indexedMap
        viewRow
        ( Array.toList model.horizons )


viewRow: Int -> Record -> Html Msg
viewRow index record =
    tr
        []
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
        ]

viewHeader: Html Msg
viewHeader =
    tr  []
        [ th [] [text "label"]
        , th [] [text "from"]
        , th [] [text "to"]]

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