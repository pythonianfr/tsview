module Settings exposing (main)

import Array
import Browser

import Json.Decode as JD
import Json.Encode as JE
import Html exposing (
    Html
    , div
    , br
    , h1
    , h2
    , option
    , table
    , thead
    , th
    , tr
    , td
    , input
    , select
    , button
    , br
    , text)
import Html.Attributes exposing (
    class
    , selected
    , type_
    , value
    )
import Html.Events exposing (
    onInput
    , on
    , onClick
    , targetValue
    )
import Http
import Http exposing (
    Expect,
    Metadata,
    Response)
import Url.Builder as UB

type alias Model =
    { baseUrl: String
    , horizons : Array.Array Record
    , toDelete: Array.Array Record
    , message : String
    , isPro : Bool
    , userModel : UserModel
    }

type alias UserModel =
    { users : List User
    , roleChoices: List String
    }

type alias User =
    { email: String
    , editedEmail: Maybe String
    , role: String
    , editedRole: Maybe String
    , new: Bool
    }

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


initModel: String -> Model
initModel baseUrl =
   { baseUrl = baseUrl
   , horizons = Array.empty
   , toDelete = Array.empty
   , message = ""
   , isPro = False
   , userModel = emptyUserModel
   }


roles =
    [ "guest"
    , "ro"
    , "rw"
    , "admin"
    ]


emptyUserModel : UserModel
emptyUserModel =
    { users = []
    , roleChoices = roles
    }

errorUser: User
errorUser =
    { email = "error"
    , editedEmail = Nothing
    , role = "error"
    , editedRole = Nothing
    , new = False
    }


emptyUser: User
emptyUser =
    { email = ""
    , editedEmail = Nothing
    , role = ""
    , editedRole = Nothing
    , new = True
    }

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


toUser : List String -> User
toUser items =
    case items of
        [ m, r ] ->
                { email = m
                , editedEmail = Nothing
                , role = r
                , editedRole = Nothing
                , new = False
                }
        _ -> errorUser


usersDecoder: JD.Decoder ( List User )
usersDecoder =
    JD.list <| JD.map
                toUser
                ( JD.list JD.string )


type Msg =
    GotHorizons ( Result Http.Error ( List Record ))
    | UserInput (Int, String) String
    | AddRow
    | Remove Int
    | Up Int
    | Down Int
    | Save
    | Saved ( Result ErrorDetailed (Metadata, String))
    | GotPro ( Result Http.Error String )
    | Users UserMsg


type UserMsg =
     GotUsers ( Result Http.Error ( List User ))
     | ChangeRole Int String
     | ChangeMail Int String
     | Cancel Int
     | SaveUsers
     | UserSaved ( Result Http.Error String )
     | CreateUser
     | RemoveNewUser Int


getHorirzons: Model -> Cmd Msg
getHorirzons model =
    Http.get
        { url = model.baseUrl ++ "list-horizons"
        , expect = Http.expectJson GotHorizons catalogDecoder
        }


getUsers: String -> Cmd Msg
getUsers baseUrl =
    Http.get
        { url = baseUrl ++ "api/permissions/user_roles"
        , expect = Http.expectJson ( \ m -> Users ( GotUsers m )) usersDecoder
        }

-- dealing  with the lack of proper message return
-- in case of 500

type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String


unpackError: ErrorDetailed -> String
unpackError error =
    case error of
        BadUrl msg ->
            msg
        Timeout ->
            "Timeout"
        NetworkError ->
            "NetworkError"
        BadStatus metadata err ->
            err
        BadBody body ->
            body

convertResponseString : Response String -> Result ErrorDetailed ( Metadata, String )
convertResponseString httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )


expectStringDetailed : (Result ErrorDetailed ( Metadata, String ) -> msg) -> Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseString

-- ending with the 500 error handling

saveHorizons: Model -> Cmd Msg
saveHorizons model =
    Http.post
    { url = model.baseUrl ++ "replace-horizons"
    , body = Http.jsonBody
                ( catalogEncode model )
    , expect = expectStringDetailed Saved
    }


saveUser: String -> User -> Cmd Msg
saveUser baseUrl user =
    Http.request
        { url = UB.crossOrigin
                    baseUrl
                    [ "api", "permissions", "user_roles" ]
                    [ UB.string "userid" <|
                                    Maybe.withDefault
                                        user.email
                                        user.editedEmail
                    , UB.string "rolename" <|
                                    Maybe.withDefault
                                        user.role
                                        user.editedRole
                    ]
        , body = Http.emptyBody
        , headers = []
        , method = "PATCH"
        , expect =  Http.expectString ( \r -> Users ( UserSaved r ))
        , timeout = Nothing
        , tracker = Nothing
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

        Saved (Ok _) -> let newModel = { model | message = "New definitions saved"
                                               , horizons = Array.empty
                                               , toDelete = Array.empty
                                        }
                        in
                            ( newModel
                            , getHorirzons newModel )

        Saved (Err error) -> ( { model | message = unpackError error }
                             , Cmd.none)
        GotPro (Ok _) -> ( { model | isPro = True }
                         , getUsers model.baseUrl
                         )
        GotPro (Err _) ->( { model | isPro = False }, Cmd.none )

        Users uMsg -> let ( uModel, cmd ) = updateUsers
                                                model.baseUrl
                                                model.userModel
                                                uMsg
                      in ( { model | userModel = uModel}
                         , cmd
                         )


updateUsers : String -> UserModel -> UserMsg -> ( UserModel, Cmd Msg )
updateUsers baseUrl model msg =
    case msg of
        GotUsers ( Ok payload ) ->
            ( { model | users = payload }
            , Cmd.none
            )

        GotUsers ( Err _ ) ->
            ( model, Cmd.none )

        ChangeRole idx role ->
            let arrayUsers = Array.fromList model.users
                user = Maybe.withDefault
                        errorUser
                        ( Array.get idx arrayUsers )
                changedUser = { user | editedRole = Just role }
                newModel = { model | users = Array.toList
                                                <| Array.set
                                                    idx
                                                    changedUser
                                                    arrayUsers
                            }
            in
                ( newModel, Cmd.none )

        ChangeMail idx mail ->
            let arrayUsers = Array.fromList model.users
                user = Maybe.withDefault
                        errorUser
                        ( Array.get idx arrayUsers )
                changedUser = { user | editedEmail = Just mail }
                newModel = { model | users = Array.toList
                                                <| Array.set
                                                    idx
                                                    changedUser
                                                    arrayUsers
                            }
            in
                ( newModel, Cmd.none )

        Cancel idx ->
            let arrayUsers = Array.fromList model.users
                user = Maybe.withDefault
                        errorUser
                        ( Array.get idx arrayUsers )
                changedUser = { user | editedEmail = Nothing
                                     , editedRole = Nothing
                              }
                newModel = { model | users = Array.toList
                                                <| Array.set
                                                    idx
                                                    changedUser
                                                    arrayUsers
                            }
            in
                ( newModel, Cmd.none )
        CreateUser ->
            ({ model | users = List.append model.users [ emptyUser ]}
            , Cmd.none )
        RemoveNewUser idx ->
            let removed = List.map
                            Tuple.second
                            <| List.filter
                                (\ ( i, _ ) -> i /= idx )
                                <| List.indexedMap
                                        Tuple.pair
                                        model.users
            in
                ({ model | users = removed } , Cmd.none )
        SaveUsers ->
            let updated = List.filter
                            isEdited
                            model.users
            in ( model,
                Cmd.batch
                   <| List.map
                        ( saveUser baseUrl )
                        updated
                )
        UserSaved ( Ok _ ) -> ( model, getUsers baseUrl)
        UserSaved ( Err _ ) -> ( model, Cmd.none )



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
        deletedRecords = ( Array.toList model.toDelete
                         ) ++ (List.filter
                                (\ rec -> rec.action == Delete)
                                ( Array.toList model.horizons ))
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
            [ class "settings-label" ]
            [ input
                [ value record.label
                , onInput ( UserInput ( index, "label" ))]
                []
            ]
        , td
             [ class "settings-date" ]
             [  input
                [ value record.from
                , onInput ( UserInput ( index, "from" ))]
                []
            ]
        , td
            [ class "settings-date" ]
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
    thead
        []
        [ tr    []
                [ th [] [text "Label"]
                , th [] [text "From"]
                , th [] [text "To"]
                , th [] []
                ]
        ]

tableFooter: Html Msg
tableFooter =
    tr
        []
        [ td [] []
        , td [] []
        , td [] []
        , td
            []
            [ button
                [  type_ "button"
                , class "  add-row"
                , onClick AddRow ]
                [ text " + " ]
            ]
        ]

rowUser: List String -> ( Int,  User ) -> Html Msg
rowUser choices ( idx, user) =
    let visible = if isEdited user
                    then ""
                    else "invisible"
        newClass = if user.new
                    then ""
                    else "invisible"
    in
    tr
        []
        [ td
            [ class "settings-label"  ]
            [ input
                [ value <| Maybe.withDefault
                                user.email
                                user.editedEmail
                , onInput (\s -> ( Users ( ChangeMail idx s )))
                ]
                []
            ]
        , td
            []
             [ dropDownRole
                choices
                idx
                <| Maybe.withDefault
                        user.role
                        user.editedRole
             ]
         , td
            []
            [ button
                [ class "btn btn-success"
                , class visible
                , onClick ( Users SaveUsers )
                ]
                [ text "save all" ]
            , button
                [ class "btn btn-warning"
                , class visible
                , onClick ( Users ( Cancel idx ))
                ]
                [ text "cancel" ]
            , button
                [ class "btn btn-danger"
                , class newClass
                , onClick ( Users ( RemoveNewUser idx ))
                ]
                [text "remove"]
            ]
        ]


isEdited: User -> Bool
isEdited user =
    case user.editedEmail of
        Just _ -> True
        Nothing ->
            case user.editedRole of
                Just _ -> True
                Nothing -> False


dropDownRole: List String -> Int ->  String -> Html Msg
dropDownRole choices idx role  =
    let decodeRole: String -> JD.Decoder Msg
        decodeRole r = JD.succeed ( Users ( ChangeRole idx r ))
    in
    select
        [ on "change" (JD.andThen decodeRole targetValue )]
        ( List.map (renderRole role) choices )


renderRole : String -> String -> Html msg
renderRole selectedRole role =
    option
        [ value role
        , selected (selectedRole == role)
        ]
        [ text role ]


viewSettings: Model -> Html Msg
viewSettings model =
    div
    [ class "settings" ]
    [ h1
        [ class "page-title" ]
        [ text "Settings" ]
    , h2
        [ ]
        [ text "Horizons" ]
    , div
        [ class "horizons" ]
        [ table
            [ class "table" ]
            ( [ viewHeader ]
            ++ ( viewRows model )
            ++ [ tableFooter ] )
        , br [] []
        , div
            []
            [ button
                [ class "btn btn-success update"
                , onClick Save ]
                [ text "Update definitions" ]
            , div
                []
                [ text model.message ]
            ]
        ]
    ]


headerUsers : Html Msg
headerUsers =
    thead
        []
        [ tr
            []
            [ th [] [text "Email"]
            , th [] [text "Role"]
            , th [] []
            ]
        ]


viewUsers: UserModel -> Html Msg
viewUsers model =
    div
        [ class "settings" ]
        [ table
            [ class "table" ]
            ([ headerUsers
             ] ++ ( List.map
                    ( rowUser model.roleChoices )
                    ( List.indexedMap Tuple.pair model.users )
                  )
             )
        ,  button
            [ class "btn btn-success update"
            , onClick ( Users CreateUser ) ]
            [ text "Create New User" ]
        ]


view: Model -> Html Msg
view model =
   div
    []
    [ viewSettings model
    , if model.isPro
        then
            viewUsers model.userModel
        else div [] []
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getPro: String -> Cmd Msg
getPro baseUrl =
    Http.get
        { url = baseUrl ++ "ispro"
        , expect = Http.expectString GotPro
        }

init: String -> ( Model, ( Cmd Msg ))
init baseUrl =
    let newModel = initModel baseUrl
    in
    ( newModel
    , Cmd.batch [ getHorirzons newModel
                , getPro baseUrl
                ]
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }