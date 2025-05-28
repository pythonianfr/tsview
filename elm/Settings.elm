module Settings exposing (main)

import Array
import Browser
import Dict exposing (Dict)

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
    , a
    , ul
    , li
    , input
    , p
    , select
    , button
    , br
    , text)
import Html.Attributes exposing (
    attribute
    , class
    , href
    , selected
    , type_
    , value
    )
import Html.Events exposing
    ( onInput
    , on
    , onClick
    , targetValue
    )
import Http
import Http exposing
    ( Expect
    , Metadata
    , Response
    )
import Info as I
import Url.Builder as UB
import Util as U
import Url.Builder as UB


type alias Model =
    { baseUrl: String
    , canwrite: Bool
    , tab : Tab
    , isPro : Bool
    , horizonModel: HorizonModel
    , userModel : UserModel
    }

type Tab
    = HorizonTab
    | UserTab


tabList =
    [ HorizonTab
    , UserTab
    ]


tabLabel: Tab -> String
tabLabel tab =
    case tab of
        HorizonTab -> "Horizons"
        UserTab -> "Users"


type alias HorizonModel =
    { horizons : Array.Array Record
    , toDelete: Array.Array Record
    , message : String
    }


type alias UserModel =
    { users : Dict String User
    , roleChoices: List String
    }

type alias User =
    { editedEmail: Maybe String
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


type Action
    = Update
    | Create
    | Delete


roles =
    [ "guest"
    , "ro"
    , "rw"
    , "admin"
    ]


emptyHorizonModel: HorizonModel
emptyHorizonModel =
    { horizons = Array.empty
    , toDelete = Array.empty
    , message = ""
    }


emptyUserModel : UserModel
emptyUserModel =
    { users = Dict.empty
    , roleChoices = roles
    }

errorUser: User
errorUser =
    { editedEmail = Nothing
    , role = "error"
    , editedRole = Nothing
    , new = False
    }


emptyUser: User
emptyUser =
    { editedEmail = Nothing
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
    JD.list
        ( JD.map
            extendRec
                ( JD.map4 FromServer
                    ( JD.field "label" JD.string )
                    ( JD.field "fromdate" JD.string )
                    ( JD.field "todate" JD.string )
                    ( JD.field "id" JD.int )
            )
        )


recordEncode: Record -> JE.Value
recordEncode record =
    JE.object
        [ ( "label", JE.string record.label )
        , ( "fromdate", JE.string record.from )
        , ( "todate", JE.string record.to )
        , ( "action", JE.string "update" )
        , ( "id", JE.int record.id )
        , ( "action", JE.string ( actionName record.action ) )
        ]


catalogEncode: HorizonModel -> JE.Value
catalogEncode model =
    JE.list
        recordEncode <|
        ( Array.toList model.horizons ) ++
        ( Array.toList model.toDelete )


toUser : List String -> ( String, User )
toUser items =
    case items of
        [ m, r ] ->
            ( m
            , { editedEmail = Nothing
              , role = r
              , editedRole = Nothing
              , new = False
              }
            )
        _ ->
            ( "Decoding error", errorUser )


usersDecoder: JD.Decoder ( Dict String  User )
usersDecoder =
    JD.map
        Dict.fromList
            <| JD.list <| JD.map
                toUser
                ( JD.list JD.string )


type Msg
    = GotPro ( Result Http.Error String )
    | GotPermissions ( Result Http.Error String )
    | ChangeTab Tab
    | Horizons HorizonMsg
    | Users UserMsg


type HorizonMsg
    = GotHorizons ( Result Http.Error ( List Record ))
    | UserInput (Int, String) String
    | AddRow
    | Remove Int
    | Up Int
    | Down Int
    | Save
    | Saved ( Result ErrorDetailed (Metadata, String))


type UserMsg
    = GotUsers ( Result Http.Error ( Dict String User ))
    | ChangeRole String String
    | ChangeMail String String
    | Cancel String
    | SaveSingle String
    | SaveUsers
    | UserSaved ( Result Http.Error String )
    | CreateUser
    | RemoveNewUser String


getHorirzons: String -> HorizonModel -> Cmd Msg
getHorirzons baseUrl model =
    Http.get
        { url = UB.crossOrigin baseUrl [ "list-horizons" ] [ ]
        , expect = Http.expectJson
                   (\r -> (Horizons ( GotHorizons r )))
                   catalogDecoder
        }


getUsers: String -> Cmd Msg
getUsers baseUrl =
    Http.get
        { url = UB.crossOrigin baseUrl [ "api", "permissions", "user_roles" ] [ ]
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
            Err ( BadUrl url )

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

saveHorizons: String -> HorizonModel -> Cmd Msg
saveHorizons baseUrl model =
    Http.post
    { url = baseUrl ++ "replace-horizons"
    , body = Http.jsonBody
                ( catalogEncode model )
    , expect = expectStringDetailed (\r -> (Horizons ( Saved r )))
    }


saveUser: String ->  ( String, User ) -> Cmd Msg
saveUser baseUrl ( name, user ) =
    Http.request
        { url = UB.crossOrigin
              baseUrl
              [ "api", "permissions", "user_roles" ]
              [ UB.string "userid" <|
                    Maybe.withDefault name user.editedEmail
              , UB.string "rolename" <|
                  Maybe.withDefault user.role user.editedRole
              ]
        , body = Http.emptyBody
        , headers = []
        , method = "PATCH"
        , expect =  Http.expectString ( \r -> Users ( UserSaved r ) )
        , timeout = Nothing
        , tracker = Nothing
        }


update: Msg -> Model -> ( Model, (Cmd Msg) )
update msg model =
    case msg of
        GotPro (Ok _) ->
            ( { model | isPro = True }
            , getUsers model.baseUrl
            )

        GotPro (Err _) ->
            ( { model | isPro = False }, Cmd.none )

        GotPermissions (Ok rawcanwrite) ->
            case JD.decodeString JD.bool rawcanwrite of
                Ok canwrite ->
                    U.nocmd { model | canwrite = canwrite }
                Err err -> U.nocmd model

        GotPermissions (Err err) ->
            U.nocmd model
            
        ChangeTab tab ->
            U.nocmd { model | tab = tab }

        Horizons hMsg ->
            let ( hModel, cmd ) =
                    updateHorizons model.baseUrl model.horizonModel hMsg
            in
            ( { model | horizonModel = hModel}
            , cmd
            )

        Users uMsg ->
            let ( uModel, cmd ) =
                    updateUsers model.baseUrl model.userModel uMsg
            in
            ( { model | userModel = uModel}
            , cmd
            )


updateHorizons: String -> HorizonModel -> HorizonMsg -> ( HorizonModel, Cmd Msg)
updateHorizons baseUrl model msg =
    case msg of
        GotHorizons (Ok stuffs) ->
            U.nocmd { model | horizons = Array.fromList stuffs }

        GotHorizons (Err emsg) ->
            U.nocmd model

        UserInput ( index, name ) value ->
            U.nocmd { model | horizons = updateFromUser model.horizons index name value }

        AddRow ->
            U.nocmd {model | horizons =  addRow model.horizons }

        Up index ->
            U.nocmd { model | horizons = permut model.horizons index ( index - 1 )}

        Down index ->
            U.nocmd { model | horizons = permut model.horizons index ( index + 1 )}

        Remove index ->
            U.nocmd <| isolateDelete
                { model | horizons = updateFromUser model.horizons index "action" "delete" }

        Save -> ( model, saveHorizons baseUrl model )

        Saved (Ok _) ->
            let newModel =
                    { model
                        | message = "New definitions saved"
                        , horizons = Array.empty
                        , toDelete = Array.empty
                    }
            in
            ( newModel
            , getHorirzons baseUrl newModel
            )

        Saved (Err error) ->
            U.nocmd { model | message = unpackError error }


updateUsers : String -> UserModel -> UserMsg -> ( UserModel, Cmd Msg )
updateUsers baseUrl model msg =
    case msg of
        GotUsers ( Ok payload ) ->
            let
                previsousEdition =
                    Dict.map
                        (\ n u -> Maybe.withDefault "" u.editedRole)
                        <| Dict.filter isEdited model.users
            in
            U.nocmd { model | users = applyPrevious payload previsousEdition }

        GotUsers ( Err _ ) ->
            U.nocmd model

        ChangeRole name role ->
            let user =
                    Maybe.withDefault errorUser ( Dict.get name model.users )
                changedUser =
                    { user | editedRole = Just role }
                    
            in
            U.nocmd { model | users = Dict.insert name changedUser model.users }

        ChangeMail name mail ->
            let user =
                    Maybe.withDefault errorUser ( Dict.get name model.users )
                changedUser =
                    { user | editedEmail = Just mail }
            in
            U.nocmd { model | users =  Dict.insert name changedUser model.users }

        Cancel name ->
            U.nocmd <| resetUser model name

        CreateUser ->
            U.nocmd { model | users = Dict.insert "" emptyUser model.users }

        RemoveNewUser name ->
            let
                removed = Dict.remove name model.users
            in
            U.nocmd { model | users = removed }

        SaveSingle name ->
            let user =
                    Maybe.withDefault errorUser ( Dict.get name model.users )
            in
            ( resetUser model name
            , ( saveUser baseUrl ) ( name, user )
            )

        SaveUsers ->
            let updated =
                    Dict.filter isEdited model.users
                reseted =
                    Dict.map
                        ( \ _ u  -> { u | editedEmail = Nothing, editedRole = Nothing } )
                        model.users
            in
            ( { model | users = reseted }
            , Cmd.batch
                <| List.map
                    ( saveUser baseUrl )
                    ( Dict.toList updated )
            )

        UserSaved ( Ok _ ) ->
            ( model
            , getUsers baseUrl
            )

        UserSaved ( Err _ ) ->
            U.nocmd model


applyPrevious: Dict String User -> Dict String String -> Dict String User
applyPrevious payload previous =
    Dict.merge
        ( \_ _ r -> r )
        ( \ name pay pre res ->
            Dict.insert name { pay | editedRole = Just pre } res
        )
        ( \ name pre res ->
              Dict.insert name { emptyUser| editedRole = Just pre } res
        )
        payload
        previous
        payload


resetUser: UserModel -> String -> UserModel
resetUser model name =
    let
        user =
            Maybe.withDefault errorUser ( Dict.get name model.users )
        changedUser =
            { user
                | editedEmail = Nothing
                , editedRole = Nothing
            }
    in
    { model | users = Dict.insert name changedUser model.users }


permut: Array.Array a -> Int -> Int -> Array.Array a
permut array i1 i2 =
    let
        temp1 = Array.get i1 array
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


isolateDelete: HorizonModel -> HorizonModel
isolateDelete model =
    let
        activeRecords =
            List.filter
                (\ rec -> rec.action /= Delete)
                ( Array.toList model.horizons )
        deletedRecords =
            ( Array.toList model.toDelete ) ++
            ( List.filter
                  (\ rec -> rec.action == Delete)
                  ( Array.toList model.horizons )
            )
    in
    { model
        | horizons = Array.fromList activeRecords
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
            ( Array.toIndexedList horizons )
       )


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


viewHorizonRows: HorizonModel -> List ( Html Msg )
viewHorizonRows model =
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
                , onInput (\ s ->
                    Horizons ( UserInput ( index, "label" ) s ))
                ]
                []
            ]
        , td
             [ class "settings-date" ]
             [  input
                [ value record.from
                , onInput (\ s ->
                    Horizons ( UserInput ( index, "from" ) s ))
                ]
                []
            ]
        , td
            [ class "settings-date" ]
            [ input
                [ value record.to
                , onInput (\ s ->
                    Horizons ( UserInput ( index, "to" ) s ))
                ]
                []
            ]
        , td
            []
            [ button
                [onClick ( Horizons (Up index))]
                [text "↑"]
            , button
                [onClick ( Horizons (Down index))]
                [text "↓"]
            , button
                [onClick ( Horizons (Remove index))]
                [text "❌"]]
        ]

viewHeader: Html Msg
viewHeader =
    thead
        []
        [ tr []
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
                , onClick ( Horizons AddRow ) ]
                [ text " + " ]
            ]
        ]

rowUser: List String -> ( String,  User ) -> Html Msg
rowUser choices ( name, user) =
    let
        visible =
            if isEdited name user
            then ""
            else "invisible"
        newClass =
            if user.new
            then ""
            else "invisible"
    in
    tr
        []
        [ td
            [ class "settings-label"  ]
            [ if user.new
              then
                input
                [ value <| Maybe.withDefault "" user.editedEmail
                , onInput (\s -> ( Users ( ChangeMail name s )))
                ] []
              else
                p [] [ text name ]
            ]
        , td
            []
             [ dropDownRole choices user.new name
                   <| Maybe.withDefault user.role user.editedRole
             ]
         , td
            []
            [ button
                [ class "btn btn-success"
                , class visible
                , onClick ( Users ( SaveSingle name ) )
                ]
                [ text "save" ]
            , button
                [ class "btn btn-warning"
                , class visible
                , onClick ( Users ( Cancel name ) )
                ]
                [ text "cancel" ]
            , button
                [ class "btn btn-danger"
                , class newClass
                , onClick ( Users ( RemoveNewUser name ) )
                ]
                [ text "remove" ]
            ]
        ]

lastRow: UserModel -> List ( Html Msg )
lastRow model =
    let visible =
            List.any
                (\  u -> u.editedRole /= Nothing )
                ( Dict.values model.users )
    in
    [ tr
        []
        [ td [] []
        , td [] []
        , td
            []
             [ button
                [ class "btn btn-success"
                , class <| if visible
                            then ""
                            else "invisible"
                , onClick ( Users SaveUsers )
                ]
                [ text "save all" ]
            , button
                [ class "btn btn-warning"
                , class "invisible"
                ]
                [ text "cancel" ]
            , button
                [ class "btn btn-danger"
                , class "invisible"
                ]
                [ text "remove" ]
            ]
        ]
    ]


isEdited: String -> User -> Bool
isEdited name user =
        case user.editedRole of
            Just _ -> True
            Nothing -> False


dropDownRole: List String -> Bool -> String ->  String -> Html Msg
dropDownRole choices new name role  =
    let
        decodeRole: String -> JD.Decoder Msg
        decodeRole r =
            JD.succeed ( Users ( ChangeRole name r ))
        moreChoices =
            if new
            then "" :: choices
            else choices
    in
    select
        [ on "change" (JD.andThen decodeRole targetValue )]
        ( List.map (renderRole role) moreChoices )


renderRole : String -> String -> Html msg
renderRole selectedRole role =
    option
        [ value role
        , selected (selectedRole == role)
        ]
        [ text role ]


viewHorizons: HorizonModel -> Html Msg
viewHorizons model =
    div
        [ class "horizons"
        , class "sub-settings"
        ]
        [ table
            [ class "table" ]
            ( [ viewHeader ]
            ++ ( viewHorizonRows model )
            ++ [ tableFooter ] )
        , br [] []
        , div
            []
            [ button
                [ class "btn btn-success update"
                , onClick ( Horizons Save ) ]
                [ text "Update definitions" ]
            ]
        , div
            []
            [ text model.message ]
        ]



headerUsers : Html Msg
headerUsers =
    thead
        []
        [ tr
            []
            [ th [] [ text "Email" ]
            , th [] [ text "Role" ]
            , th [] []
            ]
        ]


viewUsers: UserModel -> Bool -> Html Msg
viewUsers model isPro =
    if not isPro
    then div [] [ text "Only available on the pro version"
                , br [] []
                , text "Please check: "
                , a
                    [ href "https://timeseries.pythonian.fr/" ]
                    [ text "our site"]
                ]
    else
    div
        [ class "users"
        , class "sub-settings"
        ]
        [ table
            [ class "table" ]
            ([ headerUsers
             ] ++ ( List.map
                    ( rowUser model.roleChoices )
                    <| Dict.toList
                        <|Dict.filter
                            (\ _ u -> not u.new )
                            model.users
                  )
             ++ ( List.map
                    ( rowUser model.roleChoices )
                    <| Dict.toList
                        <|Dict.filter
                            (\ _ u -> u.new )
                            model.users
                 )
               ++ ( lastRow model )
             )
        ,  button
            [ class "btn btn-success update"
            , onClick ( Users CreateUser ) ]
            [ text "Create New User" ]
        ]


tabSelector: Model -> Html Msg
tabSelector model =
    ul
        [ class "nav nav-tabs"]
        <| List.map
            (\ t ->
                li
                 [ class "nav-item"
                 , attribute "role" "tablist"
                 ]
                 [ a
                    [ class "nav-link"
                    , class ( if t == model.tab then "active" else "" )
                    , attribute "data-toggle" "tab"
                    , attribute "role" "tab"
                    , onClick ( ChangeTab t )
                    ]
                    [ text ( tabLabel t )]
                 ]
            )
            tabList


view: Model -> Html Msg
view model =
   div
    [ class "settings"]
    [ h1
        [ class "page-title" ]
        [ text "Settings" ]
    , tabSelector model
    , case model.tab of
        HorizonTab -> viewHorizons model.horizonModel
        UserTab -> viewUsers model.userModel model.isPro
    ]


getPro: String -> Cmd Msg
getPro baseUrl =
    Http.get
        { url = UB.crossOrigin baseUrl [ "ispro" ] [ ]
        , expect = Http.expectString GotPro
        }


init: String -> ( Model, ( Cmd Msg ))
init baseurl =
    let
        model = 
            { baseUrl = baseurl
            , canwrite = False
            , tab = HorizonTab
            , isPro = False
            , horizonModel = emptyHorizonModel
            , userModel = emptyUserModel
            }
    in
    ( model
    , Cmd.batch
        [ getHorirzons baseurl model.horizonModel
        , getPro baseurl
        , I.getwriteperms baseurl GotPermissions
        ]
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
