module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Attributes exposing
    ( attribute
    , checked
    , class
    , draggable
    , id
    , dropzone
    , type_
    , style
    , tabindex
    , value
    )
import Html.Events as Events
import Html.Events exposing
    ( on
    , onCheck
    , onClick
    )
import Set exposing (Set)
import Tree
import Tree exposing
    ( Tree
    , indexedMap
    , mapAccumulate
    , restructure
    , tree
    )
import Tree.Zipper
import Tree.Zipper exposing
    ( Zipper
    , forward
    , fromTree
    , toTree
    )


type MyTree = MyTree ( Dict String  MyTree )
type alias Path = String

root = "root"

type alias Payload =
    { name: String
    , path: Path
    , open: Bool
    , status: LoadingStatus
    , series: Set String
    , selected : Set String
    , cut: Set String
    }

initPayload: Payload
initPayload =
    { name= ""
    , path = ""
    , open = False
    , status = Start
    , series = Set.empty
    , selected = Set.empty
    , cut = Set.empty
    }

type LoadingStatus =
    Start
    | Loading
    | Success
    | LoadingError


type MsgTree
    = Open Path Bool
    | DragStart String Path
    | DragOver Path
    | DragEnd
    | Drop Path
    | Select Path String
    | Deselect Path String
    | Focus Path


type Cut
    = NoCut
    | Cut Path ( Set String )


-- ref for drag/drop: https://benpaulhanna.com/basic-html5-drag-and-drop-with-elm.html

onDragStart msg =
    Events.on "dragstart"
        <| JD.succeed msg

onDragEnd msg =
    Events.on "dragend"
        <| JD.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover"
        <| JD.succeed (msg, True)

onDrop msg =
    Events.preventDefaultOn "drop"
        <| JD.succeed (msg, True)


decodeTree: JD.Decoder ( List String )
decodeTree =
    JD.list JD.string


emptyMTree = MyTree Dict.empty

emptyTree: Tree Payload
emptyTree = tree initPayload []


stepTree: String -> MyTree -> MyTree
stepTree label previsouTree =
    MyTree
    <| Dict.singleton
        label
        previsouTree

buildSingle : String -> MyTree
buildSingle path =
    List.foldr
        stepTree
        ( MyTree Dict.empty )
        ( String.split "." path )


convertTree : MyTree -> Tree Payload
convertTree myTree =
    Maybe.withDefault
        ( tree initPayload [] )
        <| List.head
            <| convertTreeT ""
                <| MyTree ( Dict.singleton root myTree )


convertTreeT : String -> MyTree -> List ( Tree Payload )
convertTreeT path myTree =
    case myTree of
        MyTree dict ->
            List.map
                ( recConvert path )
                ( Dict.toList dict )


recConvert: Path -> ( String,  MyTree ) -> Tree Payload
recConvert path ( name,  subTree) =
    let newPath = String.replace
                    ".root."
                    ""
                    ( path ++ "." ++ name )
    in
    tree
        { initPayload | name=name
                      , path = newPath
        }
        ( convertTreeT newPath subTree )


unpack: MyTree -> ( Dict String  MyTree )
unpack mTree =
    case mTree of
        MyTree dict -> dict

mergeMBranch: MyTree -> MyTree -> MyTree
mergeMBranch branch tree =
    let dictT = unpack tree
        dictB = unpack branch
    in
        case Dict.toList dictB of
            [] -> tree
            (k, v) :: xs ->
                case Dict.get k dictT of
                    Nothing -> MyTree ( Dict.insert k v dictT )
                    Just subTree -> MyTree
                                        <| Dict.insert
                                                k
                                                (mergeMBranch v subTree)
                                                dictT

buildMTree : List String -> MyTree
buildMTree paths =
    let branchs = List.map buildSingle paths
    in
        List.foldr
            mergeMBranch
            emptyMTree
            branchs

buildTree : List String -> Tree Payload
buildTree paths =
    convertTree
        <| buildMTree paths


zHeight path =
 if path == "." ++ "root"
 then "0"
 else
     String.fromInt
        <| String.length
            path

toListItems : Maybe Path -> ( MsgTree -> msg ) -> Maybe Path -> Cut -> Payload -> List (Html msg) -> Html msg
toListItems overDrag convertMsg focus cut payload children =
    let open = payload.open
    in
        Html.li
            [ class "folder-and-series"
            , attribute "data-path" payload.path
            ]
            ([ Html.div
                [ class "node"
                , tabindex 0
                , onClick <| convertMsg ( Focus payload.path )
                , class <| case focus of
                        Nothing -> ""
                        Just f
                            -> if f == payload.path
                                then "focused"
                                else ""
                ]
                ([ viewFolder overDrag payload open convertMsg
                 ]++ if open
                    then [ viewSelected payload cut convertMsg
                         , viewSeries overDrag payload convertMsg
                         ]
                    else  []
                )
             ] ++ if open
                    then
                        [ Html.ul
                            [ class "children sub-folders-list" ]
                            children
                        ]
                    else []
            )


viewFolder: Maybe Path -> Payload -> Bool -> ( MsgTree -> msg ) -> Html msg
viewFolder overDrag payload open convertMsg =
    Html.div
        [ class "folder-row"
        , class ( if open then "open" else "not-open" )
        , class ( classOver payload overDrag )
        , onDragOver <| convertMsg (DragOver payload.path)
        , onDrop <| convertMsg ( Drop payload.path )
        ]
        [ buttonOpen Open payload convertMsg
        , Html.p
            [ class "folder-name"
            , onClick <| convertMsg ( Open payload.path ( not open ) )
            ]
            [ Html.text payload.name
            ]
        , Html.button
            [ class "folder-action"
            , attribute "popovertarget" ( "action-" ++ payload.path )
            , style "anchor-name" ( "action-button-" ++ payload.path )
            ]
            [ Html.text "..."
            , Html.ul
                [ class "action-box"
                , attribute "popover" ""
                , style "position-anchor" ( "action-button-" ++ payload.path )
                , id ( "action-" ++ payload.path )
                ]
                [ Html.li
                    []
                    [ Html.text "Rename"]
                , Html.li
                    []
                    [ Html.text "Delete"]
                ]
            ]
        ]



buttonOpen: (Path -> Bool -> MsgTree) -> Payload -> ( MsgTree -> msg ) -> Html msg
buttonOpen openMsg payload convertMsg =
    Html.button
        [ onClick <| convertMsg
                        (openMsg payload.path (not payload.open))
        , class "button-switch"
        , class <| if payload.open
                    then "closer"
                    else "opener"
        ]
        [ Html.text <| if payload.open
                        then "-"
                        else "+"
        ]


viewSelected: Payload -> Cut -> ( MsgTree -> msg ) -> Html msg
viewSelected payload cut convertMsg  =
     Html.ul
        [ class "series-list"
        , class <| case cut of
                    NoCut -> "selected"
                    Cut path _ ->
                        if path == payload.path
                            then "cut"
                            else "selected"
        , onClick <| convertMsg ( Focus payload.path )
        ]
        <| List.map
            (\sn -> Html.li
                        [ class "series-item" ]
                        [ Html.button
                            [ onClick
                                <| convertMsg
                                    <| Deselect payload.path sn ]
                            [Html.text "▼"]
                        , Html.p
                            [ class "series-name"
                            ]
                            [ Html.text sn ]]

            )
            <| Set.toList
                payload.selected


viewSeries : Maybe Path-> Payload -> (MsgTree -> msg) -> Html msg
viewSeries overDrag payload convertMsg =
    Html.ul
        [ class "series-list"
        ,onDragOver
            <| convertMsg
                <| DragOver payload.path
        , onDrop
            <| convertMsg
                <| Drop payload.path
        , onClick
            <| convertMsg
                <| Focus payload.path
        ]
        <| List.map
            (\sn -> Html.li
                        [ class "series-item" ]
                        [ Html.button
                            [ onClick
                                <| convertMsg
                                    <| Select payload.path sn ]
                            [Html.text "▲"]
                        , Html.p
                            [ class "series-name"
                            , draggable "true"
                            , class ( classOver payload overDrag )
                            , onDragStart
                                <| convertMsg
                                    <| DragStart
                                        sn
                                        payload.path

                            ]
                            [ Html.text sn ]]
            )
            <| Set.toList
                <| Set.diff
                    payload.series
                    payload.selected


classOver: Payload -> Maybe Path -> String
classOver payload overDrag =
    case overDrag of
        Nothing -> ""
        Just path ->
            if payload.path == path
            then "targeted"
            else ""


viewTree: Tree Payload -> Maybe Path -> ( MsgTree -> msg) -> Maybe Path -> Cut -> Html msg
viewTree tree overDrag convertMsg focus cut =
    Html.ul
        [class "folders-list"]
        [ restructure
            identity
            ( toListItems overDrag convertMsg focus cut )
            tree
        ]


getZipper : String -> Zipper Payload -> Maybe ( Zipper Payload )
getZipper path menu =
    if (Tree.Zipper.label menu).path == path
        then Just menu
        else
            case ( forward menu ) of
                Nothing -> Nothing
                Just nextStep ->
                    getZipper
                        path
                        nextStep


getPayload: String -> Tree Payload -> Payload
getPayload path menu =
    Maybe.withDefault
        initPayload
        <| Maybe.map
            (Tree.Zipper.label)
            (getZipper path (fromTree menu))


mutePayload : String -> (Payload -> Payload) -> Tree Payload -> Tree Payload
mutePayload path mapping menu =
    case getZipper path ( fromTree menu ) of
        Nothing -> menu
        Just zipper
            -> toTree
                <| Tree.Zipper.mapLabel
                        mapping
                        zipper


cutSeries: Path -> Tree Payload -> ( Tree Payload, Set String )
cutSeries path menu =
    let cut = (getPayload path menu).selected
    in
        ( mutePayload
            path
            (\ p -> { p | selected = Set.empty
                        , series = Set.diff p.series cut
                    }
            )
            menu
        , cut
        )


pasteSeries: Path -> Path -> Set String -> Tree Payload -> Tree Payload
pasteSeries from to cut menu =
    mutePayload
        to
        (\ p -> { p | series = Set.union p.series cut })
        <| mutePayload
            from
            (\ p -> { p | selected = Set.empty
                        , series = Set.diff p.series cut
                    }
            )
            menu