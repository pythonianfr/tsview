module FoldersUtil exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Html exposing
    ( Html )
import Html.Attributes exposing
    ( attribute
    , class
    , disabled
    , draggable
    , id
    , placeholder
    , style
    , tabindex
    , title
    , value
    )
import Html.Events as Events
import Html.Events exposing
    ( onClick
    , onInput
    )
import Set exposing (Set)
import Tree
import Tree exposing
    ( Tree
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
type Path
    = Root
    | Branch String

root = "root"


reprPath: Path -> String
reprPath path =
    case path of
        Root -> root
        Branch branch -> branch


type alias Payload =
    { name: String
    , path: Path
    , open: Bool
    , status: LoadingStatus
    , series: Dict String SeriesAttribute
    , query: String
    }

type alias SeriesAttribute
     = { selected: Bool }

initPayload: Payload
initPayload =
    { name= root
    , path = Root
    , open = False
    , status = Start
    , series = Dict.empty
    , query = ""
    }

type LoadingStatus =
    Start
    | Loading
    | Success
    | LoadingError


type MsgTree
    = Open Path Bool
    | DragStart Path ( Set String )
    | DragOver Path
    | DragEnd
    | Drop Path
    | Select Path String
    | Deselect Path String
    | Focus Path
    | Query Path String
    | ButtonCut Path
    | ButtonPaste Path
    | ButtonReset Path
    | ShowRoot Bool
    | Delete Path


type Drag
    = NoDrag
    | Drag Path ( Set String )

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


decodeFind: JD.Decoder ( List String )
decodeFind =
    JD.list ( JD.at ["name"] JD.string )

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
    tree initPayload
            <| convertTreeT
                Nothing
                myTree


convertTreeT : Maybe String -> MyTree -> List ( Tree Payload )
convertTreeT path myTree =
    case myTree of
        MyTree dict ->
            List.map
                ( recConvert path )
                ( Dict.toList dict )


recConvert: Maybe String -> ( String,  MyTree ) -> Tree Payload
recConvert path ( name, subTree) =
    let newPath = case path of
                    Nothing -> name
                    Just p -> ( p ++ "." ++ name )
    in
    tree
        { initPayload | name = name
                      , path = Branch newPath
        }
        ( convertTreeT ( Just newPath ) subTree )


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

selectFromUser: Payload -> Set String
selectFromUser payload =
    Set.fromList
        <| Dict.keys
           <| Dict.filter
                (\ k v -> v.selected)
                payload.series

toListItems : Maybe Path -> ( MsgTree -> msg ) -> Maybe Path -> Cut -> Bool -> Payload -> List (Html msg) -> Html msg
toListItems overDrag convertMsg focus cut showRoot payload children =
    let open = payload.open
        isRoot = case payload.path of
                    Root -> True
                    Branch _ -> False
    in
        Html.li
            [ class "folder-and-series"
            , attribute "data-path" ( reprPath payload.path )
            ]
            ([ Html.div
                [ class "node"
                , tabindex 0
                , onDragOver <| convertMsg (DragOver payload.path)
                , onDrop <| convertMsg ( Drop payload.path )
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
                      then
                        ( if isRoot
                          then buttonShowRoot convertMsg showRoot
                          else []
                        ) ++
                        if ( not isRoot ) || showRoot
                        then
                          [ viewSelector payload cut convertMsg
                          , viewSelected payload cut convertMsg
                          , viewSeries overDrag payload convertMsg
                          ]
                        else
                          []
                      else []
                )
             ] ++ if open
                    then
                        [ Html.ul
                            [ class "children sub-folders-list"
                            , class <| case focus of
                                Nothing -> ""
                                Just f
                                    -> if f == payload.path
                                        then "highlight"
                                        else ""
                            ]
                            children
                        ]
                    else []
            )


viewSelector: Payload -> Cut -> ( MsgTree -> msg ) -> Html msg
viewSelector payload cut convertMsg =
    Html.div
        [class "filter-name"]
        [ Html.input
            [ placeholder "filter series (>2 chars)"
            , title "at least 3 characters"
            , onInput (\ s ->  convertMsg ( Query payload.path s ))
            , value payload.query
            ]
            []
        , Html.button
            [ onClick ( convertMsg ( ButtonCut payload.path ))
            , disabled (not ( anySelected payload ))
            ]
            [ Html.text "Cut" ]
        , Html.button
            [ onClick ( convertMsg ( ButtonPaste payload.path ))
            , disabled <| case cut of
                            NoCut -> True
                            Cut path _ -> path == payload.path
            ]
            [ Html.text "Paste" ]
                , Html.button
            [ onClick ( convertMsg ( ButtonReset payload.path ))
            , disabled ( not ( anyAction payload ))
            ]
            [ Html.text "Deselect" ]
        ]

viewFolder: Maybe Path -> Payload -> Bool -> ( MsgTree -> msg ) -> Html msg
viewFolder overDrag payload open convertMsg =
    let isRoot = case payload.path of
                    Root -> True
                    Branch _ -> False
    in
    Html.div
        [ class "folder-row"
        , class ( if open then "open" else "not-open" )
        , class ( classOver payload overDrag )
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
            , attribute "popovertarget" ( "action-" ++ ( reprPath payload.path ) )
            , style "anchor-name" ( "action-button-" ++ ( reprPath payload.path ) )
            ]
            [ Html.text "..."
            , Html.ul
                [ class "action-box"
                , attribute "popover" ""
                , style "position-anchor" ( "action-button-" ++ ( reprPath payload.path ) )
                , id ( "action-" ++ ( reprPath payload.path ) )
                ]
                ([ Html.li
                    []
                    [ Html.text "Create"]
                 ] ++
                 ( if isRoot
                    then []
                    else
                        [ Html.li
                            []
                            [ Html.text "Rename"]
                        , Html.li
                            [ onClick <| convertMsg
                                            ( Delete payload.path )
                            ]
                            [ Html.text "Delete" ]
                        ]
                    )
                 )
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


buttonShowRoot :  ( MsgTree -> msg ) -> Bool -> List ( Html msg )
buttonShowRoot convertMsg showRoot =
    [ Html.button
        [ class "button-show-root"
        , onClick <| convertMsg
                        (ShowRoot ( not showRoot ))
        ]
        [ Html.text <| if showRoot
                        then "Hide"
                        else "Show unclassified"
        ]
    ]

viewSelected: Payload -> Cut -> ( MsgTree -> msg ) -> Html msg
viewSelected payload cut convertMsg  =
     Html.ul
        [ class "series-list"
        , draggable "true"
        , onDragStart
            <| convertMsg
                <| DragStart
                    payload.path
                    ( selectFromUser payload )
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
                            [ title "deselect series"
                            , onClick
                                <| convertMsg
                                    <| Deselect payload.path sn ]
                            [ Html.text "▼" ]
                        , Html.p
                            [ class "series-name"
                            ]
                            [ Html.text sn ]]

            )
            <| Set.toList
                ( selectFromUser payload )


viewSeries : Maybe Path-> Payload -> (MsgTree -> msg) -> Html msg
viewSeries overDrag payload convertMsg =
    Html.ul
        [ class "series-list"
        ]
        <| List.map
            (\sn -> Html.li
                        [ class "series-item" ]
                        [ Html.button
                            [ title "select series"
                            , onClick
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
                                        payload.path
                                        ( Set.singleton sn )
                            ]
                            [ Html.text sn ]]
            )
            <| Dict.keys
                <| Dict.filter
                    (\ k v -> not v.selected)
                    payload.series


classOver: Payload -> Maybe Path -> String
classOver payload overDrag =
    case overDrag of
        Nothing -> ""
        Just path ->
            if payload.path == path
            then "targeted"
            else ""


viewTree: Tree Payload -> Maybe Path -> ( MsgTree -> msg) -> Maybe Path -> Cut -> Bool -> Html msg
viewTree tree overDrag convertMsg focus cut showRoot =
    Html.ul
        [class "folders-list"]
        [ restructure
            identity
            ( toListItems overDrag convertMsg focus cut showRoot )
            tree
        ]


getZipper : Path -> Zipper Payload -> Maybe ( Zipper Payload )
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


getPayload: Path -> Tree Payload -> Payload
getPayload path menu =
    Maybe.withDefault
        initPayload
        <| Maybe.map
            (Tree.Zipper.label)
            (getZipper path (fromTree menu))


mutePayload : Path -> (Payload -> Payload) -> Tree Payload -> Tree Payload
mutePayload path mapping menu =
    case getZipper path ( fromTree menu ) of
        Nothing -> menu
        Just zipper
            -> toTree
                <| Tree.Zipper.mapLabel
                        mapping
                        zipper


dressSeries : List String -> Dict String SeriesAttribute
dressSeries names =
    Dict.fromList
        <| List.map
            (\ name ->
                (name, { selected = False })
            )
            names


pasteSeries: Path -> Path -> Set String -> Tree Payload -> Tree Payload
pasteSeries source destination cut menu =
    let fromCut = dressSeries ( Set.toList cut )
    in
    mutePayload
        destination
        (\ p -> { p | series = Dict.union p.series fromCut })
        <| mutePayload
            source
            (\ p -> { p | series = Dict.diff p.series fromCut
                    }
            )
            menu


filterByWords: List String -> String -> List String
filterByWords filterme query =
    let
        querywords =
            String.words query
        filterstep word wordlist =
            List.filter
                (\item -> String.contains word item)
                wordlist
        filterall words wordlist =
            case words of
                [] -> wordlist
                head::tail -> filterall tail <| filterstep head wordlist
    in filterall querywords filterme


selectFromQuery: Payload -> Payload
selectFromQuery payload =
    let selected
            = if String.length payload.query < 3
                then []
                else filterByWords
                        ( Dict.keys payload.series )
                        payload.query
    in { payload | series =
                    Dict.map
                        ( \k v -> if List.member k selected
                                    then { v | selected = True }
                                    else { v | selected = False }
                        )
                        payload.series

       }


anySelected: Payload -> Bool
anySelected payload =
    List.any
        ( \ a -> a.selected)
        <| Dict.values payload.series

anyAction: Payload -> Bool
anyAction payload =
    anySelected payload
    || payload.query /= ""

