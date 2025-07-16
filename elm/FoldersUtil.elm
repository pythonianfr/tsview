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
    , href
    , id
    , placeholder
    , style
    , tabindex
    , target
    , title
    , value
    )
import Html.Events as Events
import Html.Events exposing
    ( onClick
    , onFocus
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
import Url.Builder as UB

type MyTree = MyTree ( Dict String  MyTree )
type Path
    = Root
    | Branch String
    | Unclassified

root = "root"
unclassified = "unclassified"



reprPath: Path -> String
reprPath path =
    case path of
        Root -> root
        Branch branch -> branch
        Unclassified -> unclassified


pathFromString : String -> Path
pathFromString repr =
    if repr == root
    then Root
    else
    if repr == unclassified
    then Unclassified
    else Branch repr

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
    { name = root
    , path = Root
    , open = False
    , status = Start
    , series = Dict.empty
    , query = ""
    }

unclassifiedPayload: Payload
unclassifiedPayload =
    { initPayload
    | name = unclassified
    , path = Unclassified
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
    | Delete Path
    | CreationName String
    | Create Path


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
    tree
        initPayload
        (( convertTreeT
            Nothing
            myTree
         ) ++ [ tree
                { initPayload
                    | name = unclassified
                    , path = Unclassified
                }
                []
              ]
        )


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

toListItems : String -> Maybe Path -> ( MsgTree -> msg ) -> Maybe Path -> Cut -> Payload -> List (Html msg) -> Html msg
toListItems baseUrl overDrag convertMsg focus cut payload children =
    let open = payload.open
        dropable = case payload.path of
                    Root -> False
                    Branch _ -> True
                    Unclassified -> True
    in
        Html.li
            [ class "folder-and-series"
            , attribute "data-path" ( reprPath payload.path )
            ]
            ([ Html.div
                ([ class "node"
                , class ( classOver payload overDrag )
                 , tabindex 0
                 ] ++ if  not dropable
                     then []
                     else
                        [ onDragOver <| convertMsg (DragOver payload.path)
                        , onDrop <| convertMsg ( Drop payload.path )
                        , onClick <| convertMsg ( Focus payload.path )
                        , class <| case focus of
                                Nothing -> ""
                                Just f
                                    -> if f == payload.path
                                        then "focused"
                                        else ""
                        ]
                )
                ([ viewFolder baseUrl overDrag payload open convertMsg
                 ]++ if open
                      then
                          [ viewSelector payload cut convertMsg
                          , viewSelected baseUrl payload cut convertMsg
                          , viewSeries baseUrl overDrag payload convertMsg
                          ]
                        else
                          []
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
    if payload.path == Root
    then Html.div [] []
    else
    Html.div
        [class "filter-name"]
        [ Html.input
            [ placeholder "filter series (>2 chars)"
            , title "at least 3 characters"
            , onInput (\ s ->  convertMsg ( Query payload.path s ))
            , onFocus ( convertMsg ( Query payload.path payload.query ))
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
        , Html.p
            [ style "display" "inline"
            , title "selected / total"
            ]
            [ Html.text ( buildCounter payload )]
        ]


viewFolder: String -> Maybe Path -> Payload -> Bool -> ( MsgTree -> msg ) -> Html msg
viewFolder baseUrl overDrag payload open convertMsg =
    let mutable = case payload.path of
                    Root -> False
                    Branch _ -> True
                    Unclassified -> False
    in
    Html.div
        [ class "folder-row"
        , class ( if open then "open" else "not-open" )
        ]
        ([ buttonOpen Open payload convertMsg
        , Html.p
            [ class "folder-name"
            , onClick <| convertMsg ( Open payload.path ( not open ) )
            ]
            [ Html.text payload.name
            ]
         ] ++
         if payload.path == Unclassified
         then []
         else
            [ folderActionButton payload mutable convertMsg
            , linkQuery baseUrl "tsview" "view" payload.path
            , linkQuery baseUrl "tseditor" "edition" payload.path
            ]
        )


folderActionButton: Payload -> Bool -> ( MsgTree -> msg ) -> Html msg
folderActionButton payload mutable convertMsg =
    Html.button
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
                [ title "Creation take effect when a series is moved into. Dots are removed"
                , onClick <| convertMsg
                                ( Create payload.path )
                ]
                [ Html.text "Create"
                , Html.div
                    []
                    [ Html.input
                        [ class "input-new-name"
                        , placeholder "name"
                        , onInput
                            (\ s ->  convertMsg
                                        ( CreationName s )
                            )
                        ]
                        []
                    , Html.button
                        [ class "validate-new-name" ]
                        [ Html.text "Ok" ]
                    ]
                ]
             ] ++
             ( if not mutable
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




buttonOpen: (Path -> Bool -> MsgTree) -> Payload -> ( MsgTree -> msg ) -> Html msg
buttonOpen openMsg payload convertMsg =
    Html.i
        [ onClick <| convertMsg
                        (openMsg payload.path (not payload.open))
        , class <| if payload.open
                    then "fa fa-folder-open"
                    else "fa fa-folder"
        ]
        []


viewSelected: String -> Payload -> Cut -> ( MsgTree -> msg ) -> Html msg
viewSelected basUrl payload cut convertMsg  =
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
                            , class "assign-series"
                            , onClick
                                <| convertMsg
                                    <| Deselect payload.path sn ]
                            [ Html.text "▼" ]
                        , Html.p
                            [ class "series-name"
                            ]
                            [ Html.text sn ]
                        , linkInfo basUrl sn
                        ]
            )
            <| Set.toList
                ( selectFromUser payload )


viewSeries : String -> Maybe Path-> Payload -> (MsgTree -> msg) -> Html msg
viewSeries baseUrl overDrag payload convertMsg =
    Html.ul
        [ class "series-list"
        ]
        <| List.map
            (\sn -> Html.li
                        [ class "series-item" ]
                        [ Html.button
                            [ title "select series"
                            , class "assign-series"
                            , onClick
                                <| convertMsg
                                    <| Select payload.path sn ]
                            [Html.text "▲"]
                        , Html.p
                            [ class "series-name"
                            , draggable "true"
                            , onDragStart
                                <| convertMsg
                                    <| DragStart
                                        payload.path
                                        ( Set.singleton sn )
                            ]
                            [ Html.text sn ]
                        , linkInfo baseUrl sn
                        ]
            )
            <| Dict.keys
                <| Dict.filter
                    (\ k v -> not v.selected)
                    payload.series


linkInfo: String -> String -> Html msg
linkInfo baseUrl sn =
    Html.a
        [ href
            <| UB.crossOrigin
                baseUrl
                ["tsinfo"]
                [ UB.string "name" sn ]
        , target "_blank"
        ]
        [ Html.text "info" ]


buildQuery: String -> String
buildQuery branch =
    "(by.at-path "
    ++ """ " """
    ++ branch
    ++ """ " """
    ++ ")"


linkQuery: String -> String -> String ->Path -> Html msg
linkQuery baseUrl page label path =
    case path of
        Unclassified -> Html.p [] []
        Root -> Html.p [] []
        Branch branch ->
            Html.a
                [ href
                    <| UB.crossOrigin
                        baseUrl
                        [page]
                        [ UB.string "query" (buildQuery branch) ]
                , target "_blank"
                ]
                [ Html.text label ]


classOver: Payload -> Maybe Path -> String
classOver payload overDrag =
    case overDrag of
        Nothing -> ""
        Just path ->
            if payload.path == path
            then "targeted"
            else ""


viewTree: String -> Tree Payload -> Maybe Path -> ( MsgTree -> msg) -> Maybe Path -> Cut -> Html msg
viewTree baseUrl tree overDrag convertMsg focus cut =
    Html.ul
        [class "folders-list"]
        [ restructure
            identity
            ( toListItems baseUrl overDrag convertMsg focus cut )
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


nbSelected : Payload -> Int
nbSelected payload =
    Dict.size
        <| Dict.filter
            (\ k v  -> v.selected)
            payload.series


buildCounter: Payload -> String
buildCounter payload =
    "("
    ++ String.fromInt ( nbSelected payload )
    ++ "/"
    ++ String.fromInt ( Dict.size payload.series )
    ++ ")"


anyAction: Payload -> Bool
anyAction payload =
    anySelected payload
    || payload.query /= ""


getOpenState: Tree Payload -> Set String
getOpenState menu =
    findOpen ( fromTree menu ) Set.empty


findOpen : Zipper Payload -> Set String -> Set String
findOpen menu opened =
    let payload = (Tree.Zipper.label menu)
        path = reprPath payload.path
        newOpen = if payload.open
                    then Set.insert path opened
                    else opened
    in
        case ( forward menu ) of
            Nothing -> newOpen
            Just nextStep ->
                    findOpen
                        nextStep
                        newOpen


setOpenState: Tree Payload -> Set String -> Tree Payload
setOpenState menu openState =
    toTree
        <| muteOpen
            ( fromTree menu )
            openState


muteOpen:  Zipper Payload -> Set String -> Zipper Payload
muteOpen menu openState =
    let payload = ( Tree.Zipper.label menu )
        path = reprPath payload.path
        newMenu = if Set.member path openState
                    then Tree.Zipper.mapLabel
                            (\ p -> { p | open = True})
                            menu
                    else menu
    in
    case ( forward newMenu ) of
        Nothing -> newMenu
        Just nextStep ->
            muteOpen
                nextStep
                openState
