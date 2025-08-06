module EdiTable exposing
    (..)

import Dict exposing (Dict)
import Set exposing (Set)


type Payload
    = Scalar FloatOrString
    | Complex Entry


type alias FloatOrString = ( ScalarType ( Maybe Float ) ( Maybe String ))


type ScalarType f s
    = MFloat f
    | MString s


type alias Entry =
    { raw : Maybe String
    , value : ( ScalarType ( Maybe Float ) ( Maybe String ) )
    , edition : Edited
    , editable: Bool
    , override : Bool
    , indexRow: String
    , indexCol: String
    , fromBatch: Bool
    }


type Edited
    = Edition ( ScalarType Float String )
    | NoEdition
    | Deletion
    | Error String


asEdited: (ScalarType (Maybe Float) (Maybe String)) -> Edited
asEdited scal =
    case scal of
        MFloat mf ->
            case mf of
                Nothing -> Deletion
                Just f -> Edition ( MFloat f )
        MString ms ->
            case ms of
                Nothing -> Deletion
                Just s -> Edition ( MString s )


dressSeries: Dict String Payload -> String -> Dict String Payload
dressSeries series name =
    Dict.map
        (\ k v  ->
            case v of
                Complex e -> Complex e
                Scalar s ->
                    Complex
                    { value = s
                   , override = False
                   , edition = asEdited s
                   , editable = True
                   , raw = case s of
                            MFloat mf -> Maybe.map String.fromFloat mf
                            MString ms -> ms
                   , indexCol = name
                   , indexRow = k
                   , fromBatch = False
                   }
        )
        series


patchCurrent : Dict String Payload -> Dict String Payload -> String -> Dict String Payload
patchCurrent base patch name  =
    -- base is supposed to be Complex
    -- patch is supposed to be Scalar
    let baseEntryF =
            { raw = Nothing
            , value = MFloat Nothing
            , edition = Deletion
            , editable = True
            , override = False
            , indexRow = ""
            , indexCol = name
            , fromBatch = True
            }
        baseEntryS =
            { baseEntryF | value = MString Nothing }
        default r = dressSeries r name
        leftOnly: String -> Payload -> Dict String Payload -> Dict String Payload
        leftOnly _ _ r = default r
        both: String -> Payload -> Payload -> Dict String Payload -> Dict String Payload
        both d ba pa r =
             case ba of
                Scalar _ -> dressSeries r name
                Complex b ->
                  case pa of
                      Complex _ -> dressSeries r name
                      Scalar sc ->
                          case sc of
                              MFloat mf ->
                                  case mf of
                                      Nothing ->
                                           Dict.insert
                                            d
                                            (Complex
                                                { b | raw = Nothing
                                                    , edition = Deletion
                                                }
                                            )
                                            r
                                      Just v ->
                                          Dict.insert
                                            d
                                            (Complex
                                                { b | raw = Just ( String.fromFloat v )
                                                    , edition = Edition ( MFloat v)
                                                }
                                            )
                                            r
                              MString ms ->
                                  case ms of
                                      Nothing ->
                                           Dict.insert
                                            d
                                            (Complex
                                                { b | raw = Nothing
                                                    , edition = Deletion
                                                }
                                            )
                                            r
                                      Just v ->
                                          Dict.insert
                                                    d
                                                    (Complex
                                                        { b | raw = Just v
                                                        , edition = Edition ( MString v )
                                                        }
                                                    )
                                                    r
        rightOnly : String -> Payload -> Dict String Payload -> Dict String Payload
        rightOnly d pa r =
            case pa of
                Complex _ -> default r
                Scalar p ->
                    case p of
                        MFloat mf ->
                            case mf of
                                Nothing -> Dict.insert
                                                d
                                                ( Complex
                                                    { baseEntryF | indexCol = d }
                                                )
                                                r
                                Just v -> Dict.insert
                                            d
                                            (Complex
                                                { baseEntryF | indexCol = d
                                                , raw = Just ( String.fromFloat v )
                                                , edition = Edition ( MFloat v)
                                                }
                                            )
                                            r
                        MString ms ->
                            case ms of
                                Nothing -> Dict.insert
                                                d
                                                ( Complex
                                                    { baseEntryS
                                                    | indexCol = d
                                                    }
                                                )
                                                r
                                Just v -> Dict.insert
                                            d
                                            ( Complex
                                                { baseEntryS
                                                | indexCol = d
                                                , raw = Just v
                                                , edition = Edition ( MString v )
                                                }
                                            )
                                            r
    in
    Dict.merge
        leftOnly
        both
        rightOnly
        base
        patch
        base


cartesianDataRec:  List ( List a ) ->  List a -> Int -> Int -> Int -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
cartesianDataRec mergedData currentRow minCol i j myDict =
    if j == minCol
    then
        case mergedData of
            [] -> myDict
            row :: rows ->
                case row of
                    [] -> myDict
                    cell :: cells ->
                        cartesianDataRec
                        rows
                        cells
                        minCol
                        i
                        ( j + 1 )
                        ( Dict.insert ( i, j ) cell myDict )

    else
        case currentRow of
            [] -> cartesianDataRec
                  mergedData
                  []
                  minCol
                  ( i + 1 )
                  minCol
                  myDict
            cell :: cells ->
                cartesianDataRec
                mergedData
                cells
                minCol
                i
                ( j + 1 )
                ( Dict.insert ( i, j ) cell myDict )


cartesianData: List ( List a )-> Dict ( Int, Int ) a
cartesianData mergedData =
    cartesianDataRec mergedData [] -1 -1 -1 Dict.empty


type CType
    = Primary
    | Formula
    | Auto


type Stuff
    = DateRow String
    | Header ( String, CType )
    | Cell Entry


parseInput : String -> Edited
parseInput value =
    if value == ""
        then Deletion
        else
            case String.toFloat <| String.replace "," "." value of
                Just val ->
                    Edition ( MFloat val )
                Nothing ->
                    Error value


parseString : String -> Edited
parseString value =
    if value == ""
        then Deletion
        else Edition ( MString value )


patchEntry: Stuff -> String ->Stuff
patchEntry stuff s =
    case stuff of
        DateRow date -> DateRow date
        Header h -> Header h
        Cell entry -> Cell { entry | raw = Just s
                                    , edition =
                                        case entry.value of
                                            MFloat _-> parseInput s
                                            MString _-> parseString s
                            }


pasteRectangle: Bool -> Dict ( Int, Int ) Stuff -> Dict ( Int, Int ) String -> ( Int, Int ) -> Dict ( Int, Int ) Stuff
pasteRectangle canwrite base patch ( cornerRow, cornerCol ) =
    let translatedPatch = Dict.fromList
                            <| List.map
                                (\ (( i, j ), v ) ->
                                    (( i + cornerRow, j + cornerCol ), v ))
                                ( Dict.toList patch )
    in
        Dict.merge
            ( \_ _ dict -> dict )
            ( \ position stuffBase sPatch dict ->
                case stuffBase of
                    DateRow _ -> dict
                    Header _ -> dict
                    Cell e ->
                        if canwrite && e.editable
                            then
                                Dict.insert
                                    position
                                    ( patchEntry stuffBase sPatch )
                                    dict
                            else dict
            )
            ( \_ _ dict -> dict )
            base
            translatedPatch
            base


type CompStatus
    = CompEmpty
    | CompLoaded
    | CompError


type alias OverSeries =
    { initialTs: Dict String Payload
    , zoomTs: Maybe (Dict String Payload)
    }


type alias OverComponent =
    { name: String
    , cType: CType
    , data: OverSeries
    , tzaware: Bool
    , status: CompStatus
    }

type alias Component =
    { name: String
    , cType: CType
    , data: Dict String Payload
    , tzaware: Bool
    , status: CompStatus
    }



toRaw: ( ScalarType (Maybe Float) (Maybe String) ) -> String
toRaw scal =
    case scal of
        MString s -> Maybe.withDefault "" s
        MFloat f -> Maybe.withDefault
                        ""
                        <| Maybe.map
                            String.fromFloat
                            f


zSeries : OverSeries -> Dict String Payload
zSeries overSeries =
    case overSeries.zoomTs of
        Nothing -> overSeries.initialTs
        Just zoomTs -> zoomTs

zComponent: OverComponent -> Component
zComponent over =
    let emptyComp = { name = over.name
                    , cType = over.cType
                    , tzaware = over.tzaware
                    , status = over.status
                    , data = Dict.empty
                    }
    in
    case over.data.zoomTs of
        Nothing -> { emptyComp | data = over.data.initialTs }
        Just zoomTs -> { emptyComp | data = zoomTs }


onlyActiveKeys : Dict String a -> List String
onlyActiveKeys series =
    Dict.keys series


getEntry: String ->  ( String , Dict String Payload ) -> Entry
getEntry date ( name, series ) =
    let defaultEntry =
            { raw = Nothing
            , value = MFloat Nothing
            , edition = NoEdition
            , editable = False
            , override = False
            , indexRow = date
            , indexCol = name
            , fromBatch = False
            }
        mPayload = Dict.get date series
    in
        case mPayload of
            Nothing -> defaultEntry
            Just payload
                -> case payload of
                    Scalar scal ->
                        { defaultEntry
                            | value = scal
                            , raw = Just ( toRaw scal )
                        }
                    Complex entry ->
                        { defaultEntry
                                | value = entry.value
                                , edition = entry.edition
                                , raw = Just ( toRaw entry.value )
                                , override = entry.override
                                , editable = True
                            }


getStuff: String ->  ( String , Dict String Payload ) -> Stuff
getStuff date ( name, series ) =
    Cell ( getEntry date ( name, series ))


builRowBasic: List Component -> String -> List Stuff
builRowBasic components date =
    [ DateRow date ]
    ++ ( List.map
             ( getStuff date )
             <| List.map
                (\ c -> ( c.name , c.data ) )
                components
       )


mergeData: List Component -> List ( List Stuff )
mergeData components =
    let dates =
            List.sort
                <| Set.toList
                    <| List.foldl
                            Set.union
                            Set.empty
                            <| List.map
                                (\ c ->  Set.fromList
                                            <| onlyActiveKeys
                                                <| c.data
                                )
                                components
        columns =
            List.map
                ( \ c -> ( c.name, c.cType ) )
                components
    in
    [[ Header ( "Dates", Primary ) ] ++ List.map ( \ s -> Header s ) columns ]
    ++ ( List.map
             ( builRowBasic components )
             dates
       )


tab : String
tab = "\t"


splitByTab : String -> List String
splitByTab s =
    String.split tab s


separatorReturn : String -> Maybe String
separatorReturn raw =
    if String.contains "\r\n" raw then Just "\r\n" -- windows
    else if String.contains "\n" raw then Just "\n" -- unix
    else Nothing


parsePasted : String -> Bool -> List ( List String )
parsePasted raw isString =
    let
        removedSpace =
            if isString
            then
                raw
            else
                String.replace " " "" raw
        sepR =
            separatorReturn raw
    in
    case sepR of
        Nothing ->
            [ splitByTab removedSpace ]
        Just s ->
            List.map
                splitByTab
                <| String.split s removedSpace