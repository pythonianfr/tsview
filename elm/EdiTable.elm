module EdiTable exposing
    (..)

import Dict exposing (Dict)


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