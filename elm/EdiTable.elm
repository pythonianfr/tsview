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


type CScalarType
    = ScalString
    | ScalFloat


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
                Scalar _ -> default r
                Complex b ->
                  case pa of
                      Complex _ -> default r
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
                                                    , fromBatch = True
                                                }
                                            )
                                            r
                                      Just v ->
                                          Dict.insert
                                            d
                                            (Complex
                                                { b | raw = Just ( String.fromFloat v )
                                                    , edition = Edition ( MFloat v)
                                                    , fromBatch = True

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
                                                    , fromBatch = True
                                                }
                                            )
                                            r
                                      Just v ->
                                          Dict.insert
                                                    d
                                                    (Complex
                                                        { b | raw = Just v
                                                        , edition = Edition ( MString v )
                                                        , fromBatch = True
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
                                                    { baseEntryF
                                                        | indexRow = d
                                                    }
                                                )
                                                r
                                Just v -> Dict.insert
                                            d
                                            (Complex
                                                { baseEntryF
                                                    | indexRow = d
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
                                                    | indexRow = d
                                                    }
                                                )
                                                r
                                Just v -> Dict.insert
                                            d
                                            ( Complex
                                                { baseEntryS
                                                | indexRow = d
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


getBounds:  Dict ( Int, Int ) a -> ( ( Int, Int ),  ( Int, Int ))
getBounds cartDict =
    let coords = Dict.keys cartDict
        rows = List.map Tuple.first coords
        cols = List.map Tuple.second coords
        minRow = Maybe.withDefault -1 <| List.minimum rows
        maxRow = Maybe.withDefault -1 <| List.maximum rows
        minCol = Maybe.withDefault -1 <| List.minimum cols
        maxCol = Maybe.withDefault -1 <| List.maximum cols
    in
    ( ( minRow, maxRow ), ( minCol, maxCol ))


dictToGrid: Dict ( Int, Int ) a -> List (List a)
dictToGrid coordDict =
    let
        (( minRow, maxRow ), ( minCol, maxCol )) = getBounds coordDict

        buildRow: Int -> List a
        buildRow rowIdx =
            List.range minCol maxCol
            |> List.filterMap (\colIdx -> Dict.get (rowIdx, colIdx) coordDict)

    in
    List.range minRow maxRow
    |> List.map buildRow


type CType
    = Primary
    | Formula
    | Auto


type alias BaseSupervision =
    { value : Maybe Float
    , override : Bool
    }

type alias BaseSupervisionString =
    { value : Maybe String
    , override : Bool
    }


baseToEntry: BaseSupervision -> Payload
baseToEntry base =
    Complex
    { raw = Maybe.map String.fromFloat base.value
    , value =  MFloat base.value
    , edition = NoEdition
    , editable = True
    , override = base.override
    , indexRow = ""
    , indexCol = ""
    , fromBatch = False
    }

baseToEntryString: BaseSupervisionString -> Payload
baseToEntryString base =
    Complex
     { raw = base.value
     , value = MString base.value
     , edition = NoEdition
     , editable = True
     , override = base.override
     , indexRow = ""
     , indexCol = ""
     , fromBatch = False
     }


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
    , editable: Bool
    , scalarType: CScalarType
    }

type alias Component =
    { name: String
    , cType: CType
    , data: Dict String Payload
    , tzaware: Bool
    , status: CompStatus
    , editable: Bool
    , scalarType: CScalarType
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
                    , editable = over.editable
                    , data = Dict.empty
                    , scalarType = over.scalarType
                    }
    in
    case over.data.zoomTs of
        Nothing -> { emptyComp | data = over.data.initialTs }
        Just zoomTs -> { emptyComp | data = zoomTs }


onlyActiveKeys : Dict String a -> List String
onlyActiveKeys series =
    Dict.keys series


getEntry: String ->  Component -> Entry
getEntry date component =
    let defaultEntry =
            { raw = Nothing
            , value = case component.scalarType of
                        ScalFloat -> MFloat Nothing
                        ScalString -> MString Nothing
            , edition = NoEdition
            , editable = component.editable
            , override = False
            , indexRow = date
            , indexCol = component.name
            , fromBatch = False
            }
        mPayload = Dict.get date component.data
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
                            }


getStuff: String ->  Component -> Stuff
getStuff date component =
    Cell ( getEntry date component)


builRowBasic: List Component -> String -> List Stuff
builRowBasic components date =
    [ DateRow date ]
    ++ ( List.map
            ( getStuff date )
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


-- Fill NAs functions and dependencies

emptyEntry : Entry
emptyEntry  =
    { raw = Nothing
    , value = MFloat Nothing
    , edition = NoEdition
    , editable = False
    , override = False
    , indexRow = ""
    , indexCol = ""
    , fromBatch = False
    }


toString: ( ScalarType Float String ) -> String
toString scal =
    case scal of
        MString s -> s
        MFloat f -> String.fromFloat f


getCurrentValue: Entry -> Maybe ( ScalarType Float String )
getCurrentValue entry =
    case entry.edition of
        Edition edition -> Just edition
        Deletion -> Nothing
        NoEdition ->
            case entry.value of
                MFloat mf ->
                    case mf of
                        Nothing -> Nothing
                        Just f -> Just (MFloat f)
                MString ms ->
                    case ms of
                        Nothing -> Nothing
                        Just s -> Just (MString s)
        Error _ -> Nothing


filterEntry: Dict ( Int, Int ) Stuff ->  Dict ( Int, Int ) Entry
filterEntry coordData =
    Dict.map
        (\ _ stuff -> case stuff of
                       Cell entry -> entry
                       DateRow _ -> emptyEntry
                       Header _ -> emptyEntry
        )
        <| Dict.filter
            (\  _ stuff -> case stuff of
                            Cell _ -> True
                            DateRow _ -> False
                            Header _ -> False
            )
            coordData


isVoid: Entry -> Bool
isVoid entry =
   getCurrentValue entry == Nothing


findNbNas: List Bool -> Int -> Int
findNbNas values nb =
    case values of
        [] -> nb
        x :: xs ->
            case x of
                True -> findNbNas xs ( nb + 1 )
                False -> nb


getNbNas: Dict ( Int, Int ) Entry -> ( Int,  Int ) -> Int
getNbNas coordData position =
    let ( iRow, iCol ) = position
        values = List.map
                    isVoid
                    <| Dict.values
                        <| Dict.filter
                            (\ (i, j) _ -> j == iCol && i > iRow )
                            coordData
    in
    findNbNas values 0


applyValue: ( Int, Int ) -> ( ScalarType Float String )  -> Int -> ( Int, Int ) -> Stuff -> Stuff
applyValue ( rowLastValue, colLastValue) lastValue  nbNas (iRow, iCol) stuff =
    case stuff of
        DateRow d -> DateRow d
        Header h -> Header h
        Cell entry ->
            if iRow > rowLastValue && iRow <= rowLastValue + nbNas && iCol == colLastValue
                then Cell { entry
                              | edition = Edition lastValue
                              , raw = Just ( toString lastValue )
                          }
                else Cell entry


getValueFromIndex: Dict ( Int, Int ) Stuff -> ( Int, Int ) -> ScalarType Float String
getValueFromIndex coordData position =
    let
        stuff = Dict.get position coordData
    in
    case stuff of
        Nothing -> MFloat 0
        Just ( DateRow _ )  -> MFloat 0
        Just ( Header _ )  -> MFloat 0
        Just ( Cell entry ) ->
            Maybe.withDefault ( MFloat 0 ) ( getCurrentValue entry )


fillNas: Dict ( Int, Int ) Stuff -> ( ScalarType Float String ) -> ( Int, Int ) ->  Dict ( Int, Int ) Stuff
fillNas coordData lastValue positionLastValue =
    let
        nbNas = getNbNas ( filterEntry coordData ) positionLastValue
    in
        Dict.map ( applyValue positionLastValue lastValue nbNas ) coordData


fillAllNas : Dict ( Int, Int ) Stuff -> List ( Int, Int ) -> Dict ( Int, Int ) Stuff
fillAllNas coordData idxNas =
    case idxNas of
        [] -> coordData
        x :: xs ->
            let
                lastValue = getValueFromIndex coordData x
            in
                ( fillAllNas
                    ( fillNas
                        coordData
                        lastValue
                        x
                    )
                    xs
                )


findLastValidRec: List ( (Int, Int ),  Entry ) -> Bool -> List ( Int, Int ) -> List ( Int, Int )
findLastValidRec entries previousIsValue found =
    case entries of
        [] -> found
        ((iRow, iCol ), val ) :: xs ->
            if previousIsValue
            then
                if getCurrentValue val == Nothing
                then  List.concat [ [ ( iRow - 1
                                      , iCol
                                      )
                                    ]
                                  , ( findLastValidRec xs False found )
                                  ]
                else ( findLastValidRec xs True found )
            else
                if getCurrentValue val == Nothing
                then ( findLastValidRec xs False found )
                else ( findLastValidRec xs True found )


findLastValidByCol: Dict (Int, Int) Entry -> Int -> List (Int, Int)
findLastValidByCol coordData iCol =
    let column =
            Dict.filter (\ ( _, j ) v -> j == iCol ) coordData
        editable =
            case List.head ( Dict.values column ) of
                Nothing -> False
                Just e -> e.editable
    in
    if not editable
    then []
    else
        findLastValidRec
        ( Dict.toList column )
        False
        []