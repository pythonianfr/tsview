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