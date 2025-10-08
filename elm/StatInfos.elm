module StatInfos exposing
    ( Msg(..)
    , StatInfos
    , TypeStat
    , StatusFreq
    , emptyStat
    , getStatistics
    , formatNumber
    , updateFirstLast
    , viewStatTable
    )

import Dateinterval exposing (medianValue)
import Dict exposing (Dict)
import Html as H
import Html exposing (Attribute)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as List
import List.Statistics as Stat
import Metadata as M
import Statistics
import Round

import TimeUtil exposing ( localize )

type alias StatInfos =
    { first: TypeStat
    , last: TypeStat
    , start : TypeStat
    , end: TypeStat
    , min: TypeStat
    , max: TypeStat
    , sum: TypeStat
    , count: TypeStat
    , nas: TypeStat
    , mean: TypeStat
    , p25: TypeStat
    , median: TypeStat
    , p75: TypeStat
    , inferFreq : TypeStat
    }


type TypeStat =
    Numeric ( Maybe Float )
    | Count Int
    | Date ( Maybe String )
    | DateNonLocalized ( Maybe String )
    | InferFreq StatusFreq


type StatusFreq =
    Blocked
    | Authorised ( Maybe String )

type Msg =
    AllowInferFreq


emptyStat =
    { first = DateNonLocalized Nothing
    , last = DateNonLocalized Nothing
    , start = Date Nothing
    , end = Date Nothing
    , min = Numeric Nothing
    , max = Numeric Nothing
    , sum = Numeric Nothing
    , count = Count 0
    , nas = Count 0
    , mean = Numeric Nothing
    , p25 = Numeric Nothing
    , median = Numeric Nothing
    , p75 = Numeric Nothing
    , inferFreq = InferFreq ( Authorised Nothing )
    }

maxPoints = 1000

updateFirstLast: StatInfos -> M.Metadata -> StatInfos
updateFirstLast statInfos meta =
    let first = DateNonLocalized <| case Dict.get "left" meta of
                            Just ( M.MString val )
                                -> Just val
                            _ -> Nothing
        last = DateNonLocalized <| case Dict.get "right" meta of
                            Just ( M.MString val )
                                -> Just val
                            _ -> Nothing
    in
        { statInfos | first = first
                    , last = last
        }

getStatistics: StatInfos -> Bool -> Dict String ( Maybe Float )-> StatInfos
getStatistics previous allowInfer series =
    let dates = List.sort ( Dict.keys series )
        values = List.sort <| justValues series
        length = List.length values
        inferFreq = if  not ( length < maxPoints || allowInfer )
                    then Blocked
                    else
                    let resultMedian =  medianValue ( Dict.keys series)
                    in
                        case resultMedian of
                            Nothing -> Authorised Nothing
                            Just (median, quality) ->
                                Authorised <| Just median
    in
        { previous
            |start = Date <| List.head dates
            , end = Date <| List.last dates
            , min = Numeric <| Stat.minimum values
            , max = Numeric <| Stat.maximum values
            , count = Count length
            , nas = Count <| ( List.length dates ) - length
            , sum = if length == 0
                        then Numeric Nothing
                        else Numeric ( Just ( List.sum values ))
            , mean = Numeric <| Stat.mean values
            , p25 = Numeric <| Statistics.quantile 0.25 values
            , median = Numeric <| Stat.median values
            , p75 = Numeric <| Statistics.quantile 0.75 values
            , inferFreq = InferFreq inferFreq
        }


justValues: Dict String ( Maybe Float ) -> List Float
justValues series =
     List.concat
        <| List.map
            (\ (k, v) -> case v of
                            Nothing -> []
                            Just val -> [ val ]
            )
            (Dict.toList series)


viewStatTable: StatInfos -> Int -> String -> Bool -> ( Msg -> msg ) -> H.Html msg
viewStatTable statInfos round tzone tzaware convertMsg =
    let partialRow = ( rowStat round tzone tzaware convertMsg )
    in
    H.table
        [ HA.class "stat-table"]
        [ H.th
            [HA.colspan 3]
            [ H.text "Data Info"]
        , partialRow "First" ( Just "Lower bound of whole series" ) statInfos.first
        , partialRow "Last" ( Just "Upper bound of whole series" ) statInfos.last
        , partialRow "Start" ( Just "Lower bound of selected horizon" ) statInfos.start
        , partialRow "End" ( Just "Upper bound of selected horizon" ) statInfos.end
        , partialRow "Min" Nothing statInfos.min
        , partialRow "Max" Nothing statInfos.max
        , partialRow "Sum" Nothing statInfos.sum
        , partialRow "Count" Nothing statInfos.count
        , partialRow "NaNs" Nothing statInfos.nas
        , partialRow "Mean" Nothing statInfos.mean
        , partialRow "P25" Nothing statInfos.p25
        , partialRow "P50" Nothing statInfos.median
        , partialRow "P75" Nothing statInfos.p75
        , partialRow "Freq" Nothing statInfos.inferFreq
        ]

rowStat : Int -> String -> Bool -> ( Msg -> msg ) -> String -> Maybe String -> TypeStat -> H.Html msg
rowStat round tzone tzaware convertMsg name mTitle statistic  =
    let content = case statistic of
                    Numeric num ->
                        case num of
                            Nothing -> [H.text ""]
                            Just value -> [ H.text
                                                <| formatNumber
                                                    <| Round.round
                                                        round
                                                        value
                                          ]
                    Count nb -> [ H.text
                                    <| formatNumber
                                        ( String.fromInt nb )
                                ]
                    Date date -> displayDate date
                    DateNonLocalized date -> displayDate
                                                <| Maybe.map
                                                    (localize tzone tzaware)
                                                    date
                    InferFreq infer -> case infer of
                                        Blocked -> [ H.button
                                                    [ HA.class "badge badge-primary h4"
                                                    , HA.title "! Might be costly !"
                                                    , HE.onClick ( convertMsg AllowInferFreq )
                                                    ]
                                                    [ H.text "Unlock" ]
                                                    ]
                                        Authorised freq ->
                                            case freq of
                                                Nothing -> []
                                                Just f -> [ H.text f ]
    in
        H.tr
             ( case mTitle of
                    Nothing ->
                        [ ]
                    Just title ->
                        [ HA.title title ]
            )
            [ H.td
                []
                [H.text name]
            , H.td
                []
                []
            , H.td
                []
                content
            ]

formatNumber: String -> String
formatNumber number =
    let negative = String.startsWith "-" number
        absolute = if negative
                    then String.replace "-" "" number
                    else number
        parts = String.split "." absolute
    in
      case parts of
          [] -> ""
          [ x ] ->  ( restoreSign negative )
                        <| String.reverse
                            <| addSpace
                                <| String.reverse x
          x :: xs ->
            String.concat
                [( restoreSign negative )
                    <| String.reverse
                        <| addSpace
                            <| String.reverse x
                , "."
                , String.concat xs
                ]


addSpace: String -> String
addSpace part =
   String.fromList
        <|addSpaceRec
            (String.toList part)
            3
            []


addSpaceRec: List Char -> Int -> List Char -> List Char
addSpaceRec parts counter result =
    if counter == 0 && not (List.isEmpty parts)
        then addSpaceRec
                parts
                3
                ( List.append result [' '] )
        else
            case parts of
                [] -> result
                x :: xs ->
                    addSpaceRec
                        xs ( counter - 1 )
                        ( List.append result [ x ])


restoreSign: Bool -> String -> String
restoreSign negative number =
    if negative
        then String.concat [ "-",  number ]
        else number

displayDate: Maybe String -> List ( H.Html msg )
displayDate date =
    case date of
        Nothing -> []
        Just dat ->
            intercal
                ( H.br [] [] )
                ( List.map
                    (\ part -> H.text part )
                    (String.split "T" dat)
                )
                []


intercal: a -> List a -> List a ->  List a
intercal toAdd parts result =
     case parts of
         [] -> result
         [x] -> result  ++ [x]
         x :: xs -> result ++ [x] ++ [toAdd] ++ intercal toAdd xs result
