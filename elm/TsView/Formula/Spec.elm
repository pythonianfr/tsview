module TsView.Formula.Spec exposing
    ( EditionNode
    , Formula
    , Input
    , JsonSpec
    , ListAction(..)
    , Model
    , Msg(..)
    , Spec
    , SpecType(..)
    , Value(..)
    , buildEditionNode
    , buildEditionTree
    , buildForest
    , buildSpecTree
    , emptyInput
    , fromString
    , getFirstChild
    , getSpecType
    , getUnionCurrentType
    , listOperators
    , parseJsonSpec
    , readInput
    , toString
    )

import Catalog
import Either exposing (Either(..))
import Html exposing (Html)
import Http
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser)
import SeriesSelector


type SpecType
    = Int
    | Float
    | String
    | Date
    | SearchString
    | Series
    | SList SpecType
    | Union (Nonempty SpecType)
    | Arg SpecType
    | OptArg String SpecType
    | OptArgs (List SpecType)
    | Operator String (List SpecType) SpecType


type alias Spec =
    Nonempty SpecType


type alias EditionFlags =
    { isOpen : Bool
    , isRemovable : Bool
    }


type Value
    = Empty
    | IntValue Int
    | FloatValue Float
    | StringValue String
    | DateValue String


type alias Input =
    ( String, Either String Value )


emptyInput : Input
emptyInput =
    ( "", Right Empty )


type alias EditionNode =
    { editFlags : EditionFlags
    , specType : SpecType
    , input : Input
    }


type alias Formula =
    { current : String
    , rendered : String
    , code : Either String (List (Html Msg))
    , saved : Either String (List (Html Msg))
    , name : String
    , error : Maybe String
    }


type alias Model =
    { urlPrefix : String
    , spec : Spec
    , errors : List String
    , buildEditionTree : SpecType -> Tree EditionNode
    , tree : Tree EditionNode
    , formula : Formula
    , search : SeriesSelector.Model
    , catalog : Catalog.Model
    }


type ListAction
    = ListAdd
    | ListRemove


type Msg
    = ToggleNode (Zipper EditionNode)
    | EditList (Zipper EditionNode) ListAction
    | EditNode (Zipper EditionNode) String
    | Render
    | CodeHighlight (Result String String)
    | Save
    | SaveDone (Result String String)
    | EditedName String
    | ToggleItem String
    | SearchSeries String
    | KindChange String Bool
    | ToggleMenu
    | SourceChange String Bool
    | GotCatalog Catalog.Msg
    | MakeSearch
    | GotFormula (Result Http.Error String)


toString : SpecType -> String
toString x =
    case x of
        Int ->
            "int"

        Float ->
            "float"

        String ->
            "string"

        Date ->
            "date"

        Series ->
            "series"

        _ ->
            "series"


fromString : String -> SpecType
fromString x =
    case x of
        "int" ->
            Int

        "float" ->
            Float

        "string" ->
            String

        "date" ->
            Date

        "series" ->
            Series

        _ ->
            Series


type alias JsonArg =
    ( String, String )


type alias JsonOperator =
    ( String, List JsonArg )


type alias JsonSpec =
    List JsonOperator


parseUnion : Parser SpecType
parseUnion =
    Parser.sequence
        { start = "Union["
        , separator = ","
        , end = "]"
        , spaces = Parser.spaces
        , item = parseSpecType
        , trailing = Parser.Forbidden
        }
        |> Parser.andThen
            (NE.fromList
                >> Maybe.map (Union >> Parser.succeed)
                >> Maybe.withDefault (Parser.problem "Empty Union")
            )


parseSpecType : Parser SpecType
parseSpecType =
    let
        succeed =
            Parser.succeed

        keyword =
            Parser.keyword

        symbol =
            Parser.symbol
    in
    Parser.oneOf
        [ succeed Int
            |. keyword "int"
        , succeed Float
            |. keyword "float"
        , succeed Float
            |. keyword "Number"
        , succeed String
            |. keyword "str"
        , succeed String
            |. keyword "bool"
        , succeed Date
            |. keyword "Timestamp"
        , succeed SearchString
            |. keyword "search_str"
        , succeed Series
            |. keyword "Series"
        , succeed SList
            |. keyword "List"
            |. symbol "["
            |= Parser.lazy (\_ -> parseSpecType)
            |. symbol "]"
        , Parser.lazy (\_ -> parseUnion)
        ]


parseArg : JsonArg -> Either String SpecType
parseArg ( name, val ) =
    let
        parseOptArg =
            Parser.oneOf
                [ Parser.succeed (OptArg name)
                    |. Parser.keyword "Optional"
                    |. Parser.symbol "["
                    |= parseSpecType
                    |. Parser.symbol "]"
                , Parser.succeed Arg
                    |= parseSpecType
                ]
    in
    Parser.run parseOptArg val
        |> Either.fromResult
        |> Either.mapLeft Parser.deadEndsToString


parseOperator : JsonOperator -> Either (List String) SpecType
parseOperator ( name, jsonArgs ) =
    case
        List.filter (Tuple.first >> (/=) "return") jsonArgs
            |> List.map parseArg
            |> Either.partition
    of
        ( [], specTypes ) ->
            let
                ( args, optArgs ) =
                    List.foldr
                        (\a b ->
                            case a of
                                OptArg _ _ ->
                                    Tuple.mapSecond ((::) a) b

                                _ ->
                                    Tuple.mapFirst ((::) a) b
                        )
                        ( [], [] )
                        specTypes
            in
            Right <| Operator name args (OptArgs optArgs)

        ( xs, _ ) ->
            Left <|
                if List.isEmpty xs then
                    [ name ++ " no argument provided" ]

                else
                    List.map (\x -> name ++ " " ++ x) xs


parseJsonSpec : JsonSpec -> ( List String, Spec )
parseJsonSpec jsonSpec =
    let
        errSpec =
            NE.fromElement <| OptArg "Spec parsing error" String
    in
    case List.map parseOperator jsonSpec |> Either.partition of
        ( [], x :: xs ) ->
            ( [], NE.Nonempty x xs )

        ( [], [] ) ->
            ( [ "No specification provided" ], errSpec )

        ( ys, x :: xs ) ->
            ( List.concat ys, NE.Nonempty x xs )

        ( ys, [] ) ->
            ( List.concat ys, errSpec )


listOperators : Spec -> List ( String, SpecType )
listOperators =
    let
        f a b =
            case a of
                Operator name _ _ ->
                    ( name, a ) :: b

                _ ->
                    b
    in
    NE.toList >> List.foldr f []


listSpecNodes : Spec -> SpecType -> List SpecType
listSpecNodes spec specType =
    -- takes a type and returns its constituents
    -- to help build subnodes for the whole
    -- edition tree node
    case specType of
        Operator _ args kargs ->
            case kargs of
                OptArgs [] ->
                    args

                _ ->
                    args ++ [ kargs ]

        OptArgs xs ->
            xs

        Arg x ->
            [ x ]

        OptArg _ x ->
            [ x ]

        Union xs ->
            [ NE.head xs ]

        Series ->
            [ NE.head spec ]

        SList x ->
            listSpecNodes spec x

        x ->
            []


buildSpecTree : Spec -> SpecType -> Tree SpecType
buildSpecTree spec =
    Tree.build (listSpecNodes spec)


{-| Build a `Tree.Forest b` (aka `LList Tree b`) from `Zipper` children
-}
buildForest : (Zipper a -> Tree b) -> Zipper a -> Tree.Forest b
buildForest mkTree =
    Zipper.openAll >> List.map mkTree >> LL.fromList


buildEditionNode : Zipper SpecType -> Tree EditionNode
buildEditionNode zipper =
    let
        specType =
            Zipper.current zipper

        isOpen =
            case specType of
                OptArgs _ ->
                    False

                _ ->
                    True

        isRemovable =
            case Zipper.up zipper |> Maybe.map Zipper.current of
                Just (SList _) ->
                    True

                _ ->
                    False
    in
    Tree.Tree
        (EditionNode
            (EditionFlags isOpen isRemovable)
            specType
            emptyInput
        )
        (buildForest buildEditionNode zipper)


buildEditionTree : Spec -> SpecType -> Tree EditionNode
buildEditionTree spec =
    buildSpecTree spec >> Zipper.fromTree >> buildEditionNode


getFirstChild : Zipper a -> Maybe (Zipper a)
getFirstChild =
    Zipper.open (always True)


getSpecType : Zipper EditionNode -> SpecType
getSpecType =
    Zipper.current >> .specType


getUnionCurrentType : SpecType -> Zipper EditionNode -> SpecType
getUnionCurrentType defaultSpecType =
    let
        getUnionChild zipper =
            case getSpecType zipper of
                Union _ ->
                    getFirstChild zipper

                _ ->
                    Nothing
    in
    getUnionChild
        >> Maybe.map getSpecType
        >> Maybe.withDefault defaultSpecType


readInput : Input -> SpecType -> String -> Input
readInput oldInput specType s =
    let
        numInput valueConstructor numConv strType =
            let
                mess =
                    "Could not convert \"" ++ s ++ "\" as " ++ strType
            in
            Either.fromMaybe mess <|
                Maybe.map valueConstructor <|
                    numConv s
    in
    if s == "" then
        emptyInput

    else
        Tuple.pair s <|
            case specType of
                Int ->
                    numInput IntValue String.toInt "Int"

                Float ->
                    numInput FloatValue String.toFloat "Float"

                String ->
                    Right <| StringValue s

                Date ->
                    Right <| DateValue s

                SearchString ->
                    Right <| StringValue s

                _ ->
                    Tuple.second oldInput
