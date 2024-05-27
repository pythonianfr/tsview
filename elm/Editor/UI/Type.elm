module Editor.UI.Type exposing (..)

import List.Extra
import Maybe.Extra as Maybe
import Either exposing (Either(..))

import Tree
import AssocList as Assoc
import List.NonEmpty as NE
import Optics.Core as O exposing (o)

import Editor.Type as T
import Editor.SpecRender as SpecRender
import Editor.Render exposing (renderFormula)
import Editor.Parser exposing (parseFormula)


type ArgType
    = Arg
    | OptArg T.EditableValue

type alias Union =
    { specTypes : NE.NonEmpty T.SpecType
    , specType : T.SpecType
    }

type alias Selector =
    { specType : T.SpecType
    , operators : NE.NonEmpty T.Operator
    }

type alias Input =
    { literalType : T.LiteralType
    , value : Maybe T.EditableValue
    , userInput : Maybe String
    , errMess : Maybe String
    }

type EntryType
    = InputEntry Input
    | OperatorEntry T.Operator

type alias RawEntry a =
    { argType : ArgType
    , argKey : T.Key
    , unions : Maybe (NE.NonEmpty Union)
    , selector : Maybe Selector
    , entryType : a
    }

type alias Entry = RawEntry EntryType

type alias VarArgEntry = RawEntry T.SpecType

type VarArgsRowType
    = VarArg VarArgEntry
    | VarItem Entry
    | VarEnd

-- Editor type of row
type RowType
    = TopRow T.Operator
    | EntryRow Entry
    | OptionsRow
    | VarArgsRow VarArgsRowType

type alias EditionRow =
    { rowType : RowType
    , isExpand : Maybe Bool
    }

type alias EditionTree = Tree.Tree EditionRow

type alias SpecTypeOps =
    Assoc.Dict String (NE.NonEmpty T.Operator)

type alias GSpec =
    { spec : T.Spec
    , specTypeOps : SpecTypeOps
    }

-- buildGSpec : Operators listed by SpecType
unstackUnion : T.SpecType -> NE.NonEmpty T.SpecType
unstackUnion specType = case specType of
    T.Union xs -> NE.concatMap unstackUnion xs

    _ -> NE.singleton specType

buildGSpec : T.Spec -> GSpec
buildGSpec spec =
    let
        pairingOperator : T.Operator -> List (T.SpecType, T.Operator)
        pairingOperator op = List.map
            (\specType -> (specType, op))
            (unstackUnion op.return |> NE.toList)

        addOperator : (T.SpecType, T.Operator) -> SpecTypeOps -> SpecTypeOps
        addOperator (specType, op) =
            Assoc.update
                (SpecRender.renderSpecType specType)
                (Maybe.unwrap (NE.singleton op) (NE.cons op) >> Just)

    in Assoc.values spec
        |> List.concatMap pairingOperator
        |> List.foldr addOperator Assoc.empty
        -- XXX Packed -> VarArgs ?
        |> GSpec spec


-- initialize Tree from TypedExpr
initializeTypedExpr : T.SpecType -> T.TypedExpr
initializeTypedExpr t = case t of
    T.Union xs -> T.TUnion xs (NE.head xs, initializeTypedExpr <| NE.head xs)

    T.VarArgs x -> T.TVarArgs x [ initializeTypedExpr x ]

    T.Packed x -> initializeTypedExpr x -- unclear

    T.Series -> T.voidTOperator

    T.Query -> T.voidTOperator

    T.Timestamp -> T.voidTOperator

    T.Editable x -> T.TLiteral x T.Nil


initializeOperator : T.Operator -> T.TypedExpr
initializeOperator ({args, optArgs} as op) =
    T.TOperator
        op
        (Assoc.map (\_ t -> initializeTypedExpr t) args)
        (Assoc.map (\_ (t, _) -> initializeTypedExpr t) optArgs)


-- lens helpers for navigating nested strucutres

type alias Forest a = List (Tree.Tree a)

-- Tree
node_ : O.SimpleLens ls (Tree.Tree a) a
node_ = O.lens Tree.label (\s a -> Tree.replaceLabel a s)

forest_ : O.SimpleLens ls (Tree.Tree a) (Forest a)
forest_ = O.lens Tree.children (\s a -> Tree.replaceChildren a s)

--Input
literalType_ : O.SimpleLens ls Input T.LiteralType
literalType_ = O.lens .literalType (\s a -> { s | literalType = a })

value_ : O.SimpleLens ls Input (Maybe T.EditableValue)
value_ = O.lens .value (\s a -> { s | value = a })

userInput_ : O.SimpleLens ls Input (Maybe String)
userInput_ = O.lens .userInput (\s a -> { s | userInput = a })

errMess_ : O.SimpleLens ls Input (Maybe String)
errMess_ = O.lens .errMess (\s a -> { s | errMess = a })

-- RawEntry a
argType_ : O.SimpleLens ls (RawEntry a) ArgType
argType_ = O.lens .argType (\s a -> { s | argType = a })

argKey_ : O.SimpleLens ls (RawEntry a) T.Key
argKey_ = O.lens .argKey (\s a -> { s | argKey = a })

unions_ : O.SimpleLens ls (RawEntry a) (Maybe (NE.NonEmpty Union))
unions_ = O.lens .unions (\s a -> { s | unions = a })

selector_ : O.SimpleLens ls (RawEntry a) (Maybe Selector)
selector_ = O.lens .selector (\s a -> { s | selector = a })

entryType_ : O.SimpleLens ls (RawEntry a) a
entryType_ = O.lens .entryType (\s a -> { s | entryType = a })

-- VarArgsRowType
listArg_ : O.SimplePrism pr VarArgsRowType VarArgEntry
listArg_ = O.prism VarArg (\s -> case s of
    VarArg a -> Right a
    _ -> Left s)

listItem_ : O.SimplePrism pr VarArgsRowType Entry
listItem_ = O.prism VarItem (\s -> case s of
    VarItem a -> Right a
    _ -> Left s)

listEnd_ : O.SimplePrism pr VarArgsRowType ()
listEnd_ = O.prism (always VarEnd) (\s -> case s of
    VarEnd -> Right ()
    _ -> Left s)

-- RowType
topRow_ : O.SimplePrism pr RowType T.Operator
topRow_ = O.prism TopRow (\s -> case s of
    TopRow a -> Right a
    _ -> Left s)

entryRow_ : O.SimplePrism pr RowType Entry
entryRow_ = O.prism EntryRow (\s -> case s of
    EntryRow a -> Right a
    _ -> Left s)

optionsRow_ : O.SimplePrism pr RowType ()
optionsRow_ = O.prism (always OptionsRow) (\s -> case s of
    OptionsRow -> Right ()
    _ -> Left s)

varArgsRow_ : O.SimplePrism pr RowType VarArgsRowType
varArgsRow_ = O.prism VarArgsRow (\s -> case s of
    VarArgsRow a -> Right a
    _ -> Left s)

-- EditionRow
rowType_ : O.SimpleLens ls EditionRow RowType
rowType_ = O.lens .rowType (\s a -> { s | rowType = a })

isExpand_ : O.SimpleLens ls EditionRow (Maybe Bool)
isExpand_ = O.lens .isExpand (\s a -> { s | isExpand = a })

-- EditionTree helpers
nodeEntry_ : O.SimpleTraversal EditionTree Entry
nodeEntry_ = o node_ (o rowType_ entryRow_)

nodeVarArgEntry_ : O.SimpleTraversal EditionTree VarArgEntry
nodeVarArgEntry_ = o node_ (o rowType_ (o varArgsRow_ listArg_))


-- EditionTree building and rendering : Union helpers
unstackTUnion : T.TypedExpr -> (Maybe (NE.NonEmpty Union), T.TypedExpr)
unstackTUnion typedExpr =
    let
        unstackTUnion_ xs t = case t of
            T.TUnion (specTypes) (specType, t_) ->
                unstackTUnion_ ((Union specTypes specType)::xs) t_

            _ -> (List.reverse xs, t)

    in unstackTUnion_ [] typedExpr |> Tuple.mapFirst NE.fromList

stackTUnion : Maybe (NE.NonEmpty Union) -> T.TypedExpr -> T.TypedExpr
stackTUnion mUnions typedExpr =
    let
        stackUnions : T.TypedExpr -> List Union -> T.TypedExpr
        stackUnions t unions = case unions of
            ({specTypes, specType}) :: xs  ->
               stackUnions (T.TUnion specTypes (specType, t)) xs

            [] -> t

    in Maybe.map (NE.toList >> List.reverse) mUnions
        |> Maybe.unwrap typedExpr (stackUnions typedExpr)


-- buildEditionTree
type alias EditionArg =
    { key : String
    , specType : T.SpecType
    , defaultValue : Maybe T.EditableValue
    }

type alias TypedExprs = T.KAssoc T.TypedExpr

type alias EditionForest = List EditionTree

newInput : T.LiteralType -> Input
newInput t =
    { literalType = t
    , value = Nothing
    , userInput = Nothing
    , errMess = Nothing
    }

newEntry : a -> RawEntry a
newEntry t =
    { argType = Arg
    , argKey = ""
    , unions = Nothing
    , selector = Nothing
    , entryType = t
    }

newEditionTree : RowType -> EditionTree
newEditionTree n = Tree.singleton <| EditionRow n Nothing

probeSelector : GSpec -> T.SpecType -> Maybe Selector
probeSelector { specTypeOps } specType =
    Assoc.get (SpecRender.renderSpecType specType) specTypeOps
        |> Maybe.map (\ops -> Selector specType ops)

buildEditionForest : GSpec -> TypedExprs -> List EditionArg -> EditionForest
buildEditionForest gSpec exprs = List.concatMap (\a ->
    let
        (unions, typedExpr) = Assoc.get a.key exprs
            |> Maybe.withDefaultLazy
                (\() -> initializeTypedExpr a.specType)
            |> unstackTUnion

        specType = Maybe.unwrap a.specType (NE.last >> .specType) unions

        initEntry : RawEntry a -> RawEntry a
        initEntry = O.assign argType_ (Maybe.unwrap Arg OptArg a.defaultValue)
            >> O.assign argKey_ a.key
            >> O.assign unions_ unions
            >> O.assign selector_ (probeSelector gSpec specType)

    in
    buildEditionTree_ gSpec typedExpr
    |> O.over nodeEntry_ initEntry
    |> O.over nodeVarArgEntry_ initEntry
    |> (\tree ->
    if O.get (o node_ rowType_) tree |> O.is (o varArgsRow_ listArg_) then
        [ tree , VarEnd |> VarArgsRow |> newEditionTree ]
    else
        [ tree ])
    )

wrapVarItem : GSpec -> T.SpecType -> T.TypedExpr -> EditionTree
wrapVarItem gSpec specType typedExpr =
    let
        (unions, typedExpr_) = unstackTUnion typedExpr

        specType_ = Maybe.unwrap specType (NE.last >> .specType) unions

        initEntry : RawEntry a -> RawEntry a
        initEntry = O.assign argType_ Arg
            >> O.assign argKey_ "list_item"
            >> O.assign unions_ unions
            >> O.assign selector_ (probeSelector gSpec specType_)

    in buildEditionTree_ gSpec typedExpr_
        |> O.over nodeEntry_ initEntry
        |> O.over (o node_ rowType_) (\r ->
            case r of
                EntryRow e -> VarArgsRow <| VarItem e
                _ -> r)

buildEditionTree_ : GSpec -> T.TypedExpr -> EditionTree
buildEditionTree_ gSpec typedExpr = case typedExpr of
    T.TOperator op typedArgs typedOptArgs ->
        let
            args = buildEditionForest gSpec typedArgs <| List.map
                (\(k, s) -> EditionArg k s Nothing)
                (Assoc.toList op.args)

            optArgs = buildEditionForest gSpec typedOptArgs <| List.map
                (\(k, (s, v)) -> EditionArg k s (Just v))
                (Assoc.toList op.optArgs)

            optNode = if Assoc.isEmpty op.optArgs
                then Nothing
                else newEditionTree OptionsRow
                    |> O.assign (o node_ isExpand_) (Just False)
                    |> O.assign forest_ optArgs
                    |> Just

        in OperatorEntry op |> newEntry |> EntryRow |> newEditionTree
            |> O.assign (o node_ isExpand_) (Just True)
            |> O.assign forest_ (List.append args <| Maybe.toList optNode)

    T.TVarArgs specType typedExprs ->
        newEntry specType |> VarArg |> VarArgsRow |> newEditionTree
            |> O.assign
                forest_
                (List.map (wrapVarItem gSpec specType) typedExprs)

    T.TUnion _ _ as t -> -- Normally handled by Operator
        let (unions, t_) = unstackTUnion t
        in buildEditionTree_ gSpec t_
            |> O.assign (o nodeEntry_ unions_) unions

    T.TLiteral t v -> newInput t |> O.assign value_ (Just v)
        |> InputEntry |> newEntry |> EntryRow |> newEditionTree

buildEditionTree : GSpec -> T.TypedExpr -> EditionTree
buildEditionTree gSpec typedExpr =
    buildEditionTree_ gSpec typedExpr |> O.over (o node_ rowType_) (\r ->
        case r of
            EntryRow {entryType} -> case entryType of
                OperatorEntry op -> TopRow op
                _ -> r
            _ -> r)


-- renderTypedExpr
renderArgs : EditionForest -> TypedExprs
renderArgs forest =
    let
        logNoKey i = "NO-KEY-" ++ (String.fromInt i)

        renderArg : Int -> EditionTree -> (T.Key, T.TypedExpr)
        renderArg i tree = renderKeyTypedExpr tree
            |> Tuple.mapFirst (Maybe.withDefault (logNoKey i))

    in List.indexedMap renderArg forest |>  Assoc.fromList

renderOperator : T.Operator -> EditionTree -> T.TypedExpr
renderOperator op tree =
    let
        forest = O.get forest_ tree

        isOpt : EditionTree -> Bool
        isOpt = O.get (o node_ rowType_) >> O.is optionsRow_

        filterVarEnd : EditionForest -> EditionForest
        filterVarEnd = List.filter
            (O.get (o node_ rowType_) >> O.is (o varArgsRow_ listEnd_) >> not)

        (argsForest, optArgsForest) = List.Extra.unconsLast forest
            |> Maybe.andThen (\((lastTree, _) as x) ->
                if isOpt lastTree then Just x else Nothing)
            |> Maybe.unwrap
                (forest, [])
                (\(optTree, args) -> Tuple.pair args <| O.get forest_ optTree)
            |> Tuple.mapBoth filterVarEnd filterVarEnd

    in T.TOperator op (renderArgs argsForest) (renderArgs optArgsForest)

type alias KeyTypedExpr = (Maybe T.Key, T.TypedExpr)

renderKeyTypedExpr : EditionTree -> KeyTypedExpr
renderKeyTypedExpr tree = case O.get (o node_ rowType_) tree of
    -- Entry point of renderTypedExpr
    TopRow op -> Tuple.pair Nothing <| renderOperator op tree

    EntryRow {argKey, unions, entryType} ->
        Tuple.pair (Just argKey) <| stackTUnion unions <| case entryType of
            InputEntry {literalType, value} ->
                Maybe.unwrap T.voidExpr (T.TLiteral literalType) value

            OperatorEntry op ->
                renderOperator op tree

    VarArgsRow (VarArg {argKey, unions, entryType}) ->
        Tuple.pair (Just argKey) <| stackTUnion unions
            <| T.TVarArgs entryType
            <| List.map
                (renderKeyTypedExpr >> Tuple.second)
                (O.get forest_ tree)

    VarArgsRow (VarItem e) ->
        O.assign (o node_ rowType_) (EntryRow e) tree |> renderKeyTypedExpr

    -- Remaining cases normally handled by renderOperator
    VarArgsRow VarEnd ->
        Tuple.pair Nothing <| renderOperator T.voidOperator tree

    OptionsRow ->
        Tuple.pair Nothing <| renderOperator T.voidOperator tree


-- render EditionTree into a TypedExpr for Formula rendering
renderTypedExpr : EditionTree -> T.TypedExpr
renderTypedExpr tree =
    if O.get (o node_ rowType_) tree |> (O.is topRow_) then
        renderKeyTypedExpr tree |> Tuple.second
    else
        T.TLiteral T.String <| T.StringValue "renderTypedExpr needs a TopRow"


-- render and parse EditionTree for displaying errors or a new tree
parseEditionTree : GSpec -> EditionTree -> (Maybe String, EditionTree)
parseEditionTree gSpec tree =
    renderTypedExpr tree
        |> renderFormula
        |> parseFormula gSpec.spec
        |> Either.unpack
            (\err -> (Just err, tree))
            (\typedExpr ->  (Nothing, buildEditionTree gSpec typedExpr))

