module Editor.UI.Type exposing (..)

import List.Extra
import Maybe.Extra as Maybe
import Either exposing (Either(..))
import Basics.Extra exposing (uncurry)

import Reader
import List.Nonempty as NE
import RoseTree.Tree as Tree
import Optics.Core as O exposing (o)

import AssocList as Assoc
import OpticsExtra as OE
import ReaderExtra as RE exposing (ask)

import Editor.Type as T
import Editor.SpecRender as SR
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)


type alias Union =
    { primitiveTypes : NE.Nonempty T.PrimitiveType
    , primitiveType : T.PrimitiveType
    }

type alias Selector =
    { returnType : T.ReturnType
    , operators : T.Operators
    }

type alias Input =
    { literalType : T.LiteralType
    , value : Maybe T.LiteralExpr
    , userInput : Maybe String
    , errMess : Maybe String
    }

type Primitive
    = PInput Input
    | POperator T.Operator

type Node
    = Primitive Primitive
    | VarArgs T.Packed
    | Packed T.Packed T.Operator

type alias Entry a =
    { union : Maybe Union
    , selector : Maybe Selector
    , entryType : a
    }

type alias Arg =
    { key : T.Key
    , entry : Entry Node
    }

type alias OptArg =
    { key : T.Key
    , defaultValue : Maybe T.LiteralExpr
    , entry : Entry Node
    }

type RowType
    = RArg Arg
    | ROptArgs
    | ROptArg OptArg
    | RVarItem (Entry Primitive)
    | RVarEnd

type alias EditionRow =
    { rowType : RowType
    , isExpand : Maybe Bool
    }

type alias EditionTree = Tree.Tree EditionRow

type alias EditionForest = List EditionTree

type alias EntryForest a = (Entry a, EditionForest)

type alias Root = EntryForest T.Operator

type alias Editor =
    { spec : T.Spec
    , returnTypeStr : T.ReturnTypeStr
    , currentFormula : T.CurrentFormula
    , root : Root
    }

-- small tree utils
type alias Forest a = List (Tree.Tree a)

newTree : (a, Forest a) -> Tree.Tree a
newTree (x, forest) = Tree.branch x forest


-- initialize Tree from SpecType

type alias Reader a = Reader.Reader T.Spec a

initializePrimitiveExpr : T.PrimitiveType -> Reader T.PrimitiveExpr
initializePrimitiveExpr primitiveType = case primitiveType of
    T.Literal t -> Reader.reader <|
        T.LiteralExpr t Nothing

    T.OperatorOutput (T.OperatorOutputType s) ->
        let voidOp = T.voidOperator -- XXX
        in { voidOp | return = T.returnTypeFromString s }
            |> (\op -> T.TypedOperator op Assoc.empty Assoc.empty)
            |> T.OperatorExpr
            |> Reader.reader

initializeTypedExpr : T.ArgType -> Reader T.ArgExpr
initializeTypedExpr argType = case argType of
    T.PrimitiveType t ->
        Reader.map T.PrimitiveExpr <| initializePrimitiveExpr t

    T.UnionType xs -> Reader.map
        (\e -> T.UnionExpr xs (NE.head xs, e))
        (initializePrimitiveExpr <| NE.head xs)

    T.PackedType ((T.Packed t) as p) -> Reader.map
        (\e -> T.VarArgsExpr p [ e ])
        (initializePrimitiveExpr t)

initializeOperator : T.Operator ->  Reader T.TypedOperator
initializeOperator ({args, optArgs} as op) = ask <| \env ->
    T.TypedOperator
        op
        (Assoc.map
            (\_ t -> initializeTypedExpr t |> RE.run env)
            args)
        (Assoc.map
            (\_ (t, _) -> initializeTypedExpr t |> RE.run env)
            optArgs)


-- probeSelector

probeSelector : T.ReturnType -> Entry a -> Reader (Entry a)
probeSelector returnType entry = ask <| \spec ->
    SR.findOperators spec returnType
        |> Either.toMaybe
        |> Maybe.map (Selector returnType)
        |> (\selector -> O.assign selector_ selector entry)

selectorFromPrimitive : Entry Primitive -> Reader (Entry Primitive)
selectorFromPrimitive entry =
    let
        returnTypeFromPrimitive : T.ReturnType
        returnTypeFromPrimitive = case entry.entryType of
            PInput {literalType} ->
                T.ReturnPrimitiveType <| T.Literal literalType

            POperator {return} ->
                return

    in probeSelector returnTypeFromPrimitive entry

selectorFromPacked : T.Packed ->  Entry Node -> Reader (Entry Node)
selectorFromPacked (T.Packed t) entry =
    probeSelector (T.ReturnList t) entry


-- helpers for building EditionTree

type alias EditionArg =
    { isOpt : Bool
    , key : String
    , argType : T.ArgType
    , defaultValue : Maybe T.LiteralExpr
    }

type alias ReaderEntry a = Reader (Entry a, EditionForest)

run : T.Spec -> Reader a -> a
run spec ra = Reader.run ra spec

andThen : (a -> Reader a_) -> Reader (a, b) -> Reader (a_, b)
andThen f rab = rab |> Reader.andThen (\(a, b) ->
    Reader.map2 Tuple.pair (f a) (Reader.reader b))

newInput : T.LiteralType -> Input
newInput t =
    { literalType = t
    , value = Nothing
    , userInput = Nothing
    , errMess = Nothing
    }

newEntry : a -> Entry a
newEntry t =
    { union = Nothing
    , selector = Nothing
    , entryType = t
    }

defaultIsExpand : RowType -> Maybe Bool
defaultIsExpand x =
    if OE.has_ (o rowEntryType_ (o primitive_ pOperator_)) x then
        Just True
    else if OE.has_ (o rowEntryType_ varArgs_) x then
        Just True
    else if OE.has_ (rOptArgs_) x then
        Just False
    else
        Nothing

newEditionRow : RowType -> EditionRow
newEditionRow x = EditionRow x <| defaultIsExpand x


-- fromTypedOperator

fromPrimitiveExpr_  : T.PrimitiveExpr -> ReaderEntry Primitive
fromPrimitiveExpr_ primitiveExpr = case primitiveExpr of
    T.LiteralExpr t v -> Reader.reader
        ( newInput t |> O.assign value_ v |> PInput |> newEntry
        , []
        )

    T.OperatorExpr x ->
        fromTypedOperator_ x
            |> Reader.map (O.over (o OE.first_ entryType_) POperator)

fromPrimitiveExpr  : T.PrimitiveExpr -> ReaderEntry Primitive
fromPrimitiveExpr x =
    fromPrimitiveExpr_ x |> andThen selectorFromPrimitive

fromArgExpr : T.ArgExpr -> ReaderEntry Node
fromArgExpr argExpr = case argExpr of
    T.PrimitiveExpr p ->
        fromPrimitiveExpr p
            |> Reader.map (O.over (o OE.first_ entryType_) Primitive)

    T.UnionExpr xs (t, p) ->
        let union = Union xs t
        in fromPrimitiveExpr p
            |> Reader.map (O.assign (o OE.first_ union_) (Just union))
            |> Reader.map (O.over (o OE.first_ entryType_) Primitive)

    T.VarArgsExpr packed primitiveExprs ->
        let
            toTree : T.PrimitiveExpr -> Reader EditionTree
            toTree e = fromPrimitiveExpr e
                |> Reader.map (O.over OE.first_ (RVarItem >> newEditionRow))
                |> Reader.map newTree

            rVarEnd : EditionTree
            rVarEnd = RVarEnd |> newEditionRow |> Tree.leaf

        in RE.traverse toTree primitiveExprs
            |> Reader.map (\forest ->
                ( newEntry <| VarArgs packed
                , List.append forest [rVarEnd]
                )
            )
            |> andThen (selectorFromPacked packed)

    T.PackedExpr packed typedOperator ->
        fromTypedOperator_ typedOperator
            |> Reader.map
                (O.over (o OE.first_ entryType_) (Packed packed))
            |> andThen (selectorFromPacked packed)

buildForest : T.ArgExprs -> List EditionArg -> Reader EditionForest
buildForest argExprs editionArgs = ask <| \spec ->
    List.map
    (\{isOpt, key, argType, defaultValue} ->
        let
            fromEditionArg : Entry Node -> EditionRow
            fromEditionArg entry = newEditionRow <|
                if isOpt then
                    (OptArg key defaultValue entry |> ROptArg)
                else
                    (Arg key entry |> RArg)

            rowVarArgs_ : O.SimpleTraversal RowType T.Packed
            rowVarArgs_ = o rowEntryType_ varArgs_

        in Assoc.get key argExprs
            |> Maybe.withDefaultLazy
                (\() -> initializeTypedExpr argType |> RE.run spec)
            |> fromArgExpr
            |> run spec
            |> O.over OE.first_ fromEditionArg
            |> newTree
    )
    editionArgs

fromTypedOperator_ : T.TypedOperator -> ReaderEntry T.Operator
fromTypedOperator_ {operator, typedArgs, typedOptArgs} =
    ask <| \spec ->
        let
            args = run spec <| buildForest typedArgs <| List.map
                (\(key, argType) ->
                    { isOpt = False
                    , key = key
                    , argType = argType
                    , defaultValue = Nothing
                    }
                )
                (Assoc.toList operator.args)

            optArgs = run spec <| buildForest typedOptArgs <| List.map
                (\(key, (argType, defaultValue)) ->
                    { isOpt = True
                    , key = key
                    , argType = argType
                    , defaultValue = defaultValue
                    }
                )
                (Assoc.toList operator.optArgs)

            optNode = if Assoc.isEmpty operator.optArgs
                then Nothing
                else Just <| Tree.branch (newEditionRow ROptArgs) optArgs
        in
        ( newEntry <| operator
        , List.append args <| Maybe.toList optNode
        )

fromTypedOperator : T.TypedOperator -> ReaderEntry T.Operator
fromTypedOperator typedOperator =
    fromTypedOperator_ typedOperator
        |> andThen (probeSelector typedOperator.operator.return)


-- toTypedOperator

getNodes :
   O.SimpleTraversal RowType a -> EditionForest -> List (a, EditionForest)
getNodes optics = List.concatMap (\tree ->
    O.getSome (o OE.node_ (o rowType_ optics)) tree
        |> Maybe.map (\a -> (a, O.get OE.forest_ tree)) |> Maybe.toList)

toPrimitiveExpr : Primitive -> EditionForest -> T.PrimitiveExpr
toPrimitiveExpr primitive forest = case primitive of
    PInput {literalType, value} ->
        T.LiteralExpr literalType value

    POperator op ->
        T.OperatorExpr <| toTypedOperator op forest

toArgExpr : Entry Node -> EditionForest -> T.ArgExpr
toArgExpr ({union, entryType} as entry) forest = case entryType of
    Primitive t ->
        let expr = toPrimitiveExpr t forest
        in case union of
            Nothing ->
                T.PrimitiveExpr expr

            Just {primitiveTypes, primitiveType} ->
                T.UnionExpr primitiveTypes (primitiveType, expr)

    VarArgs p ->
        T.VarArgsExpr p <| List.map
            (uncurry toPrimitiveExpr)
            (getNodes (o rVarItem_ entryType_) forest)

    Packed p op ->
        T.PackedExpr p <| toTypedOperator op forest

hasValue : Entry Node -> Bool
hasValue {entryType} = case entryType of
    Primitive (PInput {value}) -> Maybe.isJust value

    Primitive (POperator {name}) -> name /= ""

    _ -> True

renderArgs :
    { required : Bool } ->
    O.SimpleTraversal RowType {s | key : T.Key, entry : Entry Node} ->
    EditionForest ->
    T.ArgExprs
renderArgs {required} optics forest =
    Assoc.fromList <| Maybe.values <| List.map
    (\({key, entry}, subforest) ->
        let argExpr = (key, toArgExpr entry subforest)
        in
        if required then
            Just argExpr
        else if (hasValue entry) then
            Just argExpr
        else
            Nothing)
    (getNodes optics forest)

toTypedOperator : T.Operator -> EditionForest -> T.TypedOperator
toTypedOperator operator forest =
    let
        isOpt : EditionTree -> Bool
        isOpt = OE.has_ (o OE.node_ (o rowType_ rOptArgs_))

        (argsForest, optArgsForest) = List.Extra.unconsLast forest
            |> Maybe.andThen (\((lastTree, _) as x) ->
                if isOpt lastTree then Just x else Nothing)
            |> Maybe.unwrap
                (forest, [])
                (\(optTree, args) ->
                    Tuple.pair args <| O.get OE.forest_ optTree)

    in T.TypedOperator
        operator
        (renderArgs {required = True} rArg_ argsForest)
        (renderArgs {required = False} rOptArg_ optArgsForest)


-- Editor

parse :
    T.Spec -> T.ReturnTypeStr -> T.FormulaCode -> T.CurrentFormula
parse spec returnTypeStr formulaCode =
    parseFormula spec returnTypeStr formulaCode
        |> Either.mapBoth (Tuple.pair formulaCode) (Tuple.pair formulaCode)

initEditor : T.Spec -> T.ReturnTypeStr -> Editor
initEditor spec returnTypeStr =
    let typedOp = T.makeEmptyTypedOperator returnTypeStr
    in buildEditor spec returnTypeStr (renderFormula typedOp)

buildEditor : T.Spec -> T.ReturnTypeStr -> T.FormulaCode -> Editor
buildEditor spec returnTypeStr formulaCode =
    let
        currentFormula = parse spec returnTypeStr formulaCode

        typedOp = T.makeEmptyTypedOperator returnTypeStr

        root = Either.toMaybe currentFormula
            |> Maybe.unwrap typedOp Tuple.second
            |> fromTypedOperator
            |> run spec

    in Editor spec returnTypeStr currentFormula root

renderTypedOperator : Editor -> T.TypedOperator
renderTypedOperator {root} =
    O.over OE.first_ .entryType root |> uncurry toTypedOperator

-- Test TypedOperator rendering, useless for production
testTypedOperator : Editor -> Editor
testTypedOperator ({spec} as editor) =
    let
        newRoot = renderTypedOperator editor
            |> fromTypedOperator
            |> run spec
    in
    { editor | root = newRoot }

updateFormula : Editor -> Editor
updateFormula ({spec, returnTypeStr, root} as editor) =
    let
        currentFormula = O.over OE.first_ .entryType root
            |> uncurry toTypedOperator
            |> renderFormula
            |> parse spec returnTypeStr
    in
    { editor | currentFormula = currentFormula }

-- Test Formula rendering and parsing, useless for production
parseEditor : Editor -> Editor
parseEditor ({spec, root} as editor) =
    let
        newEditor = updateFormula editor

        -- loose current root state in case of valid formula
        newRoot = Either.toMaybe newEditor.currentFormula
            |> Maybe.unwrap
                root
                (Tuple.second >> fromTypedOperator >> run spec)
    in
    { newEditor | root = newRoot }


-- lens helpers for navigating nested strucutres

--Input
literalType_ : O.SimpleLens ls Input T.LiteralType
literalType_ = O.lens .literalType <| \s a -> { s | literalType = a }

value_ : O.SimpleLens ls Input (Maybe T.LiteralExpr)
value_ = O.lens .value <| \s a -> { s | value = a }

userInput_ : O.SimpleLens ls Input (Maybe String)
userInput_ = O.lens .userInput <| \s a -> { s | userInput = a }

errMess_ : O.SimpleLens ls Input (Maybe String)
errMess_ = O.lens .errMess <| \s a -> { s | errMess = a }

-- Primitive
pInput_ : O.SimplePrism ls Primitive Input
pInput_ = O.prism PInput <| \s -> case s of
    PInput x -> Right x

    _ -> Left s

pOperator_ : O.SimplePrism ls Primitive T.Operator
pOperator_ = O.prism POperator <| \s -> case s of
    POperator x -> Right x

    _ -> Left s

-- Node
primitive_ : O.SimplePrism ls Node Primitive
primitive_ = O.prism Primitive <| \s -> case s of
    Primitive x -> Right x

    _ -> Left s
varArgs_ : O.SimplePrism ls Node T.Packed
varArgs_ = O.prism VarArgs <| \s -> case s of
    VarArgs x -> Right x

    _ -> Left s

packed_ : O.SimplePrism ls Node (T.Packed, T.Operator)
packed_ = O.prism (uncurry Packed) <| \s -> case s of
    Packed x y -> Right (x, y)

    _ -> Left s

-- Union
primitiveType_ : O.SimpleLens ls Union T.PrimitiveType
primitiveType_ = O.lens .primitiveType <| \s a -> { s | primitiveType = a }

-- Entry a
union_ : O.SimpleLens ls (Entry a) (Maybe Union)
union_ = O.lens .union <| \s a -> { s | union = a }

justUnion_ : O.SimpleTraversal (Entry a) Union
justUnion_ = o union_ OE.just_

selector_ : O.SimpleLens ls (Entry a) (Maybe Selector)
selector_ = O.lens .selector <| \s a -> { s | selector = a }

entryType_ : O.Lens ls (Entry a) (Entry b) a b
entryType_ = O.lens .entryType <| \{union, selector} b ->
    { union = union
    , selector = selector
    , entryType = b}

-- Arg or OptArg
key_ : O.SimpleLens ls ({ s | key : T.Key }) T.Key
key_ = O.lens .key <| \s a -> { s | key = a }

entry_ : O.SimpleLens ls ({ s | entry : Entry a }) (Entry a)
entry_ = O.lens .entry <| \s a -> { s | entry = a }

-- RowType
rArg_ : O.SimplePrism pr RowType Arg
rArg_ = O.prism RArg <| \s -> case s of
    RArg a -> Right a

    _ -> Left s

rOptArgs_ : O.SimplePrism pr RowType ()
rOptArgs_ = O.prism (always ROptArgs) <| \s -> case s of
    ROptArgs -> Right ()

    _ -> Left s

rOptArg_ : O.SimplePrism pr RowType OptArg
rOptArg_ = O.prism ROptArg <| \s -> case s of
    ROptArg a -> Right a

    _ -> Left s

rVarItem_ : O.SimplePrism pr RowType (Entry Primitive)
rVarItem_ = O.prism RVarItem <| \s -> case s of
    RVarItem a -> Right a

    _ -> Left s

rVarEnd_ : O.SimplePrism pr RowType ()
rVarEnd_ = O.prism (always RVarEnd) <| \s -> case s of
    RVarEnd -> Right ()

    _ -> Left s

-- invalid prism law but useful
rowEntry_ : O.SimplePrism pr RowType (Entry Node)
rowEntry_ = O.prism (Arg "INVALID" >> RArg) <| \s -> case s of
    RArg {entry} -> Right entry

    ROptArg {entry} -> Right entry

    RVarItem entry -> Right <| O.over entryType_ Primitive entry

    _ -> Left s

rowEntryType_ : O.SimpleTraversal RowType Node
rowEntryType_ = o rowEntry_ entryType_

-- EditionRow
rowType_ : O.SimpleLens ls EditionRow RowType
rowType_ = O.lens .rowType (\s a -> { s | rowType = a })

isExpand_ : O.SimpleLens ls EditionRow (Maybe Bool)
isExpand_ = O.lens .isExpand (\s a -> { s | isExpand = a })

-- editor
root_ : O.SimpleLens ls Editor Root
root_ = O.lens .root (\s a -> { s | root = a })

castEntryType :
    O.SimpleTraversal a b -> Entry a -> Maybe (Entry b)
castEntryType optics entry = O.getSome
    (o entryType_ optics)
    entry
    |> Maybe.map (\x -> O.assign entryType_ x entry)

treeRoot_ : O.SimplePrism pr EditionTree Root
treeRoot_ = O.prism
    (\root -> root
        |> O.over (o OE.first_ entryType_) (POperator >> Primitive)
        |> O.over OE.first_ (Arg "EDITOR" >> RArg >> newEditionRow)
        |> O.assign (o OE.first_ isExpand_) Nothing
        |> newTree)
    (\tree -> O.getSome
        (o OE.node_ (o rowType_ rowEntry_))
        tree
        |> Maybe.andThen (castEntryType (o primitive_ pOperator_))
        |> Maybe.map (\op -> (op, O.get OE.forest_ tree))
        |> Either.fromMaybe tree)
