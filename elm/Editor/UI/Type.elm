module Editor.UI.Type exposing (..)

import List.Extra
import Maybe.Extra as Maybe
import Either exposing (Either(..))
import Basics.Extra exposing (uncurry)

import Reader
import ReaderExtra as RE exposing (ask, askM)
import AssocList as Assoc
import List.Nonempty as NE
import RoseTree.Tree as Tree

import Optics.Core as O exposing (o)
import OpticsExtra as OE

import Editor.Type as T
import Editor.Parser exposing (parseFormula)
import Editor.Render exposing (renderFormula)


type alias Union =
    { primitiveTypes : NE.Nonempty T.PrimitiveType
    , primitiveType : T.PrimitiveType
    }

type alias Unions = NE.Nonempty Union

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

type alias Operator =
    { specOperator : T.Operator
    , returnType : T.ReturnType
    }

type Primitive
    = PInput Input
    | POperator Operator

type Composite
    = CVarArgs T.PrimitiveType
    | CPacked T.PrimitiveType Operator

type Node
    = Primitive Primitive
    | Composite Composite

type alias Entry a =
    { unions : Maybe Unions
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

type alias Root = EntryForest Operator

type alias Editor =
    { gSpec : T.GSpec
    , returnType : T.ReturnType
    , currentFormula : T.CurrentFormula
    , root : Root
    }

-- small tree utils
type alias Forest a = List (Tree.Tree a)

newTree : (a, Forest a) -> Tree.Tree a
newTree (x, forest) = Tree.branch x forest

has : O.Optic pr ls s t a b -> s -> Bool
has optics s = O.getSome optics s |> Maybe.isJust


-- initialize Tree from SpecType

initializePrimitiveExpr : T.PrimitiveType -> Reader T.PrimitiveExpr
initializePrimitiveExpr primitiveType = case primitiveType of
    T.Literal t -> Reader.reader <|
        T.LiteralExpr t Nothing

    T.OperatorOutput x ->
        T.ReturnOperatorOutput x
            |> T.BaseReturnType
            |> T.TypedOperator T.voidOperator Assoc.empty Assoc.empty
            |> T.OperatorExpr
            |> Reader.reader

    T.Union xs -> Reader.map
        (\e -> T.UnionExpr xs (NE.head xs, e))
        (initializePrimitiveExpr <| NE.head xs)

initializeCompositeExpr : T.CompositeType -> Reader T.CompositeExpr
initializeCompositeExpr compositeType = case compositeType of
    T.VarArgs t -> Reader.map
        (\e -> T.VarArgsExpr t [ e ])
        (initializePrimitiveExpr t)

    T.Packed t -> Reader.reader
        (T.PackedExpr t T.voidTypedOperator)

initializeTypedExpr : T.SpecType -> Reader T.TypedExpr
initializeTypedExpr specType = case specType of
    T.PrimitiveType t ->
        Reader.map T.PrimitiveExpr <| initializePrimitiveExpr t

    T.CompositeType t ->
        Reader.map T.CompositeExpr <| initializeCompositeExpr t

initializeOperator : T.Operator -> T.ReturnType -> Reader T.TypedOperator
initializeOperator ({args, optArgs} as op) returnType = ask <| \env ->
    T.TypedOperator
        op
        (Assoc.map
            (\_ t -> initializeTypedExpr t |> RE.run env)
            args)
        (Assoc.map
            (\_ (t, _) -> initializeTypedExpr t |> RE.run env)
            optArgs)
        returnType


-- EditionTree building and rendering : Union helpers

unstackUnion :
   T.PrimitiveExpr -> (Maybe (NE.Nonempty Union), T.PrimitiveExpr)
unstackUnion primitiveExpr =
    let
        unstackUnion_ xs t = case t of
            T.UnionExpr (primitiveTypes) (primitiveType, t_) ->
                unstackUnion_ ((Union primitiveTypes primitiveType)::xs) t_

            _ -> (List.reverse xs, t)

    in unstackUnion_ [] primitiveExpr |> Tuple.mapFirst NE.fromList

stackUnion : Maybe (NE.Nonempty Union) -> T.PrimitiveExpr -> T.PrimitiveExpr
stackUnion mUnions primitiveExpr =
    let
        stackUnions : T.PrimitiveExpr -> List Union -> T.PrimitiveExpr
        stackUnions t unions = case unions of
            ({primitiveTypes, primitiveType}) :: xs  ->
               stackUnions (T.UnionExpr primitiveTypes (primitiveType, t)) xs

            [] -> t

    in Maybe.map (NE.toList >> List.reverse) mUnions
        |> Maybe.unwrap primitiveExpr (stackUnions primitiveExpr)


-- probeSelector

returnTypeFromPrimitive : Primitive -> T.ReturnType
returnTypeFromPrimitive primitive = case primitive of
    PInput {literalType} -> T.BaseReturnType <| T.ReturnLiteral literalType

    POperator {returnType} -> returnType

returnTypeFromComposite : Composite -> T.ReturnType
returnTypeFromComposite composite =
   T.ReturnPacked <| T.listBaseReturnType <| case composite of
       CVarArgs primitiveType -> primitiveType

       CPacked primitiveType _ -> primitiveType

probeSelector : T.ReturnType -> Entry a -> Reader (Entry a)
probeSelector returnType entry = ask <| \gSpec ->
    T.findOperators gSpec returnType
        |> Either.toMaybe
        |> Maybe.map (Selector returnType)
        |> (\selector -> O.assign selector_ selector entry)

selectorFromPrimitive : Entry Primitive -> Reader (Entry Primitive)
selectorFromPrimitive entry =
    probeSelector (returnTypeFromPrimitive entry.entryType) entry

selectorFromComposite : Entry Composite -> Reader (Entry Composite)
selectorFromComposite entry =
    probeSelector (returnTypeFromComposite entry.entryType) entry


-- helpers for building EditionTree

type alias EditionArg =
    { isOpt : Bool
    , key : String
    , specType : T.SpecType
    , defaultValue : Maybe T.LiteralExpr
    }

type alias TypedExprs = T.KAssoc T.TypedExpr

type alias Reader a = Reader.Reader T.GSpec a

type alias ReaderEntry a = Reader (Entry a, EditionForest)

run : T.GSpec -> Reader a -> a
run gSpec ra = Reader.run ra gSpec

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
    { unions = Nothing
    , selector = Nothing
    , entryType = t
    }

newEditionRow : RowType -> EditionRow
newEditionRow x = EditionRow x <|
    if has (o rowEntryType_ (o primitive_ pOperator_)) x then
        Just True
    else if has (o rowEntryType_ composite_) x then
        Just True
    else if has (rOptArgs_) x then
        Just False
    else
        Nothing


-- fromTypedOperator

fromPrimitiveExpr_  : T.PrimitiveExpr -> ReaderEntry Primitive
fromPrimitiveExpr_ primitiveExpr = case primitiveExpr of
    T.LiteralExpr t v -> Reader.reader
        ( newInput t |> O.assign value_ v |> PInput |> newEntry
        , []
        )

    T.UnionExpr _ _ as unionExpr ->
        let (unions, t) = unstackUnion unionExpr
        in fromPrimitiveExpr t
            |> Reader.map (O.assign (o OE.first_ unions_) unions)

    T.OperatorExpr x ->
        fromTypedOperator_ x
            |> Reader.map (O.over (o OE.first_ entryType_) POperator)

fromPrimitiveExpr  : T.PrimitiveExpr -> ReaderEntry Primitive
fromPrimitiveExpr x =
    fromPrimitiveExpr_ x |> andThen selectorFromPrimitive

fromCompositeExpr_ : T.CompositeExpr -> ReaderEntry Composite
fromCompositeExpr_ compositeExpr = case compositeExpr of
    T.VarArgsExpr primitiveType primitiveExprs ->
        let
            toTree : T.PrimitiveExpr -> Reader EditionTree
            toTree e = fromPrimitiveExpr e
                |> Reader.map (O.over OE.first_ (RVarItem >> newEditionRow))
                |> Reader.map newTree

        in Reader.map2
            Tuple.pair
            (CVarArgs primitiveType |> newEntry |> Reader.reader)
            (RE.traverse toTree primitiveExprs)

    T.PackedExpr primitiveType typedOperator ->
        fromTypedOperator_ typedOperator
            |> Reader.map
                (O.over (o OE.first_ entryType_) (CPacked primitiveType))

fromCompositeExpr : T.CompositeExpr -> ReaderEntry Composite
fromCompositeExpr x =
    fromCompositeExpr_ x |> andThen selectorFromComposite

fromTypedExpr : T.TypedExpr -> ReaderEntry Node
fromTypedExpr typedExpr = case typedExpr of
    T.PrimitiveExpr e ->
        fromPrimitiveExpr e
            |> Reader.map (O.over (o OE.first_ entryType_) Primitive)

    T.CompositeExpr e ->
        fromCompositeExpr e
            |> Reader.map (O.over (o OE.first_ entryType_) Composite)

buildForest : TypedExprs -> List EditionArg -> Reader EditionForest
buildForest typedExprs editionArgs = ask <| \gSpec ->
    List.map
    (\{isOpt, key, specType, defaultValue} ->
        let
            fromEditionArg : Entry Node -> EditionRow
            fromEditionArg entry = newEditionRow <|
                if isOpt then
                    (OptArg key defaultValue entry |> ROptArg)
                else
                    (Arg key entry |> RArg)

            rowVarArgs_ : O.SimpleTraversal RowType T.PrimitiveType
            rowVarArgs_ = o rowEntryType_ (o composite_ cVarArgs_)

        in Assoc.get key typedExprs
            |> Maybe.withDefaultLazy
                (\() -> initializeTypedExpr specType |> RE.run gSpec)
            |> fromTypedExpr
            |> run gSpec
            |> O.over OE.first_ fromEditionArg
            |> newTree
            |> (\tree ->
            if has (o OE.node_ (o rowType_ rowVarArgs_)) tree then
                let x = RVarEnd |> newEditionRow |> Tree.leaf
                in O.over OE.forest_ (\xs -> List.append xs [ x ]) tree
            else
                tree)
    )
    editionArgs

fromTypedOperator_ : T.TypedOperator -> ReaderEntry Operator
fromTypedOperator_ {operator, typedArgs, typedOptArgs, returnType} =
    ask <| \gSpec ->
        let
            args = run gSpec <| buildForest typedArgs <| List.map
                (\(key, specType) ->
                    { isOpt = False
                    , key = key
                    , specType = specType
                    , defaultValue = Nothing
                    }
                )
                (Assoc.toList operator.args)

            optArgs = run gSpec <| buildForest typedOptArgs <| List.map
                (\(key, (specType, defaultValue)) ->
                    { isOpt = True
                    , key = key
                    , specType = specType
                    , defaultValue = defaultValue
                    }
                )
                (Assoc.toList operator.optArgs)

            optNode = if Assoc.isEmpty operator.optArgs
                then Nothing
                else Just <| Tree.branch (newEditionRow ROptArgs) optArgs
        in
        ( newEntry <| Operator operator returnType
        , List.append args <| Maybe.toList optNode
        )

fromTypedOperator : T.TypedOperator -> ReaderEntry Operator
fromTypedOperator typedOperator =
    fromTypedOperator_ typedOperator
        |> andThen (probeSelector typedOperator.returnType)


-- toTypedOperator

getNodes :
   O.SimpleTraversal RowType a -> EditionForest -> List (a, EditionForest)
getNodes optics = List.concatMap (\tree ->
    O.getSome (o OE.node_ (o rowType_ optics)) tree
        |> Maybe.map (\a -> (a, O.get OE.forest_ tree)) |> Maybe.toList)

toPrimitiveExpr : Entry Primitive -> EditionForest -> T.PrimitiveExpr
toPrimitiveExpr {unions, entryType} forest = stackUnion unions <|
    case entryType of
        PInput {literalType, value} ->
            T.LiteralExpr literalType value

        POperator op ->
            T.OperatorExpr <| toTypedOperator op forest

toCompositeExpr : Entry Composite -> EditionForest -> T.CompositeExpr
toCompositeExpr {entryType} forest = case entryType of
    CVarArgs primitiveType ->
        T.VarArgsExpr primitiveType <|
            List.map (uncurry toPrimitiveExpr) (getNodes rVarItem_ forest)

    CPacked primitiveType op ->
        T.PackedExpr primitiveType <| toTypedOperator op forest

toTypedExpr : Entry Node -> EditionForest -> T.TypedExpr
toTypedExpr ({entryType} as entry) forest = case entryType of
    Primitive t ->
        T.PrimitiveExpr <|
            toPrimitiveExpr (O.assign entryType_ t entry) forest

    Composite t ->
        T.CompositeExpr <|
            toCompositeExpr (O.assign entryType_ t entry) forest

hasValue : Entry Node -> Bool
hasValue {entryType} = case entryType of
    Primitive (PInput {value}) -> Maybe.isJust value
    
    _ -> True

renderArgs :
    { required : Bool } ->
    O.SimpleTraversal RowType {s | key : T.Key, entry : Entry Node} ->
    EditionForest ->
    TypedExprs
renderArgs {required} optics forest =
    Assoc.fromList <| Maybe.values <| List.map
    (\({key, entry}, subforest) ->
        let typedExpr = (key, toTypedExpr entry subforest)
        in
        if required then
            Just typedExpr
        else if (hasValue entry) then
            Just typedExpr
        else
            Nothing)
    (getNodes optics forest)

toTypedOperator : Operator -> EditionForest -> T.TypedOperator
toTypedOperator {specOperator, returnType} forest =
    let
        isOpt : EditionTree -> Bool
        isOpt = has (o OE.node_ (o rowType_ rOptArgs_))

        (argsForest, optArgsForest) = List.Extra.unconsLast forest
            |> Maybe.andThen (\((lastTree, _) as x) ->
                if isOpt lastTree then Just x else Nothing)
            |> Maybe.unwrap
                (forest, [])
                (\(optTree, args) -> Tuple.pair args <| O.get OE.forest_ optTree)

    in T.TypedOperator
        specOperator
        (renderArgs {required = True} rArg_ argsForest)
        (renderArgs {required = False} rOptArg_ optArgsForest)
        returnType


-- Editor

parse :
    T.GSpec -> T.ReturnType -> T.FormulaCode -> T.CurrentFormula
parse gSpec returnType formulaCode =
    parseFormula gSpec returnType formulaCode
        |> Either.mapBoth (Tuple.pair formulaCode) (Tuple.pair formulaCode)

initEditor : T.GSpec -> T.ReturnType -> Editor
initEditor gSpec returnType =
    buildEditor gSpec returnType (renderFormula T.voidTypedOperator)

buildEditor : T.GSpec -> T.ReturnType -> T.FormulaCode -> Editor
buildEditor gSpec returnType formulaCode =
    let
        currentFormula = parse gSpec returnType formulaCode

        root = Either.toMaybe currentFormula
            |> Maybe.unwrap T.voidTypedOperator Tuple.second
            |> fromTypedOperator
            |> run gSpec

    in Editor gSpec returnType currentFormula root

-- Test TypedOperator rendering, useless for production
renderTypedOperator : Editor -> Editor
renderTypedOperator ({gSpec, root} as editor) =
    let
        newRoot = O.over OE.first_ .entryType root
            |> uncurry toTypedOperator
            |> fromTypedOperator
            |> run gSpec
    in
    { editor | root = newRoot }

parseEditor : Editor -> Editor
parseEditor ({gSpec, returnType, root} as editor) =
    let
        currentFormula = O.over OE.first_ .entryType root
            |> uncurry toTypedOperator
            |> renderFormula
            |> parse gSpec returnType

        newRoot = Either.toMaybe currentFormula
            |> Maybe.unwrap
                root
                (Tuple.second >> fromTypedOperator >> run gSpec)
    in
    { editor | currentFormula = currentFormula, root = newRoot }


-- lens helpers for navigating nested strucutres

--Input
literalType_ : O.SimpleLens ls Input T.LiteralType
literalType_ = O.lens .literalType (\s a -> { s | literalType = a })

value_ : O.SimpleLens ls Input (Maybe T.LiteralExpr)
value_ = O.lens .value (\s a -> { s | value = a })

userInput_ : O.SimpleLens ls Input (Maybe String)
userInput_ = O.lens .userInput (\s a -> { s | userInput = a })

errMess_ : O.SimpleLens ls Input (Maybe String)
errMess_ = O.lens .errMess (\s a -> { s | errMess = a })

-- Primitive
pInput_ : O.SimplePrism ls Primitive Input
pInput_ = O.prism PInput (\s -> case s of
    PInput x -> Right x
    _ -> Left s)

pOperator_ : O.SimplePrism ls Primitive Operator
pOperator_ = O.prism POperator (\s -> case s of
    POperator x -> Right x
    _ -> Left s)

-- Composite
cVarArgs_ : O.SimplePrism ls Composite T.PrimitiveType
cVarArgs_ = O.prism CVarArgs (\s -> case s of
    CVarArgs x -> Right x
    _ -> Left s)

cPacked_ : O.SimplePrism ls Composite (T.PrimitiveType, Operator)
cPacked_ = O.prism (uncurry CPacked) (\s -> case s of
    CPacked x y -> Right (x, y)
    _ -> Left s)

-- Node
primitive_ : O.SimplePrism ls Node Primitive
primitive_ = O.prism Primitive (\s -> case s of
    Primitive x -> Right x
    _ -> Left s)

composite_ : O.SimplePrism ls Node Composite
composite_ = O.prism Composite (\s -> case s of
    Composite x -> Right x
    _ -> Left s)

-- Union
primitiveType_ : O.SimpleLens ls Union T.PrimitiveType
primitiveType_ = O.lens .primitiveType <| \s a -> { s | primitiveType = a }

-- Entry a
unions_ : O.SimpleLens ls (Entry a) (Maybe Unions)
unions_ = O.lens .unions (\s a -> { s | unions = a })

justUnions_ : O.SimpleTraversal (Entry a) Unions
justUnions_ = o unions_ OE.just_

selector_ : O.SimpleLens ls (Entry a) (Maybe Selector)
selector_ = O.lens .selector (\s a -> { s | selector = a })

entryType_ : O.Lens ls (Entry a) (Entry b) a b
entryType_ = O.lens .entryType (\{unions, selector} b ->
    { unions = unions
    , selector = selector
    , entryType = b})

-- Arg or OptArg
key_ : O.SimpleLens ls ({ s | key : T.Key }) T.Key
key_ = O.lens .key (\s a -> { s | key = a })

entry_ : O.SimpleLens ls ({ s | entry : Entry a }) (Entry a)
entry_ = O.lens .entry (\s a -> { s | entry = a })

-- RowType
rArg_ : O.SimplePrism pr RowType Arg
rArg_ = O.prism RArg (\s -> case s of
    RArg a -> Right a
    _ -> Left s)

rOptArgs_ : O.SimplePrism pr RowType ()
rOptArgs_ = O.prism (always ROptArgs) (\s -> case s of
    ROptArgs -> Right ()
    _ -> Left s)

rOptArg_ : O.SimplePrism pr RowType OptArg
rOptArg_ = O.prism ROptArg (\s -> case s of
    ROptArg a -> Right a
    _ -> Left s)

rVarItem_ : O.SimplePrism pr RowType (Entry Primitive)
rVarItem_ = O.prism RVarItem (\s -> case s of
    RVarItem a -> Right a
    _ -> Left s)

rVarEnd_ : O.SimplePrism pr RowType ()
rVarEnd_ = O.prism (always RVarEnd) (\s -> case s of
    RVarEnd -> Right ()
    _ -> Left s)

-- invalid prism law but useful
rowEntry_ : O.SimplePrism pr RowType (Entry Node)
rowEntry_ = O.prism (Arg "INVALID" >> RArg) (\s -> case s of
    RArg {entry} -> Right entry
    ROptArg {entry} -> Right entry
    RVarItem entry -> Right <| O.over entryType_ Primitive entry
    _ -> Left s)

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
