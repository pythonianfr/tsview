module Editor.UI.Tree exposing (..)

import Array
import Either
import Maybe.Extra as Maybe
import Basics.Extra exposing (flip)

import Reader
import ReaderExtra as RE exposing (ask, asks)
import AssocList as Assoc
import List.Nonempty as NE
import RoseTree.Tree as Tree

import Browser
import Html as H exposing (Html)
import Html.Extra as HEX
import Html.Events as HE
import Html.Attributes as HA
import Html.Attributes.Extra as HAX
import Json.Decode as JD
import Parser.Advanced as PA exposing ((|.))
import Cmd.Extra exposing (withNoCmd, withCmd)

import Optics.Core as O exposing (o)
import OpticsExtra as OE

import AceEditor as Ace
import Editor.Type as T
import Editor.SpecRender exposing (renderPrimitiveType)
import Editor.SpecParser as Parser
import Editor.Render exposing (renderBool)

import Editor.UI.Type exposing (..)


type alias TreePath = Array.Array Int

type alias RowEnv =
    { treePath : TreePath
    , defaultValue : Maybe T.LiteralExpr
    }

type alias Reader a = Reader.Reader RowEnv a

type alias EReader a = Reader.Reader Editor a

type ListAction
    = AddItem Int
    | RemoveItem Int

type EntryAction
    = SelectUnion Int T.PrimitiveType
    | SelectOperator Operator
    | UnselectOperator T.PrimitiveType
    | ReadInput String

type EditAction
    = ToggleExpand
    | EditEntry EntryAction
    | EditList ListAction

type alias Model =
    { editor : Editor
    , specErrors : T.SpecErrors
    }


editor_ : O.SimpleLens ls Model Editor
editor_ = O.lens .editor <| \s a -> {s | editor = a}


type Msg
    = EditNode TreePath EditAction


type alias HMsg = Html Msg


parseLiteralExpr : T.LiteralType -> Parser.Parser c x T.LiteralExpr
parseLiteralExpr literalType = ask <| \({expecting} as env) ->
    Reader.run (Parser.literalParser literalType) env
        |. PA.spaces
        |. (PA.end <| expecting <| T.renderLiteralType literalType)

updateInput : String -> Input -> Input
updateInput s ({literalType} as input) =
    let
        setStr : (String -> a) -> Maybe a
        setStr f = if (s == "") then Nothing else (Just <| f s)

        setErr : String -> Maybe String
        setErr e = if (s == "") then Nothing else (Just e)

    in O.assign userInput_ (Just s) <| case literalType of
       T.String ->
           O.assign value_ (setStr T.StringExpr) input

       T.TimestampString ->
           O.assign value_ (setStr T.TimestampExpr) input

       T.SearchString ->
           O.assign value_ (setStr T.StringExpr) input

       _ -> parseLiteralExpr literalType
            |> Parser.run s
            |> Either.unpack
                (\e -> input
                    |> O.assign value_ Nothing
                    |> O.assign errMess_  (setErr e))
                (\x -> input
                    |> O.assign value_ (Just x)
                    |> O.assign errMess_  Nothing)

resetUnions : Unions -> Unions
resetUnions unions =
    let
        union = NE.head unions

        listUnions : T.PrimitiveType -> List Union -> List Union
        listUnions t xs = case t of
            T.Union ts ->
                let t_ = NE.head ts
                in listUnions t_ ((Union ts t_) :: xs)

            _ -> List.reverse xs

    in union
    |> O.get primitiveType_
    |> flip listUnions []
    |> NE.Nonempty union

updateEntryNode :
   EntryAction -> EntryForest Node -> EReader (EntryForest Node)
updateEntryNode entryAction ((entry, forest) as x) = case entryAction of
    SelectUnion i primitiveType -> entry
        |> O.assign
            (o justUnions_ (o (OE.neIx_ i) primitiveType_))
            primitiveType
        -- |> O.over justUnions_ resetUnions XXX handle nested unions (split)
        |> \e -> Maybe.unwrap
            (Reader.reader x)
            (\t -> updateEntryNode (UnselectOperator t) (e, forest))
            (O.getSome
                (o justUnions_ (o OE.neLast_ primitiveType_))
                e)

    SelectOperator {specOperator, returnType} -> ask <| \editor ->
        initializeOperator specOperator returnType
            |> Reader.andThen fromTypedOperator
            |> RE.run editor.gSpec
            |> O.over (o OE.first_ entryType_) (POperator >> Primitive)
            |> O.assign (o OE.first_ unions_) entry.unions

    UnselectOperator primitiveType -> ask <| \editor ->
        initializePrimitiveExpr primitiveType
            |> Reader.andThen fromPrimitiveExpr
            |> RE.run editor.gSpec
            |> O.over (o OE.first_ entryType_) Primitive
            |> O.assign (o OE.first_ unions_) entry.unions

    ReadInput s ->  Reader.reader <| O.over
        (o (o OE.first_ entryType_) (o primitive_ pInput_))
        (updateInput s)
        x

type alias RowForest = (RowType, EditionForest)

updateRowType : EntryAction -> RowForest -> EReader RowForest
updateRowType entryAction (rowType, forest) = case rowType of
    RArg ({entry} as x) -> updateEntryNode
        entryAction
        (entry, forest)
        |> Reader.map
            (O.over
                OE.first_
                (\e -> O.assign entry_ e x |> RArg))

    ROptArg ({entry} as x) -> updateEntryNode
        entryAction
        (entry, forest)
        |> Reader.map
            (O.over
                OE.first_
                (\e -> O.assign entry_ e x |> ROptArg))

    RVarItem entry -> updateEntryNode
        entryAction
        (O.over entryType_ Primitive entry, forest)
        |> Reader.map
            (O.over
                OE.first_
                (castEntryType primitive_ >> Maybe.unwrap rowType RVarItem))

    _ -> Reader.reader (rowType, forest)

initAddItem : Editor -> T.PrimitiveType -> EditionTree
initAddItem {gSpec} primitiveType =
    initializePrimitiveExpr primitiveType
        |> Reader.andThen fromPrimitiveExpr
        |> RE.run gSpec
        |> O.over OE.first_ (RVarItem >> newEditionRow)
        |> newTree

updateNode : EditAction -> EditionTree -> EReader EditionTree
updateNode editAction tree = ask <| \editor -> case editAction of
    ToggleExpand -> O.over
        (o OE.node_ (o isExpand_ OE.just_))
        (not)
        tree

    EditEntry entryAction ->
        let node = O.get OE.node_ tree
        in Tuple.pair
            (O.get rowType_ node)
            (O.get OE.forest_ tree)
            |> updateRowType entryAction
            |> RE.run editor
            |> O.over
                OE.first_
                (\r -> O.assign rowType_ r node)
            |> newTree

    EditList (RemoveItem i) ->
        Tree.removeAt [i] tree

    EditList (AddItem i) -> (o composite_ cVarArgs_)
        |> o (o rowType_ rowEntryType_)
        |> o OE.node_
        |> flip O.getSome tree
        |> Maybe.unwrap
            tree
            (\t -> Tree.insertBefore [i] (initAddItem editor t) tree)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    EditNode treePath editAction -> model
        |> O.over (o editor_ root_) (\root ->
            O.review treeRoot_ root
            |> O.over
                (OE.treeIx_ <| Array.toList treePath)
                (updateNode editAction >> RE.run model.editor)
            |> O.getSome treeRoot_ |> Maybe.withDefault root)
        |> O.over editor_ parseEditor
        |> withNoCmd


-- HTML rendering

renderLiteralExpr : T.LiteralExpr -> String
renderLiteralExpr literalExpr = case literalExpr of
    T.StringExpr x -> x

    T.TimestampExpr x -> x

    _ -> Editor.SpecRender.renderLiteralExpr literalExpr

renderCheckInput : Input -> Reader HMsg
renderCheckInput {value} =
    let
        getBool : Maybe T.LiteralExpr -> Maybe Bool
        getBool literalExpr = case literalExpr of
            Just (T.BoolExpr x) -> Just x

            _ -> Nothing

    in
    ask <| \{treePath, defaultValue} -> H.span []
    [ H.input
        [ HA.type_ "checkbox"
        , HA.checked <| Maybe.withDefault
            (Maybe.withDefault False <| getBool defaultValue)
            (getBool value)
        , HE.onCheck
            (renderBool >> ReadInput >> EditEntry >> EditNode treePath)
        ]
        []
    ]

renderInput : Input -> Reader HMsg
renderInput {literalType, value, userInput, errMess} =
    ask <| \{treePath, defaultValue} -> H.span []
    [ H.input
        [ HA.value <| Maybe.withDefault
            (Maybe.unwrap "" renderLiteralExpr value)
            userInput
        , HA.placeholder <| Maybe.unwrap "" renderLiteralExpr defaultValue
        , HE.onInput (ReadInput >> EditEntry>> EditNode treePath)
        ]
        []
    , H.text " "
    , H.text <| "[" ++ T.renderLiteralType literalType ++ "]"
    , H.text " "
    , HEX.viewMaybe H.text errMess
    ]

renderNode : Node -> Reader HMsg
renderNode node = case node of
    Primitive (PInput ({literalType} as x)) -> case literalType of
        T.Bool -> renderCheckInput x

        _ -> renderInput x

    Primitive (POperator op) -> Reader.reader <| H.span []
        [ H.text <| "Operator(" ++ op.specOperator.name ++")"
        ]

    _ -> Reader.reader <| HEX.nothing

type alias SelectArgs =
    { current : String
    , labels : List String
    , toEditAction : String -> Maybe EntryAction
    }

renderSelect : SelectArgs -> Reader HMsg
renderSelect {current, labels, toEditAction} = asks .treePath <| \treePath ->
    let
        renderOption : String -> HMsg
        renderOption label = H.option
            [ HA.value label
            , HA.selected (label == current)
            ]
            [ H.text label ]

    in H.select
    [ HE.targetValue
        |> JD.andThen (\s -> Maybe.unwrap
            ("No message for " ++ current |> JD.fail)
            (EditEntry >> EditNode treePath >> JD.succeed)
            (toEditAction s))
        |> HE.on "change"
    ]
    (List.map renderOption labels)

renderUnion : Node -> Int -> Union -> Reader HMsg
renderUnion node i {primitiveTypes, primitiveType} =
    renderSelect
        { current = renderPrimitiveType primitiveType
        , labels = NE.toList <| NE.map renderPrimitiveType primitiveTypes
        , toEditAction = \s -> Parser.primitiveTypeParser
            |> Parser.run s
            |> Either.toMaybe
            |> Maybe.map (SelectUnion i)
        }

-- XXX should be improved (workshop)
fromReturnType : T.ReturnType -> T.PrimitiveType
fromReturnType returnType = case T.fromReturnType returnType |> NE.head of
    T.ReturnLiteral t -> T.Literal t

    T.ReturnOperatorOutput t -> T.OperatorOutput t

renderSelector : Node -> Selector -> Reader HMsg
renderSelector node {operators, returnType} = case node of
    Primitive _ ->
        let unselected = "___"
        in renderSelect
            { current = O.getSome (o primitive_ pOperator_) node
                |> Maybe.unwrap unselected (\op -> op.specOperator.name)
            , labels = unselected :: (Assoc.keys operators)
            , toEditAction = \s -> Assoc.get s operators
                |> Maybe.unwrap
                    (UnselectOperator <| fromReturnType returnType)
                    (\x -> SelectOperator <| Operator x returnType)
                |> Just
            }

    Composite _ -> Reader.reader HEX.nothing -- XXX Packed

renderEntry : Entry Node -> Reader (List HMsg)
renderEntry {unions, selector, entryType} =
    let
        renderUnions : List Union -> List (Maybe (Reader HMsg))
        renderUnions = List.indexedMap
            (\i x -> renderUnion entryType i x |> Just)

    in RE.sequence <| Maybe.values <| List.append
        (Maybe.unwrap [] (NE.toList >> renderUnions) unions)
        [ Maybe.map (renderSelector entryType) selector
        , Just <| renderNode entryType
        ]

liftTuple : (a, Reader b) -> Reader (a, b)
liftTuple (a, rb) = Reader.map2 Tuple.pair (Reader.reader a) rb

prefix : { bullet : Bool, label : String } -> HMsg
prefix {bullet, label} = H.span []
    [ if bullet then (H.text " • ") else HEX.nothing
    , H.text label
    ]

listButton : (Int -> ListAction) -> String -> Reader HMsg
listButton listAction symbol = asks .treePath <| \treePath -> H.span []
    [ H.button
        [ Array.toList treePath
            |> O.getSome OE.consLast_
            |> Maybe.map (\(i, path) ->
                listAction i |> EditList |> EditNode (Array.fromList path))
            |> HAX.attributeMaybe HE.onClick
        ]
        [ H.text symbol ]
    ]

renderRowType : RowType -> Reader (HMsg, List HMsg)
renderRowType rowType = liftTuple <| case rowType of
    RArg {key, entry} ->
        ( prefix {bullet = True, label = key}
        , renderEntry entry
        )

    ROptArgs ->
        ( prefix {bullet = False, label = "Options"}
        , Reader.reader []
        )

    ROptArg {key, defaultValue, entry} ->
        ( prefix {bullet = True, label = key}
        , Reader.local
            (O.assign defaultValue_ defaultValue)
            (renderEntry entry)
        )

    RVarItem entry ->
        ( prefix {bullet = True, label = ""}
        , Reader.map List.concat <| RE.sequence
            [ renderEntry <| O.over entryType_ Primitive entry
            , listButton RemoveItem "-" |> Reader.map List.singleton
            ]
        )

    RVarEnd ->
        ( prefix {bullet = True, label = ""}
        , listButton AddItem "+" |> Reader.map List.singleton
        )

renderExpand : Maybe Bool -> Reader HMsg
renderExpand mIsExpand = asks .treePath <| \treePath ->
    flip HEX.viewMaybe mIsExpand <| \isExpand -> H.a
        [ HE.onClick <| EditNode treePath ToggleExpand ]
        [ H.text <| if isExpand then "-" else "+" ]

type alias HRow =
    { expandNode : HMsg
    , indent : Int
    , prefixNode : HMsg
    , entryNodes : List HMsg
    }

expandNode_ : O.SimpleLens ls HRow HMsg
expandNode_ = O.lens .expandNode <| \s a -> {s | expandNode = a}

prefixNode_ : O.SimpleLens ls HRow HMsg
prefixNode_ = O.lens .prefixNode <| \s a -> {s | prefixNode = a}

renderEditionRow : Int -> EditionRow -> Reader HRow
renderEditionRow indent {rowType, isExpand} = Reader.map2
    (\expandNode (prefixNode, entryNodes) ->
        { expandNode = expandNode
        , indent = indent
        , prefixNode = prefixNode
        , entryNodes = entryNodes
        })
    (renderExpand isExpand)
    (renderRowType rowType)

doExpand : EditionTree -> Bool
doExpand tree = case O.get (o OE.node_ isExpand_) tree of
    Just True -> True

    Just False -> False

    Nothing -> True

renderTree : Int -> EditionTree -> Reader (List HRow)
renderTree indent tree =
    let
        render : (Int, EditionTree) -> Reader (List HRow)
        render (i, t) = Reader.local
            (O.over treePath_ (Array.push i))
            (renderTree (indent + 1) t)

    in (if (doExpand tree) then (O.get OE.forest_ tree) else [])
        |> List.indexedMap Tuple.pair
        |> RE.traverse render
        |> Reader.map List.concat
        |> Reader.map2
            (::)
            (renderEditionRow indent <| O.get OE.node_ tree)

renderHRow : HRow -> List HMsg
renderHRow {expandNode, indent, prefixNode, entryNodes} =
    [ H.div
        [ HA.class "col_expand center_item"
        ]
        [ expandNode ]
    , H.div
        [ HA.class "col_editor"
        , HA.style "padding-left" (String.fromInt (2 * indent) ++ "em")
        ]
        (prefixNode :: entryNodes)
    ]

view : Model -> HMsg
view {editor} =
    let
        col cls txt =
            H.header [ HA.class cls ] [ H.text txt ]

    in O.review treeRoot_ editor.root
        |> renderTree 0
        |> RE.run (RowEnv Array.empty Nothing)
        |> O.assign (o OE.listHead_ prefixNode_) HEX.nothing
        |> List.concatMap renderHRow
        |> List.append
            [ col "col_expand header" "Expand"
            , col "col_editor header" "Editor"
            ]
        |> H.section [ HA.class "ui_editor" ]


-- Internal application for development

type EditionMode
    = ReadOnly
    | EditionMode


editorHeight =
    HA.attribute "style" "--min-height-editor: 36vh"

viewHeader : EditionMode -> Html Msg
viewHeader editionMode =
    let
        ( newState, sign ) = case editionMode of
            ReadOnly -> ( EditionMode, "✎" )

            EditionMode -> ( ReadOnly, "💾" )
    in H.header
    [ HA.class "code_left" ]
    [ H.span [] [ H.text "Formula edition" ]
  --  , H.a [ Events.onClick (ChangeState newState) ] [ H.text sign ]
    ]

viewError : T.CurrentFormula -> List (Html Msg)
viewError = Either.unpack
        (\(_, errs) -> Parser.renderParserErrors errs
            |> String.lines
            |> List.map (\x -> H.span
                [ HA.class "error" ]
                [ H.text x ]))
        (always [])

renderReadOnly : Model -> List HMsg
renderReadOnly model =
    [ H.div
        [ HA.class "code_left" , editorHeight ]
        [ viewHeader ReadOnly
        , Ace.readOnly_ <| Either.unpack
            Tuple.first
            Tuple.first
            model.editor.currentFormula
        , H.footer
            [ HA.class "code_left" ]
            (viewError model.editor.currentFormula)
        ]
    ]
    |> H.section [ HA.class "code_editor" ]
    |> (\x -> [ x, view model ])

viewTree : Model -> HMsg
viewTree model =
    renderReadOnly model
    |> H.div [ HA.class "code_left" ]
    |> List.singleton
    |> H.article [ HA.class "main" ]

updateTree : Msg -> Model -> (Model, Cmd Msg)
updateTree msg model = update msg model

initModel : JD.Value -> Model
initModel jsonSpec =
    Parser.parseSpecValue jsonSpec |> \(errs, spec) ->
 --       { editor = initEditor (T.buildGSpec spec) T.returnSeries
        { editor = buildEditor (T.buildGSpec spec) T.returnSeries formulaDev
        , specErrors = errs
        }
        |> O.over editor_ parseEditor

main : Program JD.Value Model Msg
main = Browser.element
    { init = initModel >> withNoCmd
    , update = updateTree
    , view = viewTree
    , subscriptions = always Sub.none
    }


-- Optics utils
treePath_ : O.SimpleLens ls RowEnv TreePath
treePath_ = O.lens .treePath <| \s a -> { s | treePath = a }

defaultValue_ : O.SimpleLens ls RowEnv (Maybe T.LiteralExpr)
defaultValue_ = O.lens .defaultValue <| \s a -> { s | defaultValue = a }


-- dev
formulaDev : String
formulaDev = """
(priority
    (+
        3
        (series "a.b" #:weight 3.5))
    (priority
        (*
            10.3
            (series "gaz.es"))
        (+
            -1.7
            (series "gaz.pt" #:weight 0.3)
            #:flag #t)
        (series "gaz.es.pt.predicted" #:fill 2 #:weight 1.1)
        #:k2 (timedelta
            (today)
            #:years 5))
    (series "gaz.nl" #:fill "all" #:weight 3)
    (*
        1.2
        (series "gaz.fr"))
    (series "gaz.de" #:fill 79)
    #:k1 4.7)
    """

