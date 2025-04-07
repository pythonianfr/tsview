module Editor.UI.Tree exposing (..)

import Array
import Either
import Maybe.Extra as Maybe
import Basics.Extra exposing (flip)
import Cmd.Extra exposing (withNoCmd, withCmd)
import List.Extra exposing (zip)

import Reader
import ReaderExtra as RE exposing (ask, asks)
import List.Nonempty as NE
import RoseTree.Tree as Tree
import Optics.Core as O exposing (o)
import Parser.Advanced as PA exposing ((|.))

import Http
import Browser
import Url.Builder as UB
import Json.Encode as JE
import Json.Decode as JD
import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA

import HtmlExtra as HX
import Util exposing (sendCmd)


import AssocList as Assoc
import OpticsExtra as OE
import ParserExtra as PE
import AceEditor as Ace

import Editor.Type as T
import Editor.SpecRender exposing
    ( renderLiteralType
    , renderPrimitiveType
    )
import Editor.SpecParser exposing
    ( literalParser
    , primitiveTypeParser
    , parseSpecValue
    )
import Editor.Render exposing (renderBool)

import Editor.UI.Type exposing (..)


type alias TreePath = Array.Array Int

type alias Proposals = Assoc.Dict T.ProposalType (List String)

type alias RowEnv =
    { tree : EditionTree
    , treePath : TreePath
    , defaultValue : Maybe T.LiteralExpr
    , proposals : Proposals
    }

type alias Reader a = Reader.Reader RowEnv a

type alias EReader a = Reader.Reader Editor a

type ListAction
    = AddItem Int
    | SwapBefore Int
    | SwapAfter Int
    | RemoveItem Int
    | InsertBefore Int
    | InsertAfter Int

type SelectorAction
    = OpenSelector Bool
    | SetKeywords String
    | SelectItem String

type EntryAction
    = SelectUnion T.PrimitiveType
    | SelectOperator T.Operator
    | UnselectOperator T.PrimitiveType
    | SelectVarArgs T.Packed
    | SelectPacked T.Packed T.Operator
    | ReadInput String
    | SelectorAction SelectorAction

type EditAction
    = ToggleExpand
    | EditEntry EntryAction
    | EditList ListAction

type alias Model =
    { editor : Editor
    , proposals : Proposals
    , specErrors : T.SpecErrors
    }


editor_ : O.SimpleLens ls Model Editor
editor_ = O.lens .editor <| \s a -> {s | editor = a}


type Msg
    = EditNode TreePath EditAction
    | TreeEdited T.FormulaCode
    | Edit T.FormulaCode
    | GotProposals T.ProposalType (Result Http.Error (List String))


type alias HMsg = Html Msg


type alias SelectorInput =
    { selectedItem : Maybe String
    , keywords : String
    , isOpen : Bool
    }

encodeSelectorInput : SelectorInput -> String
encodeSelectorInput s = JE.encode 0 <| JE.object
    [ ("selectedItem", Maybe.unwrap JE.null JE.string s.selectedItem)
    , ("keywords", JE.string s.keywords)
    , ("isOpen", JE.bool s.isOpen)
    ]

decodeSelectorInput : String -> Maybe SelectorInput
decodeSelectorInput s =
    let toMaybe = Either.toMaybe << Either.fromResult
    in toMaybe <| flip JD.decodeString s <| JD.map3 SelectorInput
        (JD.field "selectedItem" <| JD.nullable JD.string)
        (JD.field "keywords" JD.string)
        (JD.field "isOpen" JD.bool)

decodeSelectorInputFromUserInput : Maybe String -> SelectorInput
decodeSelectorInputFromUserInput userInput =
    Maybe.withDefault
        (SelectorInput Nothing "" False)
        (Maybe.join <| Maybe.map decodeSelectorInput userInput)

parseLiteralExpr : T.LiteralType -> PE.Parser c x T.LiteralExpr
parseLiteralExpr literalType = ask <| \({expecting} as env) ->
    Reader.run (literalParser literalType) env
        |. PA.spaces
        |. (PA.end <| expecting <| renderLiteralType literalType)

updateInput : String -> Input -> Input
updateInput s ({literalType} as input) =
    let
        setStr : (String -> a) -> Maybe a
        setStr f = if (s == "") then Nothing else (Just <| f s)

        setErr : String -> Maybe String
        setErr e = if (s == "") then Nothing else (Just e)

        defaultUpdate = parseLiteralExpr literalType
            |> PE.run s
            |> Either.unpack
                (\e -> input
                    |> O.assign value_ Nothing
                    |> O.assign errMess_  (setErr e))
                (\x -> input
                    |> O.assign value_ (Just x)
                    |> O.assign errMess_  Nothing)

    in O.assign userInput_ (Just s) <| case literalType of
       T.String ->
           O.assign value_ (setStr T.StringExpr) input

       T.TimestampString ->
           O.assign value_ (setStr T.TimestampExpr) input

       T.Proposal _ ->
           O.assign value_ (setStr T.StringExpr) input

       T.LiteralKeywords _ ->
           O.assign value_ (setStr T.LiteralKeywordExpr) input

       T.Bool -> case s of
            "" -> O.assign value_ Nothing input

            _ -> defaultUpdate

       _ -> defaultUpdate

updateSelector : SelectorAction -> Input -> Input
updateSelector act ({userInput} as input) =
    let
        selectorInput = decodeSelectorInputFromUserInput userInput

        store : (SelectorInput, Input) -> Input
        store (s, i) = O.assign
            userInput_
            (Just <| encodeSelectorInput s)
            i

    in store <| case act of
        OpenSelector isOpen ->
            ( {selectorInput | isOpen = isOpen}
            , input
            )

        SetKeywords s ->
            ( {selectorInput | keywords = s}
            , input
            )

        SelectItem s ->
            ( { selectorInput
                | selectedItem = Just s
                , isOpen = False }
            , { input | value = Just <| T.StringExpr s }
            )

updateEntryNode :
   EntryAction -> EntryForest Node -> EReader (EntryForest Node)
updateEntryNode entryAction ((entry, forest) as x) = case entryAction of
    SelectUnion primitiveType -> entry
        |> O.assign (o justUnion_ primitiveType_) primitiveType
        |> \e -> updateEntryNode (UnselectOperator primitiveType) (e, forest)

    SelectOperator op -> asks .spec <| \spec ->
        initializeOperator op
            |> Reader.andThen fromTypedOperator
            |> RE.run spec
            |> O.assign (o OE.first_ entryType_) (Primitive <| POperator op)
            |> O.assign (o OE.first_ union_) entry.union

    UnselectOperator primitiveType -> asks .spec <| \spec ->
        initializePrimitiveExpr primitiveType
            |> Reader.andThen fromPrimitiveExpr
            |> RE.run spec
            |> O.over (o OE.first_ entryType_) Primitive
            |> O.assign (o OE.first_ union_) entry.union

    SelectVarArgs packed -> asks .spec <| \spec ->
        initializeTypedExpr (T.PackedType packed)
            |> Reader.andThen fromArgExpr
            |> RE.run spec

    SelectPacked packed op -> asks .spec <| \spec ->
        initializeOperator op
            |> Reader.andThen fromTypedOperator
            |> RE.run spec
            |> O.assign (o OE.first_ entryType_) (Packed packed op)

    ReadInput s ->  Reader.reader <| O.over
        (o (o OE.first_ entryType_) (o primitive_ pInput_))
        (updateInput s)
        x

    SelectorAction act -> Reader.reader <| O.over
        (o (o OE.first_ entryType_) (o primitive_ pInput_))
        (updateSelector act)
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
initAddItem {spec} primitiveType =
    initializePrimitiveExpr primitiveType
        |> Reader.andThen fromPrimitiveExpr
        |> RE.run spec
        |> O.over OE.first_ (RVarItem >> newEditionRow)
        |> newTree

updateExpand : EditionRow -> EditionRow
updateExpand ({rowType, isExpand} as x) =
    let default = defaultIsExpand rowType
    in case (default, isExpand) of
        (Nothing, _) -> O.assign isExpand_ Nothing x

        (_, Nothing) -> O.assign isExpand_ default x

        _ -> x


updateNode : EditAction -> EditionTree -> EReader EditionTree
updateNode editAction tree = ask <| \editor ->
    let
        addItem : Int -> EditionTree
        addItem i = varArgs_
            |> o (o rowType_ rowEntryType_)
            |> o OE.node_
            |> flip O.getSome tree
            |> Maybe.unwrap
                tree
                (\(T.Packed t) -> Tree.insertBefore [i] (initAddItem editor t) tree)

        swapItem : Int -> Int -> EditionTree
        swapItem sourceIndex targetIndex = Maybe.map2
            Tuple.pair
            (Tree.get [sourceIndex] tree)
            (Tree.get [targetIndex] tree)
            |> Maybe.unwrap
                tree
                (\(sourceItem, targetItem) -> tree
                    |> Tree.replaceAt [sourceIndex] targetItem
                    |> Tree.replaceAt [targetIndex] sourceItem
                )

    in case editAction of
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
                |> O.over OE.first_ updateExpand
                |> newTree

        EditList (SwapBefore i) ->
            swapItem i (i - 1)

        EditList (SwapAfter i) ->
            swapItem i (i + 1)

        EditList (RemoveItem i) ->
            Tree.removeAt [i] tree

        EditList (InsertBefore i) ->
            addItem i

        EditList (InsertAfter i) ->
            addItem (i + 1)

        EditList (AddItem i) ->
            addItem i

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GotProposals proposalType (Ok xs) ->
        let proposals = Assoc.insert proposalType xs model.proposals
        in { model  | proposals = proposals } |> withNoCmd

    GotProposals _ (Err _) ->
        model |> withNoCmd

    EditNode treePath editAction -> model
        |> O.over (o editor_ root_) (\root ->
            O.review treeRoot_ root
            |> O.over
                (OE.treeIx_ <| Array.toList treePath)
                (updateNode editAction >> RE.run model.editor)
            |> O.getSome treeRoot_ |> Maybe.withDefault root)
        |> O.over editor_ updateFormula
        |> (\m -> ( m, probeTreeEdited editAction m))

    Edit code ->
        let
            {spec, returnTypeStr} = model.editor

            newEditor = buildEditor spec returnTypeStr code
        in
        { model | editor = newEditor } |> withNoCmd

    TreeEdited _ ->
        model |> withNoCmd


-- HTML rendering

renderLiteralExpr : T.LiteralExpr -> String
renderLiteralExpr literalExpr = case literalExpr of
    T.StringExpr x -> x

    T.TimestampExpr x -> x

    T.LiteralKeywordExpr x -> x

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
    , H.button
        [ HE.onClick (ReadInput "" |> EditEntry |> EditNode treePath)
        ]
        [ H.text <| Util.fromCharCode 8634 ] -- ANTICLOCKWISE OPEN CIRCLE ARROW
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
    , H.text <| "[" ++ renderLiteralType literalType ++ "]"
    , H.text " "
    , HX.viewMaybe H.text errMess
    ]

renderClosedSelector :
    Input -> SelectorInput -> (SelectorAction -> Msg) -> HMsg
renderClosedSelector {value} {selectedItem} toMsg =
    let
        name =
            Maybe.withDefault
            (Maybe.unwrap "" renderLiteralExpr value)
            selectedItem
    in
    H.span
        [ HA.title name ]
        [ H.input
            [ HA.value name
            , HA.disabled True
            ]
            []
        , H.button
            [ HE.onClick (OpenSelector True |> toMsg) ]
            [ H.text <| Util.fromCharCode 9998 ] -- LOWER RIGHT PENCIL
        ]

renderOpenSelector :
    SelectorInput -> List String -> (SelectorAction -> Msg) -> HMsg
renderOpenSelector {keywords, selectedItem } rawItems toMsg =
    let
        limitResults xs =
            let len = List.length xs
            in if (len > 100) then
                [ ((String.fromInt len) ++  " results", False) ]
            else if len ==0 then
                    [ ("No result", False) ]
                else
                    (List.map (\x -> (x, True)) xs)

        items = List.foldl
            (\word xs -> List.filter (String.contains word) xs)
            rawItems
            (String.words keywords)
            |> limitResults

        width = List.map (Tuple.first >> String.length) items
            |> List.maximum
            |> Maybe.withDefault 20
            |> \x -> round (1.1 * toFloat x)

        renderItem (x, enabled) = H.button
            [ HA.class "list-group-item list-group-item-action"
            , HA.classList [("active", (Just x == selectedItem))]
            , HA.disabled (not enabled)
            , HE.onMouseDown (SelectItem x |> toMsg)
            ]
            [ H.text x ]

    in H.span
    [ HA.class "proposal_dropdown"
    ]
    [ H.input
        [ HA.value keywords
        , HA.placeholder "Type keywords for selection"
        , HE.onInput (SetKeywords >> toMsg)
        ]
        []
    , H.button
        [ HE.onClick (OpenSelector False |> toMsg)
        ]
        [ H.text <| Util.fromCharCode 10060 ] -- CROSS MARK
    , H.div
        [ HA.class "proposal_dropdown_menu"
        , HA.style "min-width" (String.fromInt width ++ "ch")
        ]
        [ H.div
            [ HA.class "list-group" ]
            (List.map renderItem items)
        ]
    ]

renderProposals : T.ProposalType -> Input -> Reader HMsg
renderProposals proposalType ({userInput} as input) =
    ask <| \{proposals, treePath} ->
    let
        selectorInput = decodeSelectorInputFromUserInput userInput

        toMsg : SelectorAction -> Msg
        toMsg msg = msg |> SelectorAction |> EditEntry |> EditNode treePath

        proposalList =
            Assoc.get proposalType proposals |> Maybe.withDefault []

    in if selectorInput.isOpen || Maybe.isNothing input.value then
        renderOpenSelector selectorInput proposalList toMsg
    else
        renderClosedSelector input selectorInput toMsg

renderKeywords : List String -> Input -> Reader HMsg
renderKeywords keywords {value} =
    let unselected = ""
        values = unselected :: keywords
    in renderSelect
        { current = Maybe.unwrap unselected renderLiteralExpr value
        , values = values
        , labels = values
        , toEditAction = ReadInput >> Just
        }  

renderNode : Node -> Reader HMsg
renderNode node = case node of
    Primitive (PInput ({literalType} as x)) -> case literalType of
        T.Bool -> renderCheckInput x

        T.Proposal proposalType -> renderProposals proposalType  x

        T.LiteralKeywords keywords -> renderKeywords keywords x

        _ -> renderInput x

    _ -> Reader.reader <| HX.nothing

type alias SelectArgs =
    { current : String
    , values : List String
    , labels : List String
    , toEditAction : String -> Maybe EntryAction
    }

renderSelect : SelectArgs -> Reader HMsg
renderSelect {current, values, labels, toEditAction} = asks .treePath <| \treePath ->
    let
        renderOption : (String, String) -> HMsg
        renderOption (value, label) = H.option
            [ HA.value value
            , HA.selected (value == current)
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
    (List.map renderOption <| zip values labels)

renderUnion : Node -> Union -> Reader HMsg
renderUnion node {primitiveTypes, primitiveType} =
    let values = NE.toList <| NE.map renderPrimitiveType primitiveTypes
        labels = flip List.map values <| \value ->
            if String.startsWith "Literal" value then
                "Keywords"
            else
                value
    in renderSelect
        { current = renderPrimitiveType primitiveType
        , values = values
        , labels = labels
        , toEditAction = \s -> primitiveTypeParser
            |> PE.run s
            |> Either.toMaybe
            |> Maybe.map SelectUnion
        }

fromReturnType : T.ReturnType -> T.PrimitiveType
fromReturnType returnType = case returnType of
    T.ReturnPrimitiveType p -> p

    T.ReturnList p -> p

renderSelector : Node -> Selector -> Reader HMsg
renderSelector node {operators, returnType} =
    let
        unselected = "   "

        values = unselected :: (Assoc.keys operators)

        toEditAction toUnselectAction toSelectAction = \s ->
            Assoc.get s operators
                |> Maybe.unwrap toUnselectAction toSelectAction
                |> Just

    in renderSelect <| case node of
        Primitive _ ->
            { current = O.getSome (o primitive_ pOperator_) node
                |> Maybe.unwrap unselected .name
            , values = values
            , labels = values
            , toEditAction = toEditAction
                (UnselectOperator <| fromReturnType returnType)
                SelectOperator
            }

        VarArgs packed ->
            { current = unselected
            , values = values
            , labels = values
            , toEditAction = toEditAction
                (SelectVarArgs packed)
                (SelectPacked packed)
            }

        Packed packed op ->
            { current = op.name
            , values = values
            , labels = values
            , toEditAction = toEditAction
                (SelectVarArgs packed)
                (SelectPacked packed)
            }

renderEntry : Entry Node -> Reader (List HMsg)
renderEntry {union, selector, entryType} = RE.sequence <| Maybe.values
    [ Maybe.map (renderUnion entryType) union
    , Maybe.map (renderSelector entryType) selector
    , Just <| renderNode entryType
    ]

liftTuple : (a, Reader b) -> Reader (a, b)
liftTuple (a, rb) = Reader.map2 Tuple.pair (Reader.reader a) rb

prefix : { bullet : Bool, label : String } -> HMsg
prefix {bullet, label} = H.span []
    [ if bullet then (H.text " • ") else HX.nothing
    , H.text label
    ]

listButton : (Int -> ListAction) -> String -> Reader HMsg
listButton listAction symbol = ask <| \{tree, treePath} ->
    let
        mBind = flip Maybe.andThen

        isSwapBefore = case listAction 0 of
            SwapBefore _ -> True
            _ -> False

        isSwapAfter = case listAction 0 of
            SwapAfter _ -> True
            _ -> False

        mPath = O.getSome OE.arrUnconsLast_ treePath

        disabled : Bool
        disabled = Maybe.withDefault False <|
            mBind mPath <| \(i, path) ->
            mBind (O.getSome (o (OE.treeArrIx_ path) OE.forest_) tree) <| \forest ->
                let itemsNb = List.length forest - 1 -- remove RVarEnd
                in Just <|
                    (isSwapBefore && i == 0)  ||
                    (isSwapAfter  && i == itemsNb - 1)

    in H.span []
        [ H.button
            [ Maybe.map
                (\(i, path) -> listAction i |> EditList |> EditNode path)
                mPath
                |> HX.attributeMaybe HE.onClick
            , HA.disabled disabled
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
        , Reader.map2 List.append
            (renderEntry <| O.over entryType_ Primitive entry)
            <| RE.sequence
                [ listButton SwapBefore <| Util.fromCharCode 8613
                    -- UPWARDS ARROW FROM BAR
                , listButton SwapAfter <| Util.fromCharCode 8615
                    -- DOWNWARDS ARROW FROM BAR
                , listButton RemoveItem "X"
                , listButton InsertBefore <| Util.fromCharCode 8624
                    -- UPWARDS ARROW WITH TIP LEFTWARDS
                , listButton InsertAfter <| Util.fromCharCode 8629
                    -- DOWNWARDS ARROW WITH CORNER LEFTWARDS
                ]
        )

    RVarEnd ->
        ( prefix {bullet = True, label = ""}
        , listButton AddItem "+" |> Reader.map List.singleton
        )

renderExpand : Maybe Bool -> Reader HMsg
renderExpand mIsExpand = asks .treePath <| \treePath ->
    flip HX.viewMaybe mIsExpand <| \isExpand -> H.a
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
view {editor, proposals} =
    let
        col cls txt =
            H.header [ HA.class cls ] [ H.text txt ]

    in O.review treeRoot_ editor.root
        |> \tree -> RE.run
            (RowEnv tree Array.empty Nothing proposals)
            (renderTree 0 tree)
        |> O.assign (o OE.listHead_ prefixNode_) HX.nothing
        |> List.map renderHRow
        |> List.concat
        |> List.append
            [ col "col_expand header" "Expand"
            , col "col_editor header" "Editor"
            ]
        |> H.section [ HA.class "ui_editor" ]


renderErrorRow : Int -> PE.Annotation -> Html msg
renderErrorRow charCode {rowPos, colPos, errMess} = H.tr
    []
    [ H.td [] [ H.text <| Util.fromCharCode charCode ]
    , H.td [] [ H.text <| String.fromInt rowPos ]
    , H.td [] [ H.text <| String.fromInt colPos ]
    , H.td [] [ H.text <| errMess ]
    ]

renderParserError : PE.ParserError -> NE.Nonempty (Html msg)
renderParserError {annotation, contextStack} = NE.Nonempty
    (renderErrorRow 9888 annotation) -- WARNING SIGN
    (Maybe.unwrap
        []
        (NE.map (renderErrorRow 8505) >> NE.toList) -- INFORMATION SOURCE
        contextStack
    )

renderParserErrors : PE.ParserErrors -> Html msg
renderParserErrors xs =
    let
        tableHead = ["", "Line", "Col.", "Message"]
            |> List.map
                (\x -> H.th [ HA.scope "col" ] [ H.text x ])
            |> H.tr []
            |> List.singleton
            |> H.thead []

    in H.table
    [ HA.class "table" ]
    [ tableHead
    , H.tbody
        []
        (NE.concatMap renderParserError xs |> NE.toList)
    ]

getParserErrors : Model -> Maybe PE.ParserErrors
getParserErrors {editor} =
    editor.currentFormula |> Either.leftToMaybe |> Maybe.map Tuple.second

viewErrors : Model -> Html msg
viewErrors model = case T.getCode model.editor.currentFormula of
    "()" -> HX.nothing

    _ -> getParserErrors model |> HX.viewMaybe renderParserErrors

hasErrors : Model -> Bool
hasErrors { editor } = Either.isLeft editor.currentFormula

viewErrors_ : Maybe (List String) -> Html msg
viewErrors_ mList = flip HX.viewMaybe mList <| \xs ->
    H.div []
    (List.map (\x -> H.span [ HA.class "error" ] [ H.text x ]) xs)

viewSpecErrors : Model -> Html msg
viewSpecErrors {specErrors} = specErrors
    |> Maybe.map NE.toList
    |> viewErrors_


type alias Flags =
    { urlPrefix : String
    , jsonSpec : JD.Value
    , formulaCode : Maybe T.FormulaCode
    , returnTypeStr : T.ReturnTypeStr
    }


init_ :
    Maybe T.FormulaCode ->
    T.ReturnTypeStr ->
    (T.SpecErrors, T.Spec) ->
    Model
init_ formulaCode returnTypeStr (errs, spec) =
    { editor = Maybe.unwrap
        (initEditor spec returnTypeStr)
        (\code -> buildEditor spec returnTypeStr code |> updateFormula)
        formulaCode
    , proposals = Assoc.empty
    , specErrors = errs
    }

getSeriesName : String -> Cmd Msg
getSeriesName urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "find" ]
        [ UB.string "query" "(by.everything)"
        , UB.string "meta" "false"
        ]
    , expect = Http.expectJson
        (GotProposals T.SeriesName)
        (JD.list (JD.field "name" JD.string))
    }

getSource : String -> Cmd Msg
getSource urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "sources" ]
        []
    , expect = Http.expectJson
        (GotProposals T.Source)
        (JD.list (JD.index 0 JD.string))
    }

getMetaKey : String -> Cmd Msg
getMetaKey urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "metadata-keys" ]
        []
    , expect = Http.expectJson
        (GotProposals T.MetaKey)
        (JD.list JD.string)
    }

getCachePolicy : String -> Cmd Msg
getCachePolicy urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "cache", "policies" ]
        []
    , expect = Http.expectJson
        (GotProposals T.CachePolicy)
        (JD.list JD.string)
    }

getBasketName : String -> Cmd Msg
getBasketName urlPrefix = Http.get
    { url = UB.crossOrigin
        urlPrefix
        [ "api", "series", "baskets" ]
        []
    , expect = Http.expectJson
        (GotProposals T.BasketName)
        (JD.list JD.string)
    }

getProposals : String -> Cmd Msg
getProposals urlPrefix = Cmd.batch
    [ getSeriesName urlPrefix
    , getSource urlPrefix
    , getMetaKey urlPrefix
    , getCachePolicy urlPrefix
    , getBasketName urlPrefix
    ]

init : Flags -> (Model, Cmd Msg)
init {urlPrefix, jsonSpec, formulaCode, returnTypeStr} =
    parseSpecValue {reduce = True} jsonSpec
        |> init_ formulaCode returnTypeStr
        |> withCmd (getProposals urlPrefix)

sendTreeEdited : Model -> Cmd Msg
sendTreeEdited m = sendCmd TreeEdited <| T.getCode m.editor.currentFormula

probeTreeEdited : EditAction -> Model -> Cmd Msg
probeTreeEdited editAction model = case editAction of
    ToggleExpand -> Cmd.none

    EditEntry (SelectorAction (OpenSelector _)) -> Cmd.none

    EditEntry (SelectorAction (SetKeywords _)) -> Cmd.none

    _ -> sendTreeEdited model

setFormula : Maybe T.FormulaCode -> Model -> Model
setFormula mFormulaCode ({editor} as model) = mFormulaCode
    |> Maybe.withDefault ""
    |> buildEditor editor.spec editor.returnTypeStr
    |> updateFormula
    |> \newEditor -> { model | editor = newEditor }

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
    (\(_, errs) -> PE.renderParserErrors errs
        |> String.lines
        |> List.map (\x -> H.span [ HA.class "error" ] [ H.text x ])
    )
    (always [])

renderReadOnly : Model -> List HMsg
renderReadOnly model =
    [ H.div
        [ HA.class "code_left" , editorHeight ]
        [ viewHeader ReadOnly
        , Ace.readOnly_ Nothing <| Either.unpack
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

initModel : Flags -> Model
initModel {jsonSpec, formulaCode} =
    parseSpecValue {reduce = True} jsonSpec |> \(errs, spec) ->
        { editor = Maybe.unwrap
            (buildEditor spec "Series" formulaDev)
            (buildEditor spec "Series")
            formulaCode
        , proposals = Assoc.empty
        , specErrors = errs
        }

main : Program Flags Model Msg
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
(resample
    (*
        0.0005
        (mul
            (sub
                (resample
                    (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.obs.10min")
                    "30min"
                    #:method "mean")
                (resample
                    (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.constant.fcst.h")
                    "30min"
                    #:method "interpolate"))
            (add
                (mul
                    (series "power.price.imbalance.fr.short.eurmwh.entsoe.obs.30min")
                    (<
                        (resample
                            (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.obs.10min")
                            "30min"
                            #:method "mean")
                        (resample
                            (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.constant.fcst.h")
                            "30min"
                            #:method "interpolate")))
                (mul
                    (series "power.price.imbalance.fr.long.eurmwh.entsoe.obs.30min")
                    (>
                        (resample
                            (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.obs.10min")
                            "30min"
                            #:method "mean")
                        (resample
                            (series "powerplant.prod.wind.fr.autremencourt.kw.kallista.constant.fcst.h")
                            "30min"
                            #:method "interpolate"))))))
    "H"
    #:method "sum")
    """

