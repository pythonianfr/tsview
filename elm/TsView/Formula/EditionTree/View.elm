module TsView.Formula.EditionTree.View exposing
    ( renderRowTree
    , strRowTree
    , viewEditionNode
    )

import Either
import Html as H
import Html.Attributes as A
import Html.Events as HE
import Json.Decode as Decode
import List.Nonempty as NE
import Maybe.Extra as Maybe
import Tree exposing (Tree)
import Tree.Zipper as Zipper
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.Spec.Type as S
import TsView.Formula.Utils exposing (icon, openAll)


type alias HMsg =
    H.Html ET.Msg


type alias Config =
    { spec : S.Spec
    , indent : Int
    }


type alias ZEN =
    Zipper.Zipper ET.EditionNode


type RowType
    = ReturnType
    | OptArgs
    | Arg
    | OptArg
    | SListItem
    | SListAdd


type Row
    = Row RowType ZEN (List ZEN)


type RowChunk
    = TreeRowT (Tree Row)
    | ZListT (List ZEN) (ET.Forest Row)


strRowType : RowType -> String
strRowType x =
    case x of
        ReturnType ->
            "ReturnType"

        OptArgs ->
            "OptArgs"

        Arg ->
            "Arg"

        OptArg ->
            "OptArg"

        SListItem ->
            "SListItem"

        SListAdd ->
            "SListAdd"


strEditionType : ET.EditionType -> String
strEditionType editionType =
    case editionType of
        ET.ReturnTypeT xs ->
            let
                args =
                    NE.map S.strBaseType xs |> NE.toList |> String.join ", "
            in
            "ReturnTypeT[" ++ args ++ "]"

        ET.OperatorT (ET.Operator name _ _) ->
            "OperatorT[" ++ name ++ "]"

        ET.OptArgsT _ ->
            "OptArgsT"

        ET.OptArgT _ ->
            "OptArgT"

        ET.ArgT _ ->
            "ArgT"

        ET.InputSelectorT x ->
            "InputSelectorT[" ++ S.strInputType x ++ "]"

        ET.SelectorT x ->
            "SelectorT[" ++ S.strBaseType x ++ "]"

        ET.ExpTypeT x ->
            "ExpTypeT[" ++ S.strExpType x ++ "]"


strRow : Row -> String
strRow (Row rowType _ xs) =
    let
        strZEN =
            Zipper.label >> .editionType >> strEditionType
    in
    (if List.isEmpty xs then
        []

     else
        [ ":", String.join ", " <| List.map strZEN xs ]
    )
        |> List.append
            [ "Row"
            , strRowType rowType
            ]
        |> String.join " "


strRowTree : Tree Row -> String
strRowTree tree =
    let
        indentString : String
        indentString =
            String.repeat 2 " "

        strIndentedRowTree : Row -> ( Int, List String ) -> ( Int, List String )
        strIndentedRowTree row ( indent, lines ) =
            let
                line =
                    String.repeat indent indentString ++ strRow row
            in
            ( doIndent row indent, line :: lines )
    in
    Tree.foldl strIndentedRowTree ( 0, [] ) tree
        |> Tuple.second
        |> List.reverse
        |> String.join "\n"


hasProperty : (ET.EditionType -> Bool) -> Row -> Bool
hasProperty test (Row _ _ xs) =
    List.map (Zipper.label >> .editionType >> test) xs |> List.any identity


hasOperator : Row -> Bool
hasOperator =
    hasProperty <|
        \editionType ->
            case editionType of
                ET.OperatorT _ ->
                    True

                _ ->
                    False


doIndent : Row -> Int -> Int
doIndent ((Row rowType _ _) as row) i =
    let
        inc =
            i + 1
    in
    case rowType of
        ReturnType ->
            inc

        OptArgs ->
            inc

        _ ->
            if hasOperator row then
                inc

            else
                i


partitionChunks : List RowChunk -> ( List ZEN, ET.Forest Row )
partitionChunks =
    let
        gather a b =
            case a of
                TreeRowT tree ->
                    Tuple.mapSecond (\xs -> tree :: xs) b

                ZListT zlist forest ->
                    Tuple.mapBoth
                        (\xs -> List.append zlist xs)
                        (\xs -> List.append forest xs)
                        b
    in
    List.foldr gather ( [], [] )


addListItem : Bool -> ZEN -> RowChunk -> RowChunk
addListItem isList zipper chunk =
    case ( isList, chunk ) of
        ( True, ZListT zlist forest ) ->
            TreeRowT <| Tree.tree (Row SListItem zipper zlist) forest

        _ ->
            chunk


renderRowChunk : ZEN -> RowChunk
renderRowChunk zipper =
    let
        n =
            Zipper.label zipper

        listChildren =
            if n.editionFlags.isOpen then
                openAll zipper

            else
                []

        isList =
            case n.editionType of
                ET.ExpTypeT (S.SList _) ->
                    True

                _ ->
                    False

        ( zlist, children ) =
            listChildren
                |> List.map (\z -> renderRowChunk z |> addListItem isList z)
                |> partitionChunks

        mkTree row xs =
            Tree.tree row xs |> TreeRowT

        mkRowTree rowType =
            mkTree (Row rowType zipper (zipper :: zlist)) children
    in
    case n.editionType of
        ET.ReturnTypeT _ ->
            mkRowTree ReturnType

        ET.OptArgsT _ ->
            mkRowTree OptArgs

        ET.ArgT _ ->
            mkRowTree Arg

        ET.OptArgT _ ->
            mkRowTree OptArg

        ET.ExpTypeT (S.SList _) ->
            let
                listAction : RowType -> ZEN -> Tree Row
                listAction rowType z =
                    Row rowType z [] |> Tree.singleton
            in
            ZListT
                (zipper :: zlist)
                (List.append children [ listAction SListAdd zipper ])

        ET.ExpTypeT (S.ExpBaseType (S.BaseInput _)) ->
            ZListT [ zipper ] []

        _ ->
            ZListT (zipper :: zlist) children


renderRowTree : ZEN -> Tree Row
renderRowTree zipper =
    renderRowChunk zipper
        |> (\x ->
                case x of
                    TreeRowT tree ->
                        tree

                    ZListT zlist forest ->
                        Tree.tree (Row Arg zipper zlist) forest
           )


renderExpand : ( ET.EditionNode, ZEN ) -> List HMsg
renderExpand ( node, zipper ) =
    let
        navTag =
            H.a
                [ A.class "col_expand center_item"
                , HE.onClick <| ET.ToggleNode zipper
                ]
                [ if node.editionFlags.isOpen then
                    H.text "-"

                  else
                    H.text "+"
                ]
                |> List.singleton
    in
    case node.editionType of
        ET.OperatorT _ ->
            navTag

        ET.OptArgsT _ ->
            navTag

        ET.ExpTypeT (S.SList _) ->
            navTag

        _ ->
            []


findLabel : ET.EditionType -> Maybe String
findLabel editionType =
    case editionType of
        ET.SelectorT x ->
            Just <| S.strBaseType x

        ET.InputSelectorT x ->
            Just <| S.strInputType x

        ET.OperatorT (ET.Operator x _ _) ->
            Just x

        ET.OptArgT (ET.OptArg x _ _) ->
            Just x

        ET.ExpTypeT x ->
            Just <| S.strExpType x

        _ ->
            Nothing


getChildLabel : ZEN -> Maybe String
getChildLabel parentZipper =
    Zipper.firstChild parentZipper
        |> Maybe.map (Zipper.label >> .editionType >> findLabel)
        |> Maybe.join


renderSelect : ZEN -> NE.Nonempty String -> HMsg
renderSelect zipper labels =
    let
        current =
            getChildLabel zipper |> Maybe.withDefault (NE.head labels)

        msgDecoder : Decode.Decoder ET.Msg
        msgDecoder =
            Decode.map (ET.ReadInput >> ET.EditNode zipper) HE.targetValue

        renderOption x =
            H.option
                [ A.selected (x == current), A.value x ]
                [ H.text x ]
    in
    H.select
        [ HE.on "change" msgDecoder ]
        (List.map renderOption <| NE.toList labels)


listButton : ZEN -> ET.EditAction -> String -> HMsg
listButton zipper action symbol =
    H.span []
        [ H.button
            [ HE.onClick <| ET.EditNode zipper action ]
            [ H.text symbol ]
        ]


input : S.InputType -> ZEN -> HMsg
input inputType zipper =
    let
        n =
            Zipper.label zipper

        inputTag ( s, errMess ) =
            H.span []
                [ H.input
                    [ A.value s
                    , HE.onInput (ET.ReadInput >> ET.EditNode zipper)
                    ]
                    []
                , H.text " "
                , H.text <| "[" ++ S.strInputType inputType ++ "]"
                , H.text " "
                , H.text <| Maybe.withDefault "" errMess
                ]
    in
    inputTag <| Tuple.mapSecond Either.leftToMaybe n.input


renderEditor : Config -> ( ET.EditionNode, ZEN ) -> List HMsg
renderEditor { spec } ( node, zipper ) =
    let
        getOpNames : S.BaseType -> Maybe (NE.Nonempty String)
        getOpNames baseType =
            S.getOperators baseType spec |> Maybe.map (NE.map .name)

        renderSelectAsList : Maybe (NE.Nonempty String) -> List HMsg
        renderSelectAsList =
            Maybe.map (renderSelect zipper) >> Maybe.toList
    in
    case node.editionType of
        ET.ReturnTypeT xs ->
            NE.map S.strBaseType xs |> Just |> renderSelectAsList

        ET.SelectorT x ->
            getOpNames x |> renderSelectAsList

        ET.InputSelectorT inputType ->
            getOpNames (S.BaseInput inputType)
                |> Maybe.map (NE.cons (S.strInputType inputType))
                |> renderSelectAsList

        ET.OptArgsT _ ->
            [ H.span [] [ H.text "Options" ] ]

        ET.ArgT (ET.Arg _ (S.SList _)) ->
            []

        ET.ArgT (ET.Arg name _) ->
            [ H.text " • ", H.span [] [ H.text name ] ]

        ET.OptArgT (ET.OptArg name _ _) ->
            [ H.text " • ", H.span [] [ H.text name ] ]

        ET.ExpTypeT (S.SList _) ->
            [ H.span [] [ H.text "List" ] ]

        ET.ExpTypeT (S.Union xs) ->
            NE.map S.strExpType xs |> Just |> renderSelectAsList

        ET.ExpTypeT (S.ExpBaseType (S.BaseInput x)) ->
            [ input x zipper ]

        _ ->
            []


renderEditionNode : Config -> Tree Row -> List HMsg
renderEditionNode ({ indent } as cfg) rowTree =
    let
        ((Row rowType zipper zlist) as row) =
            Tree.label rowTree

        withNode : ZEN -> ( ET.EditionNode, ZEN )
        withNode z =
            ( Zipper.label z, z )

        listActionNode : List HMsg
        listActionNode =
            case rowType of
                SListAdd ->
                    [ listButton zipper ET.ListAdd "+" ]

                SListItem ->
                    [ listButton zipper ET.ListRemove "-" ]

                _ ->
                    []

        expandNode : List HMsg -> List HMsg
        expandNode =
            List.head >> Maybe.toList

        editorNode : List HMsg -> List HMsg
        editorNode elts =
            H.div
                [ A.style "padding-left" (String.fromInt (2 * indent) ++ "em")
                , A.class "col_editor"
                ]
                (List.append elts listActionNode)
                |> List.singleton

        childCfg : Config
        childCfg =
            { cfg | indent = doIndent row indent }
    in
    List.concat
        [ List.concatMap (withNode >> renderExpand) zlist |> expandNode
        , List.concatMap (withNode >> renderEditor cfg) zlist |> editorNode
        , List.concatMap (renderEditionNode childCfg) <| Tree.children rowTree
        ]


viewEditionNode : S.Spec -> ET.EditionTree -> HMsg
viewEditionNode spec tree =
    let
        line cls txt =
            H.header [ A.class cls ] [ H.text txt ]

        header =
            [ line "col_expand header" "Expand"
            , line "col_editor header" "Editor"
            ]

        content =
            Zipper.fromTree tree
                |> renderRowTree
                |> renderEditionNode (Config spec 0)
    in
    H.section [ A.class "ui_editor" ] (header ++ content)
