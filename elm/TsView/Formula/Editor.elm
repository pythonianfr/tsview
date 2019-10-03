module TsView.Formula.Editor exposing (main)

import Browser
import Dict
import Html as H exposing (Html)
import Html.Attributes as A
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)
import TsView.Formula.Renderer exposing (renderString)
import TsView.Formula.Spec as S exposing (Model, Msg(..))
import TsView.Formula.ViewEditor exposing (viewEditor)


init : Model
init =
    let
        spec =
            S.spec
    in
    Model spec (S.buildEditionTree <| NE.head spec)


updateEditor : Zipper S.EditionNode -> String -> Model -> Zipper S.EditionNode
updateEditor zipper s model =
    let
        n =
            Zipper.current zipper
    in
    case n.specType of
        S.Operator _ _ _ ->
            let
                ops =
                    S.listOperators model.spec |> Dict.fromList

                specZipper =
                    Zipper.update
                        (\_ ->
                            Dict.get s ops
                                |> Maybe.withDefault (NE.head model.spec)
                                |> S.buildSpecTree
                        )
                        (Zipper.map .specType zipper)
            in
            Zipper.update (\_ -> S.buildEditionNode specZipper) zipper

        S.Union _ ->
            Zipper.open (always True) zipper
                |> Maybe.map
                    (Zipper.update (\_ -> S.fromString s |> S.buildEditionTree))
                |> Maybe.withDefault zipper

        _ ->
            Zipper.updateItem
                (\x -> { x | input = S.readInput x.input x.specType s })
                zipper


update : Msg -> Model -> Model
update msg model =
    let
        newTreeModel zipper =
            { model | tree = zipper |> Zipper.root |> Zipper.getTree }
    in
    case msg of
        ToggleNode zipper ->
            newTreeModel <|
                Zipper.updateItem
                    (\n ->
                        let
                            flags =
                                n.editFlags
                        in
                        { n | editFlags = { flags | isOpen = not flags.isOpen } }
                    )
                    zipper

        EditList zipper S.ListAdd ->
            S.getSpecType zipper
                |> S.buildEditionTree
                |> Tree.descendants
                |> LL.foldl (\a b -> Zipper.insert a b) zipper
                |> newTreeModel

        EditList zipper S.ListRemove ->
            Zipper.attempt Zipper.delete zipper
                |> newTreeModel

        EditNode zipper s ->
            newTreeModel <| updateEditor zipper s model


view : Model -> Html Msg
view model =
    let
        formula =
            renderString <|
                Zipper.fromTree model.tree

        formulaLines =
            String.split "\n" formula
    in
    H.article []
        [ H.div [ A.style "margin" "30px" ]
            [ H.textarea
                [ A.rows <| List.length formulaLines
                , A.cols <|
                    List.foldl max 0 <|
                        List.map String.length formulaLines
                ]
                [ H.text formula ]
            ]
        , viewEditor model
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
