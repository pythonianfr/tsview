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
                                |> S.buildSpecTree model.spec
                        )
                        (Zipper.map .specType zipper)
            in
            Zipper.update (\_ -> S.buildEditionNode specZipper) zipper

        S.Union _ ->
            Zipper.open (always True) zipper
                |> Maybe.map
                    (Zipper.update (\_ -> S.fromString s |> model.buildEditionTree))
                |> Maybe.withDefault zipper

        _ ->
            Zipper.updateItem
                (\x -> { x | input = S.readInput x.input x.specType s })
                zipper


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newTreeModel zipper =
            ( { model | tree = zipper |> Zipper.root |> Zipper.getTree }
            , Cmd.none
            )
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
                |> model.buildEditionTree
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
        errMess =
            let
                itemize =
                    H.text >> List.singleton >> H.li []
            in
            Maybe.map
                (\xs -> H.ul [ A.style "margin" "30px" ] (List.map itemize xs))
                model.specParsingError
                |> Maybe.withDefault
                    (H.text "")

        formula =
            renderString <|
                Zipper.fromTree model.tree

        formulaLines =
            String.split "\n" formula
    in
    H.article []
        [ errMess
        , H.div [ A.style "margin" "30px" ]
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


main : Program ( String, S.JsonSpec ) Model Msg
main =
    let
        init ( urlPrefix, jsonSpec ) =
            let
                ( specError, spec ) =
                    S.parseJsonSpec jsonSpec

                buildEditionTree =
                    S.buildEditionTree spec

                defaultOperator =
                    NE.head spec
            in
            ( Model
                spec
                specError
                buildEditionTree
                (buildEditionTree defaultOperator)
            , Cmd.none
            )
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
