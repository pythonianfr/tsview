module TsView.Formula.ViewEditor exposing (viewEditor)

import Either exposing (Either(..))
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import Json.Decode as Decode
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)
import Tachyons exposing (classes)
import Tachyons.Classes as T
import TsView.Formula.Spec as S exposing (Model, Msg(..))


type alias HMsg =
    Html Msg


renderNavigationNode : Zipper S.EditionNode -> Bool -> String -> HMsg
renderNavigationNode zipper isOpen label =
    H.a [ Events.onClick <| ToggleNode zipper ]
        [ H.span []
            [ if isOpen then
                H.text "- "

              else
                H.text "+ "
            ]
        , H.text label
        ]


renderSelect : Zipper S.EditionNode -> String -> List String -> HMsg
renderSelect zipper label labels =
    H.select
        [ Events.on "change" <| Decode.map (EditNode zipper) Events.targetValue ]
        (List.map
            (\x -> H.option [ A.selected (x == label), A.value x ] [ H.text x ])
            labels
        )


listButton : Zipper S.EditionNode -> S.ListAction -> String -> HMsg
listButton zipper action label =
    H.span [ A.style "margin" "10px" ]
        [ H.button
            [ Events.onClick <| S.EditList zipper action ]
            [ H.text label ]
        ]


input : Zipper S.EditionNode -> HMsg
input zipper =
    let
        n =
            Zipper.current zipper

        inputTag ( s, errMess ) =
            H.span []
                [ H.input [ A.value s, Events.onInput (EditNode zipper) ] []
                , H.text " "
                , H.text <| "[" ++ S.toString n.specType ++ "]"
                , H.text " "
                , H.text <| Maybe.withDefault "" errMess
                ]
    in
    inputTag <| Tuple.mapSecond Either.leftToMaybe n.input


viewEditor : Model -> HMsg
viewEditor model =
    let
        operators =
            S.listOperators model.spec |> List.map Tuple.first

        viewEditionNode : Zipper S.EditionNode -> HMsg
        viewEditionNode zipper =
            let
                n =
                    Zipper.current zipper

                flags =
                    n.editFlags

                viewChildren : List HMsg
                viewChildren =
                    if flags.isOpen then
                        Zipper.openAll zipper
                            |> List.map viewEditionNode

                    else
                        []

                listChildren : HMsg
                listChildren =
                    H.ul [] viewChildren

                viewChild : HMsg
                viewChild =
                    S.getFirstChild zipper
                        |> Maybe.map viewEditionNode
                        |> Maybe.withDefault (H.text "No child Error")
            in
            case n.specType of
                S.Operator name _ _ ->
                    H.li [] <|
                        [ renderNavigationNode zipper flags.isOpen "operator "
                        , renderSelect zipper name operators
                        , if flags.isRemovable then
                            listButton zipper S.ListRemove "Remove argument"

                          else
                            H.text ""
                        , listChildren
                        ]

                S.OptArgs _ ->
                    H.li []
                        [ renderNavigationNode zipper flags.isOpen "options "
                        , listChildren
                        ]

                S.Arg _ ->
                    let
                        child =
                            S.getFirstChild zipper
                                |> Maybe.withDefault zipper
                    in
                    case S.getSpecType child of
                        S.Series ->
                            viewChild

                        S.SList _ ->
                            H.li []
                                [ viewChild
                                , listButton child S.ListAdd "Add argument"
                                ]

                        _ ->
                            H.li [] [ viewChild ]

                S.OptArg name _ ->
                    H.li [] [ H.text name, H.text " ", viewChild ]

                S.Union xs ->
                    let
                        current =
                            S.toString <|
                                S.getUnionCurrentType (NE.head xs) zipper

                        items =
                            List.map S.toString <| NE.toList xs
                    in
                    H.span []
                        [ renderSelect zipper current items
                        , H.text " "
                        , viewChild
                        ]

                S.SList _ ->
                    listChildren

                S.Series ->
                    listChildren

                S.SearchString ->
                    input zipper

                S.Date ->
                    input zipper

                _ ->
                    input zipper
    in
    H.ul [ classes [ T.fl, T.w_100 ] ]
        [ viewEditionNode (Zipper.fromTree model.tree) ]
