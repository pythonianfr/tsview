module ItemSelector exposing (view)

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (classList)
import Html.Styled.Events exposing (onMouseDown)
import Tachyons.Classes as T


view : (String -> msg) -> List String -> List String -> Html msg
view toMsg items selectedItems =
    let
        ulClass =
            classes [ T.list, T.pl0, T.ml0, T.w_100, T.ba, T.b__light_silver, T.br3 ]

        liAttrs item =
            let
                liClass =
                    classes [ T.ph3, T.pv2, T.bb, T.b__light_silver, T.dim ]

                liSelected =
                    let
                        isSelected =
                            List.member item selectedItems
                    in
                    classList <|
                        List.map
                            (\x -> ( x, isSelected ))
                            [ T.white, T.bg_blue ]
            in
            [ liClass, liSelected, onMouseDown <| toMsg item ]
    in
    ul [ ulClass ] <|
        List.map
            (\x -> li (liAttrs x) [ text x ])
            items
