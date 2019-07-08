module ItemSelector exposing (Config, Context, view)

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (classList)
import Html.Styled.Events exposing (onMouseDown)
import Tachyons.Classes as T


type alias Config msg =
    { action :
        Maybe
            { attrs : List (Attribute msg)
            , html : Html msg
            , clickMsg : msg
            }
    , defaultText : Html msg
    , toggleMsg : String -> msg
    }


type alias Context =
    { items : List String
    , selectedItems : List String
    }


view : Config msg -> Context -> Html msg
view cfg ctx =
    let
        enabled =
            List.length ctx.items > 0

        ulClass =
            classes [ T.list, T.pl0, T.ml0, T.w_100, T.ba, T.b__light_silver, T.br3 ]

        liAttrs item =
            let
                liClass =
                    classes [ T.ph3, T.pv2, T.bb, T.b__light_silver, T.dim ]

                liSelected =
                    let
                        isSelected =
                            List.member item ctx.selectedItems
                    in
                    classList <|
                        List.map
                            (\x -> ( x, isSelected ))
                            [ T.white, T.bg_blue ]
            in
            [ liClass, liSelected, onMouseDown <| cfg.toggleMsg item ]

        lst =
            ul [ ulClass ] <|
                List.map
                    (\x -> li (liAttrs x) [ text x ])
                    ctx.items

        lstWithAction act =
            let
                aClass =
                    classes
                        [ T.w_100, T.link, T.dim, T.ph3, T.pv2, T.ma2, T.dib, T.tc ]

                attrs =
                    [ aClass, onMouseDown act.clickMsg ] ++ act.attrs
            in
            div [ classes [ T.dt, T.dt__fixed ] ] [ a attrs [ act.html ], lst ]
    in
    if enabled then
        Maybe.map lstWithAction cfg.action |> Maybe.withDefault lst

    else
        div [ classes [ T.pl0, T.ml0 ] ] [ cfg.defaultText ]
