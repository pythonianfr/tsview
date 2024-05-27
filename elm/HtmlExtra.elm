module HtmlExtra exposing (..)

import Maybe

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA


attributeMaybe : (a -> H.Attribute msg) -> Maybe a -> H.Attribute msg
attributeMaybe f ma =
    let
        noAttribute = HA.classList []
    in
    Maybe.map f ma |> Maybe.withDefault noAttribute

nothing : Html msg
nothing = H.text ""

viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f ma =
    Maybe.map f ma |> Maybe.withDefault nothing

viewIf : Bool -> Html msg -> Html msg
viewIf cond html = if cond then html else nothing
