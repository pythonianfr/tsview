module HtmlExtra exposing (..)

import Maybe

import HtmlData as H exposing (Html)
import HtmlData.Events as HE
import HtmlData.Attributes as HA


attributeMaybe : (a -> HA.Attribute msg) -> Maybe a -> HA.Attribute msg
attributeMaybe f = Maybe.map f >> Maybe.withDefault HA.NoAttribute

nothing : Html msg
nothing = H.text ""

viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f ma =
    Maybe.map f ma |> Maybe.withDefault nothing
