module KeywordSingleSelector exposing (Config, Context, view)

import Common
import Html.Styled exposing (Html)
import KeywordMultiSelector


type alias Config msg =
    KeywordMultiSelector.Config msg


type alias Context msg =
    { searchString : String
    , searchedItems : List String
    , selectedItem : Maybe String
    , errorMessage : Maybe (Html msg)
    }


view : Config msg -> Context msg -> Html msg
view cfg ctx =
    KeywordMultiSelector.view cfg <|
        KeywordMultiSelector.Context
            ctx.searchString
            ctx.searchedItems
            (Common.maybe [] List.singleton ctx.selectedItem)
            ctx.errorMessage
