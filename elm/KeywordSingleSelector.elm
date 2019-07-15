module KeywordSingleSelector exposing (Config, Context, view)

import Common
import Html.Styled exposing (Html)
import KeywordMultiSelector


type alias Config msg =
    KeywordMultiSelector.Config msg


type alias Context =
    { searchedItems : List String
    , selectedItem : Maybe String
    }


view : Config msg -> Context -> Html msg
view cfg ctx =
    KeywordMultiSelector.view cfg <|
        KeywordMultiSelector.Context ctx.searchedItems <|
            Common.maybe [] (\x -> [ x ]) ctx.selectedItem
