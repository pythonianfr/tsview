module Common exposing (classes)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (class)


classes : List String -> Attribute msg
classes xs =
    class (String.join " " xs)
