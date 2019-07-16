module KeywordSelector exposing (countKeywords, select)

import Common exposing (maybe)


countKeywords : String -> List String -> List ( Int, String )
countKeywords keywordsString series =
    let
        keywords =
            String.words keywordsString
                |> List.map (\k -> ( String.toLower k, String.length k ))

        matchCounter : String -> Maybe Int
        matchCounter serieName =
            let
                nameLen =
                    String.length serieName

                searchName =
                    String.toLower serieName

                matchKeyword : ( String, Int ) -> Maybe Int -> Maybe Int
                matchKeyword ( key, weight ) b =
                    let
                        prec =
                            toFloat weight / toFloat nameLen

                        val =
                            round (1.0e6 + prec * 1.0e5) |> negate
                    in
                    if String.contains key searchName then
                        Just <| maybe val ((+) val) b

                    else
                        b
            in
            List.foldl matchKeyword Nothing keywords
    in
    List.sort <|
        List.foldl
            (\x b -> maybe b (\i -> ( i, x ) :: b) (matchCounter x))
            []
            series


select : String -> List String -> List String
select k =
    List.map Tuple.second << countKeywords k
