module KeywordSelector exposing (countKeywords, select)

import Dict


countKeywords : String -> List String -> List ( Int, String )
countKeywords keywordsString series =
    let
        keywords =
            String.words keywordsString
                |> List.map (\k -> ( String.toLower k, String.length k ))

        matchCounter : Dict.Dict String Int
        matchCounter =
            let
                matchKeyword ( key, weight ) serieName b =
                    let
                        val =
                            negate (1000 + weight)
                    in
                    if String.contains key <| String.toLower serieName then
                        Dict.update
                            serieName
                            (\maybeCounter ->
                                Maybe.map ((+) val) maybeCounter
                                    |> Maybe.withDefault val
                                    |> Just
                            )
                            b

                    else
                        b
            in
            List.foldl
                (\kw b -> List.foldl (matchKeyword kw) b series)
                Dict.empty
                keywords
    in
    List.map2 Tuple.pair (Dict.values matchCounter) (Dict.keys matchCounter)
        |> List.sort


select : String -> List String -> List String
select k =
    List.map Tuple.second << countKeywords k
