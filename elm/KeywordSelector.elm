module KeywordSelector exposing (countKeywords, select)

import Dict


countKeywords : String -> List String -> List ( Int, String )
countKeywords keywordsString series =
    let
        keywords =
            String.words keywordsString |> List.map String.toLower

        matchCounter : Dict.Dict String Int
        matchCounter =
            let
                matchKeyword k serieName b =
                    if String.contains k <| String.toLower serieName then
                        Dict.update
                            serieName
                            (\maybeCounter ->
                                Maybe.map (\x -> (-) x 1) maybeCounter
                                    |> Maybe.withDefault 0
                                    |> Just
                            )
                            b

                    else
                        b
            in
            List.foldl
                (\k b -> List.foldl (matchKeyword k) b series)
                Dict.empty
                keywords
    in
    List.map2 Tuple.pair (Dict.values matchCounter) (Dict.keys matchCounter)
        |> List.sort


select : String -> List String -> List String
select k =
    List.map Tuple.second << countKeywords k
