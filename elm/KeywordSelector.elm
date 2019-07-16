module KeywordSelector exposing (countKeywords, select)

import Common exposing (maybe)


type alias SplitFunc =
    String -> List String


matchKeywordWithWeight : String -> SplitFunc -> ( String, Int ) -> Maybe Float
matchKeywordWithWeight item splitItem ( key, weight ) =
    let
        parts =
            splitItem item

        partsLen =
            List.length parts

        addPart a b =
            if String.contains key a then
                let
                    prec =
                        toFloat weight / toFloat (String.length a)
                in
                Just <| Maybe.withDefault 0 b + prec

            else
                b
    in
    List.foldl addPart Nothing parts
        |> Maybe.map (\x -> x / toFloat partsLen)


countKeywords : String -> SplitFunc -> List String -> List ( Int, String )
countKeywords keywordsString splitItem items =
    let
        keywords =
            String.words keywordsString
                |> List.map (\k -> ( String.toLower k, String.length k ))

        matchCounter : String -> Maybe Int
        matchCounter item =
            let
                addKeywordPrec : ( String, Int ) -> Maybe Float -> Maybe Float
                addKeywordPrec kw b =
                    Common.maybe
                        b
                        ((+) (Maybe.withDefault 0 b) >> Just)
                        (matchKeywordWithWeight item splitItem kw)
            in
            List.foldl addKeywordPrec Nothing keywords
                |> Maybe.map (\x -> round (1.0e6 * x) |> negate)
    in
    List.sort <|
        List.foldl
            (\x b -> maybe b (\i -> ( i, x ) :: b) (matchCounter x))
            []
            items


select : String -> SplitFunc -> List String -> List String
select k f =
    List.map Tuple.second << countKeywords k f
