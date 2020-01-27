module Common exposing
    ( checkUrlPrefix
    , classes
    , decodeJsonMessage
    , expectJsonMessage
    , maybe
    , resultEither
    , taskSequenceEither
    )

import Dict
import Either exposing (Either)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (class)
import Http exposing (Response)
import Json.Decode as D
import Task exposing (Task)


classes : List String -> Attribute msg
classes xs =
    class (String.join " " xs)


maybe : b -> (a -> b) -> Maybe a -> b
maybe b f =
    Maybe.map f >> Maybe.withDefault b


resultEither : (a -> c) -> (b -> c) -> Result a b -> c
resultEither mapErr mapOk result =
    case result of
        Err a ->
            mapErr a

        Ok b ->
            mapOk b


type alias ToMsg a msg =
    Result String a -> msg


type alias ReadError =
    String -> String


decodeResponse : D.Decoder a -> Response String -> Result String a
decodeResponse decoder resp =
    let
        dictToStr =
            Dict.toList
                >> List.map (\( k, v ) -> "(" ++ k ++ ", " ++ v ++ ")")
                >> String.join " "

        getMessage x =
            Dict.get "message" x
                |> Maybe.withDefault (dictToStr x)

        readErr = D.decodeString (D.dict D.string)
            >> Result.map getMessage
            >> resultEither D.errorToString identity

        badStatus code body =
            readErr body
                ++ " ["
                ++ String.fromInt code
                ++ "]"
    in
        case resp of
            Http.BadUrl_ x -> Err <| "BadUrl : " ++ x

            Http.Timeout_ -> Err "Timeout"

            Http.NetworkError_ -> Err "NetworkError"

            Http.BadStatus_ metadata body ->
                Err <| badStatus metadata.statusCode body

            Http.GoodStatus_ _ body ->
                D.decodeString decoder body
                    |> Result.mapError D.errorToString


decodeJsonMessage : D.Decoder a -> Response String -> Result String a
decodeJsonMessage =
    decodeResponse


expectJsonResponse : ToMsg a msg -> D.Decoder a -> Http.Expect msg
expectJsonResponse toMsg decoder =
    Http.expectStringResponse toMsg (decodeResponse decoder)


expectJsonMessage : ToMsg a msg -> D.Decoder a -> Http.Expect msg
expectJsonMessage toMsg =
    expectJsonResponse toMsg


checkUrlPrefix : String -> String
checkUrlPrefix x =
    if x == "/" then
        ""
    else
        x


taskSequenceEither : List (Task x a) -> Task (List x) (List (Either x a))
taskSequenceEither tasks =
    Task.andThen
        (\xs ->
             let
                 lefts =
                     Either.lefts xs
             in
                 if
                     not (List.isEmpty xs) && (List.length lefts == List.length xs)
                 then
                     Task.fail lefts
                 else
                     Task.succeed xs
        )
    (List.foldr
         (\a b ->
              let
                  rightTask = Task.map Either.Right a
              in
                  Task.map2 (::) (Task.onError (Either.Left >> Task.succeed) rightTask) b
         )
         (Task.succeed [])
         tasks
    )
