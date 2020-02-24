module Common exposing
    ( classes
    , decodeJsonMessage
    , expectJsonMessage
    , maybe
    , taskSequenceEither
    )

import Dict
import Either exposing (Either)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (class)
import Http exposing (Response)
import Json.Decode as D
import Task exposing (Task)



-- css helper


classes : List String -> Attribute msg
classes xs =
    class (String.join " " xs)



-- core helpers


maybe : b -> (a -> b) -> Maybe a -> b
maybe b f =
    Maybe.map f >> Maybe.withDefault b



-- messaging/decoding helpers


type alias ToMsg a msg =
    Result String a -> msg


type alias ReadError =
    String -> String


decodeResponse : D.Decoder a -> Response String -> Result String a
decodeResponse decoder resp =
    case resp of
        Http.BadUrl_ x ->
            Err <| "BadUrl : " ++ x

        Http.Timeout_ ->
            Err "Timeout"

        Http.NetworkError_ ->
            Err "NetworkError"

        Http.BadStatus_ _ body ->
            D.decodeString (D.field "message" D.string) body
                |> Either.fromResult
                |> Either.mapLeft D.errorToString
                |> Either.unpack Err Err

        Http.GoodStatus_ _ body ->
            D.decodeString decoder body
                |> Result.mapError D.errorToString


decodeJsonMessage : D.Decoder a -> Response String -> Result String a
decodeJsonMessage =
    decodeResponse


expectJsonMessage : ToMsg a msg -> D.Decoder a -> Http.Expect msg
expectJsonMessage toMsg decoder =
    Http.expectStringResponse toMsg (decodeResponse decoder)



-- task helper


taskSequenceEither : List (Task x a) -> Task (List x) (List (Either x a))
taskSequenceEither tasks =
    Task.andThen
        (\xs ->
            let
                lefts =
                    Either.lefts xs
            in
            if not (List.isEmpty xs) && (List.length lefts == List.length xs) then
                Task.fail lefts

            else
                Task.succeed xs
        )
        (List.foldr
            (\a b ->
                let
                    rightTask =
                        Task.map Either.Right a
                in
                Task.map2 (::) (Task.onError (Either.Left >> Task.succeed) rightTask) b
            )
            (Task.succeed [])
            tasks
        )
