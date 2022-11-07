module Common exposing
    ( expectJsonMessage
    , maybe
    )

import Dict
import Either exposing (Either)
import Html exposing (Attribute)
import Html.Attributes exposing (class)
import Http exposing (Response)
import Json.Decode as D
import Task exposing (Task)


-- core helpers


maybe : b -> (a -> b) -> Maybe a -> b
maybe b f =
    Maybe.map f >> Maybe.withDefault b



-- messaging/decoding helpers


type alias ToMsg a msg =
    Result String a -> msg


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


expectJsonMessage : ToMsg a msg -> D.Decoder a -> Http.Expect msg
expectJsonMessage toMsg decoder =
    Http.expectStringResponse toMsg (decodeResponse decoder)
