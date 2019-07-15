module Common exposing
    ( classes
    , expectJsonMessage
    , expectStringResponse
    , maybe
    , resultEither
    )

import Dict
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as D


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


expectStringResponse :
    (Result String a -> msg)
    -> (String -> String)
    -> D.Decoder a
    -> Http.Expect msg
expectStringResponse toMsg readErr decoder =
    let
        badStatus code body =
            "BadStatus "
                ++ String.fromInt code
                ++ " : "
                ++ readErr body
    in
    Http.expectStringResponse toMsg <|
        \resp ->
            case resp of
                Http.BadUrl_ x ->
                    Err <| "BadUrl : " ++ x

                Http.Timeout_ ->
                    Err "Timeout"

                Http.NetworkError_ ->
                    Err "NetworkError"

                Http.BadStatus_ metadata body ->
                    Err <| badStatus metadata.statusCode body

                Http.GoodStatus_ _ body ->
                    D.decodeString decoder body
                        |> Result.mapError D.errorToString


expectJsonMessage : (Result String a -> msg) -> D.Decoder a -> Http.Expect msg
expectJsonMessage toMsg =
    let
        getMessage x =
            Dict.get "message" x
                |> Maybe.withDefault (Debug.toString x)

        readErr body =
            D.decodeString (D.dict D.string) body
                |> Result.map getMessage
                |> resultEither D.errorToString identity
    in
    expectStringResponse toMsg readErr
