module ReaderExtra exposing (..)

import Reader exposing (Reader)


run : env -> Reader env a -> a
run env ra = Reader.run ra env

ask : (env -> b) -> Reader env b
ask f = Reader.ask |> Reader.map f

asks : (env -> v) -> (v -> b) -> Reader env b
asks accessor f = Reader.asks accessor |> Reader.map f

askM : (env -> Reader env b) -> Reader env b
askM f = Reader.ask |> Reader.andThen f

asksM : (env -> v) -> (v -> Reader env b) -> Reader env b
asksM accessor f = Reader.asks accessor |> Reader.andThen f

traverse : (a -> Reader env b) -> List a -> Reader env (List b)
traverse f =
    List.foldr (\a b -> Reader.map2 (::) (f a) b) (Reader.reader [])

sequence : List (Reader env a) -> Reader env (List a)
sequence = traverse identity
