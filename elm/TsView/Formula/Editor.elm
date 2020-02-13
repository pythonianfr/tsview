module TsView.Formula.Editor exposing (main)

import Browser
import Catalog
import Cmd.Extra exposing (withNoCmd)
import Common
import Dict
import Either exposing (Either(..))
import Html as H exposing (Attribute, Html, input)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util exposing (toVirtualDom)
import Html.Styled as HS
import Http
import Json.Decode as Decode
import Json.Encode as E
import KeywordSelector
import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)
import List.Nonempty as NE exposing (Nonempty)
import SeriesSelector
import Tachyons exposing (classes)
import Tachyons.Classes as T
import Time
import TsView.Formula.Parser exposing (parseSpec)
import TsView.Formula.Renderer exposing (renderString)
import TsView.Formula.Spec as S exposing (Model, Msg(..))
import TsView.Formula.ViewEditor exposing (viewEditor)
import Url.Builder as UB


updateEditor : Zipper S.EditionNode -> String -> Model -> Zipper S.EditionNode
updateEditor zipper s model =
    let
        n =
            Zipper.current zipper
    in
    case n.specType of
        S.Operator _ _ _ ->
            let
                ops =
                    S.listOperators model.spec |> Dict.fromList

                specZipper =
                    Zipper.update
                        (\_ ->
                            Dict.get s ops
                                |> Maybe.withDefault (NE.head model.spec)
                                |> S.buildSpecTree model.spec
                        )
                        (Zipper.map .specType zipper)
            in
            Zipper.update (\_ -> S.buildEditionNode specZipper) zipper

        S.Union _ ->
            Zipper.open (always True) zipper
                |> Maybe.map
                    (Zipper.update (\_ -> S.fromString s |> model.buildEditionTree))
                |> Maybe.withDefault zipper

        _ ->
            Zipper.updateItem
                (\x -> { x | input = S.readInput x.input x.specType s })
                zipper


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        formula =
            model.formula

        newTreeModel zipper =
            let
                root =
                    Zipper.root zipper
            in
            ( { model
                | tree = Zipper.getTree root
                , formula = { formula | current = renderString root }
              }
            , Cmd.none
            )

        removeItem x xs =
            List.filter ((/=) x) xs

        toggleItem x xs =
            if List.member x xs then
                removeItem x xs

            else
                x :: xs

        keywordMatch xm xs =
            if String.length xm < 2 then
                []

            else
                KeywordSelector.select xm xs |> List.take 20
    in
    case msg of
        ToggleNode zipper ->
            newTreeModel <|
                Zipper.updateItem
                    (\n ->
                        let
                            flags =
                                n.editFlags
                        in
                        { n | editFlags = { flags | isOpen = not flags.isOpen } }
                    )
                    zipper

        EditList zipper S.ListAdd ->
            S.getSpecType zipper
                |> model.buildEditionTree
                |> Tree.descendants
                |> LL.foldl (\a b -> Zipper.insert a b) zipper
                |> newTreeModel

        EditList zipper S.ListRemove ->
            Zipper.attempt Zipper.delete zipper
                |> newTreeModel

        EditNode zipper s ->
            newTreeModel <| updateEditor zipper s model

        Render ->
            if formula.rendered /= formula.current then
                ( { model | formula = { formula | rendered = formula.current } }
                , Http.post
                    { url =
                        UB.crossOrigin
                            model.urlPrefix
                            [ "tsformula", "pygmentize" ]
                            []
                    , body = Http.stringBody "text/plain" formula.current
                    , expect = Common.expectJsonMessage CodeHighlight Decode.string
                    }
                )

            else
                ( model, Cmd.none )

        CodeHighlight (Ok x) ->
            let
                code =
                    Html.Parser.run x
                        |> Either.fromResult
                        |> Either.mapBoth
                            (\_ -> "Could not parse : " ++ x)
                            toVirtualDom
            in
            ( { model | formula = { formula | code = code } }, Cmd.none )

        CodeHighlight (Err x) ->
            ( { model | formula = { formula | code = Left x } }, Cmd.none )

        Save ->
            ( model
            , Http.post
                { url =
                    UB.crossOrigin
                        model.urlPrefix
                        [ "tsformula", "save" ]
                        []
                , body =
                    Http.jsonBody
                        (E.object
                            [ ( "code", E.string formula.rendered )
                            , ( "name", E.string formula.name )
                            ]
                        )
                , expect = Http.expectString SaveDone
                }
            )

        SaveDone (Ok _) ->
            ( { model
                | formula =
                    { formula
                        | saved = formula.code
                        , error = Nothing
                    }
              }
            , Cmd.none
            )

        SaveDone (Err _) ->
            ( { model | formula = { formula | error = Just "Wrong formula, could not be saved" } }
            , Cmd.none
            )

        EditedName newContent ->
            ( { model | formula = { formula | name = newContent } }, Cmd.none )

        ToggleItem x ->
            let
                newsearch =
                    SeriesSelector.updateselected
                        model.search
                        (toggleItem x model.search.selected)
            in
            { model | search = newsearch } |> withNoCmd

        SearchSeries x ->
            let
                newsearch =
                    SeriesSelector.updatesearch model.search x
            in
            { model | search = newsearch } |> withNoCmd

        KindChange kind checked ->
            let
                newsearch =
                    SeriesSelector.updatekinds
                        model.search
                        model.catalog
                        kind
                        checked
            in
            { model | search = newsearch } |> withNoCmd

        ToggleMenu ->
            { model | search = SeriesSelector.togglemenu model.search } |> withNoCmd

        SourceChange source checked ->
            let
                newsearch =
                    SeriesSelector.updatesources
                        model.search
                        model.catalog
                        source
                        checked
            in
            { model | search = newsearch } |> withNoCmd

        GotCatalog catmsg ->
            let
                newcat =
                    Catalog.update catmsg model.catalog

                newsearch =
                    SeriesSelector.fromcatalog model.search newcat
            in
            { model
                | catalog = newcat
                , search = newsearch
            }
                |> withNoCmd

        MakeSearch ->
            let
                newsearch =
                    SeriesSelector.updatefound
                        model.search
                        (keywordMatch
                            model.search.search
                            model.search.filteredseries
                        )
            in
            { model | search = newsearch } |> withNoCmd

        GotFormula (Ok x) ->
            let
                newModel =
                    { model | formula = { formula | current = x } }

                parse =
                    Either.unpack
                        (\s -> { newModel | errors = s :: model.errors })
                        (\tree -> { newModel | tree = tree })
                        (parseSpec model.spec x)
            in
            ( parse, Cmd.none )

        GotFormula (Err _) ->
            ( { model | errors = "Formula could not be loaded" :: model.errors }, Cmd.none )


formatDiv : Either String (List (Html Msg)) -> Html Msg
formatDiv formular =
    H.div [ classes [ T.ma3 ] ]
        (Either.unpack (H.text >> List.singleton) identity formular)


makeCard : String -> List (Html msg) -> Html msg
makeCard title contentChildren =
    H.article
        [ classes
            [ T.center
            , T.w_90
            , T.w_60_l
            , T.ba
            , T.mv4
            ]
        , A.hidden False
        ]
        [ H.h1
            [ classes
                [ T.f4
                , T.bg_moon_gray
                , T.navy
                , T.mv0
                , T.pv2
                , T.ph3
                ]
            ]
            [ H.text title ]
        , H.div
            [ classes
                [ T.pa3
                , T.bt
                ]
            ]
            contentChildren
        ]


selectorConfig : SeriesSelector.SelectorConfig Msg
selectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            HS.text
                "Type some keywords in input bar for selecting time series"
        , toggleMsg = ToggleItem
        }
    , actionSelector =
        { action =
            Nothing
        , defaultText = HS.text ""
        , toggleMsg = ToggleItem
        }
    , onInputMsg = SearchSeries
    , onMenuToggle = ToggleMenu
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ Common.classes [ T.mb4 ] ]
    }


view : Model -> Html Msg
view model =
    let
        formula =
            model.formula

        errMess errorList =
            let
                itemize =
                    H.text >> List.singleton >> H.li []
            in
            Maybe.map
                (\xs -> H.ul [ A.style "margin" "30px" ] (NE.map itemize xs |> NE.toList))
                (NE.fromList errorList)
                |> Maybe.withDefault
                    (H.text "")
    in
    H.main_ []
        [ errMess model.errors
        , makeCard "Load formula"
            [ SeriesSelector.view model.search model.catalog selectorConfig |> HS.toUnstyled ]
        , makeCard
            "Formula text editor"
            [ formatDiv formula.code
            ]
        , makeCard
            "Formula graphical editor"
            [ viewEditor model ]
        , makeCard
            "Save formula"
            [ H.button [ onClick Save ] [ H.text "Save" ]
            , H.input
                [ A.placeholder "Enter a name"
                , A.value formula.name
                , onInput EditedName
                ]
                []
            , errMess <| Maybe.withDefault [] <| Maybe.map List.singleton formula.error
            , formatDiv formula.saved
            ]
        ]


getFormulaCode : String -> Maybe String -> Cmd Msg
getFormulaCode urlPrefix formulaName =
    let
        getFormulaCode_ : String -> Cmd Msg
        getFormulaCode_ x =
            Http.get
                { url =
                    UB.crossOrigin urlPrefix
                        [ "tsformula", "code" ]
                        [ UB.string "name" x ]
                , expect = Http.expectString GotFormula
                }

        maybeCmd =
            Maybe.map getFormulaCode_ formulaName
    in
    Maybe.withDefault Cmd.none maybeCmd


main : Program ( String, S.JsonSpec, Maybe String ) Model Msg
main =
    let
        init ( urlPrefix, jsonSpec, formulaName ) =
            let
                ( specError, spec ) =
                    S.parseJsonSpec jsonSpec

                buildEditionTree =
                    S.buildEditionTree spec

                defaultOperator =
                    NE.head spec

                tree =
                    buildEditionTree defaultOperator
            in
            ( Model
                urlPrefix
                spec
                specError
                buildEditionTree
                tree
                (S.Formula
                    (renderString <| Zipper.fromTree tree)
                    ""
                    (Left "No rendering")
                    (Left "Nothing saved")
                    (Maybe.withDefault "" formulaName)
                    Nothing
                )
                SeriesSelector.null
                (Catalog.new Dict.empty)
            , Cmd.batch
                [ Cmd.map GotCatalog (Catalog.get urlPrefix 0)
                , getFormulaCode urlPrefix formulaName
                ]
            )

        sub model =
            Sub.batch <| List.map (always >> Time.every 1000) [ Render, MakeSearch ]
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = sub
        }
