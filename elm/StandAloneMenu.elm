module StandAloneMenu exposing (main)

import Browser
import Html as H
import Html.Attributes as A
import Html exposing
    ( Html
    , Attribute
    )
import Menu as Men
import Icons exposing (getIcons)

type alias Model =
    { baseUrl: String
    , menu : Men.Model
    }

type Msg =
    Menu Men.Msg

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Menu menumsg ->
            ( { model | menu = Men.updateModel menumsg model.menu }
            , Men.buildCmd menumsg model.menu)


initModel: String -> String -> Model
initModel baseUrl selected =
    { baseUrl = baseUrl
    , menu = Men.initmenu selected
    }

init: (String, String) -> ( Model, ( Cmd Msg ))
init (baseUrl, selected) =
    ( initModel baseUrl selected
    , Cmd.batch
        [ Men.getMenu baseUrl ( \ returnHttp ->  Menu (Men.GotMenu returnHttp ) )
        , getIcons baseUrl ( \ returnHttp ->  Menu (Men.GotIcons returnHttp ) )
       ]
   )


view: Model -> Html Msg
view model =
    H.div
        [ A.class ( if model.menu.menuModeText
                        then "grid-container-text"
                        else "grid-container-icon") ]
        [ H.img [ A.class "img-logo", A.src ( model.baseUrl ++ "tsview_static/logo.jpg" ) ] []
        , Men.viewMenu model.menu Menu ]

sub model =  Men.loadMenuData (\ str -> Menu (Men.LoadMenuData str))

main = Browser.element
        { init = init
        , view = view
        , update = update
       , subscriptions = sub
        }