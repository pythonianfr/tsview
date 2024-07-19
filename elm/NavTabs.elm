module NavTabs exposing
    ( header
     , tabcontents
     , strseries
     , Tabs(..)
     , viewdatespicker
     , DeleteEvents
     , MetaEvents
    )
import Array exposing (Array)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Selection as LS
import Metadata as M
import Util as U


type Tabs
    = Plot
    | Logs
    | UserMetadata
    | FormulaCache


type alias MetaEvents msg =
    { metaeditasked : msg
    , metaeditcancel : msg
    , editedvalue : String -> String -> msg
    , metaitemtodelete : String -> msg
    , newkey : String -> msg
    , newvalue : String -> msg
    , savemeta : msg
    , addmetaitem : msg
    }


type alias DeleteEvents msg =
    { confirmdeletion : msg
    , canceldeletion : msg
    , askdeletion : msg
    }


viewdatespicker : Int -> Array String ->  ( String -> msg ) -> H.Html msg
viewdatespicker date_index insertion_dates msg  =
    let
        currdate =
            case Array.get date_index insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date
    in H.div
        [ HA.class "row" ]
        [H.div
            [ HA.class "col" ]
            [ H.label [ HA.for "idate-picker" ] [ H.text "Revision date : " ]
            , H.span [ ] [ H.text " " ]
            , H.input [ HA.type_ "datetime-local"
                        , HA.id "idate-picker"
                        , HA.name "idate-picker"
                        , HA.value currdate
                        , HE.onInput msg
                        ] [ ]
            ]
        ]


strseries : M.StdMetadata -> Bool
strseries meta =
    case M.dget "value_type" meta of
        "object" -> True
        _ -> False


strtab : Tabs -> String
strtab tablelayout =
    case tablelayout of
        Plot -> "Plot"
        UserMetadata -> "Metadata"
        Logs -> "Logs"
        FormulaCache -> "Cache"


maketab : Bool -> ( Tabs -> msg ) -> Tabs -> H.Html msg
maketab active msg tab =
    let
        tabname = strtab tab
    in
    H.li
        [ HA.class "nav-item" ]
        [ H.a
              ([ HE.onClick (msg tab)
               , HA.class "nav-link"
               , HA.attribute "data-toggle" "tab"
               , HA.attribute "role" "tab"
               , HA.attribute "aria-selected" (if active then "true" else "false")
               , HA.id tabname
               ] ++ if active then [ HA.class "active" ] else []
              )
            [ H.div
                  []
                  [ H.text <| tabname ++ " " ]
            ]
        ]

header : ( Tabs -> msg ) -> LS.Selection Tabs -> H.Html msg
header msg tabs =
    H.ul [ HA.id "tabs"
         , HA.class "nav nav-tabs"
         , HA.attribute "role" "tablist"
         ]
        <| LS.toList
        <| LS.mapSelected
            { selected = maketab True msg
            , rest = maketab False msg
            }
            tabs


tabcontents : List (H.Html msg) -> H.Html msg
tabcontents items =
    H.span [ HA.style "margin" ".1rem" ]
        items
