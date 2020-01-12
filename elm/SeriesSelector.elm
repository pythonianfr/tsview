module SeriesSelector exposing (Model, new, null, updatesearch, updatefound, updateselected)


type alias Model =
    { search : String
    , found : List String
    , selected : List String
    }


new search searched selected =
    Model search searched selected


null =
    new "" [] []


updatesearch model newsearch =
    { model | search = newsearch }


updatefound model newfound =
    { model | found = newfound }


updateselected model newselection =
    { model | selected = newselection }
