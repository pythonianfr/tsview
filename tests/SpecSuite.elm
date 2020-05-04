module SpecSuite exposing (parsing)

import Either exposing (Either(..))
import Expect
import Json.Decode as D
import List.Nonempty as NE exposing (Nonempty)
import Test exposing (Test, test)
import TsView.Formula.Spec.Parser exposing (parseSpecString)
import TsView.Formula.Spec.Render exposing (renderSpec)
import TsView.Formula.Spec.Type as S


type alias T =
    { name : String
    , jsonSpec : String
    , result : String
    }


tests : List T
tests =
    [ T "series"
        """
[
  [
    "series",
    [
      [
        "name",
        "seriesname"
      ],
      [
        "date",
        "Timestamp"
      ],
      [
        "fill",
        "Default[Union[str, Number]=None]"
      ],
      [
        "prune",
        "Default[int=None]"
      ],
      [
        "weight",
        "Default[Number=None]"
      ],
      [
        "return",
        "Series"
      ]
    ]
  ]
]
"""
        """
-----
Series
    series
        arguments:
            SearchString
            Timestamp
        keyword_arguments:
            fill: Union[String, Number]
            prune: Int
            weight: Number
        return: Series
-----
"""
    , T "series, today, priority"
        """
[
  [
    "series",
    [
      [
        "name",
        "seriesname"
      ],
      [
        "fill",
        "Default[Union[str, Number]=None]"
      ],
      [
        "weight",
        "Default[Number=None]"
      ],
      [
        "return",
        "Series"
      ]
    ]
  ],
  [
    "today",
    [
      [
        "naive",
        "Default[bool=False]"
      ],
      [
        "tz",
        "Default[str=\\"UTC\\"]"
      ],
      [
        "return",
        "Timestamp"
      ]
    ]
  ],
  [
    "priority",
    [
      [
        "serieslist",
        "List[Series]"
      ],
      [
        "return",
        "Series"
      ]
    ]
  ],
  [
    "timedelta",
    [
      [
        "return",
        "Timestamp"
      ],
      [
        "date",
        "Timestamp"
      ],
      [
        "years",
        "Default[int=0]"
      ],
      [
        "months",
        "Default[int=0]"
      ]
    ]
  ]
]
"""
        """
-----
Series
    series
        arguments:
            SearchString
        keyword_arguments:
            fill: Union[String, Number]
            weight: Number
        return: Series
    priority
        arguments:
            List[Series]
        return: Series
-----
Timestamp
    today
        keyword_arguments:
            naive: Bool Default=False
            tz: String Default="UTC"
        return: Timestamp
    timedelta
        arguments:
            Timestamp
        keyword_arguments:
            years: Int Default=0
            months: Int Default=0
        return: Timestamp
-----
"""
    ]


parsing : Test
parsing =
    let
        rdr : String -> String
        rdr =
            parseSpecString
                >> Either.unpack
                    (Tuple.second >> NE.toList >> String.join "\n")
                    renderSpec

        checkStr x _ =
            let
                res =
                    rdr x.jsonSpec
            in
            Expect.equal res (String.trim x.result)
    in
    List.map (\x -> checkStr x |> test x.name) tests |> Test.concat
