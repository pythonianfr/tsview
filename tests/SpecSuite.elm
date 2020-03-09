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
        "search_str"
      ],
      [
        "date",
        "Timestamp"
      ],
      [
        "fill",
        "Optional[Union[str, Number]]"
      ],
      [
        "prune",
        "Optional[int]"
      ],
      [
        "weight",
        "Optional[Number]"
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
        "search_str"
      ],
      [
        "fill",
        "Optional[Union[str, Number]]"
      ],
      [
        "weight",
        "Optional[Number]"
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
        "bool"
      ],
      [
        "tz",
        "Optional[str]"
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
        arguments:
            Bool
        keyword_arguments:
            tz: String
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
            Expect.equal (rdr x.jsonSpec) (String.trim x.result)
    in
    List.map (\x -> checkStr x |> test x.name) tests |> Test.concat
