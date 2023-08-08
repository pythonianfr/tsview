module Spec2Suite exposing (parsing)

import Editor.SpecParser exposing (parseSpecString)
import Editor.SpecRender exposing (renderSpec)
import Either exposing (Either(..))
import Expect
import Json.Decode as D
import List.Nonempty as NE exposing (Nonempty)
import Test exposing (Test, test)


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
series
    arguments:
        name: SearchString
        date: Timestamp
    optional_arguments:
        fill: Union[String, Number] Default=nil
        prune: Int Default=nil
        weight: Number Default=nil
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
series
    arguments:
        name: SearchString
    optional_arguments:
        fill: Union[String, Number] Default=nil
        weight: Number Default=nil
    return: Series
-----
today
    optional_arguments:
        naive: Bool Default=False
        tz: String Default="UTC"
    return: Timestamp
-----
priority
    arguments:
        serieslist: List[Series]
    return: Series
-----
timedelta
    arguments:
        date: Timestamp
    optional_arguments:
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
