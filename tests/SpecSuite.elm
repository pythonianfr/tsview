module SpecSuite exposing (testParsing)

import Test

import Editor.SpecParser exposing (parseSpecString)
import Editor.SpecRender exposing (renderSpec)

import TestUtil exposing (T)


testParsing : Test.Test
testParsing =
    let
        render spec = parseSpecString {reduce = False} spec
            |> Tuple.second
            |> renderSpec

    in TestUtil.buildTest render <|
    [ T "series"
        """
[
  [
    "series",
    [
      [
        "return",
        "Series"
      ],
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
      ]
    ]
  ]
]
"""
        """
-----------------------------------------------------------------------------

    Series

series
    arguments:
        name: SeriesName
        date: Timestamp
    optional_arguments:
        fill: Union[String, Number] Default=None
        prune: Int Default=None
        weight: Number Default=None
    return: Series
-----------------------------------------------------------------------------
"""
    , T "series, today, priority"
        """
[
  [
    "series",
    [
      [
        "return",
        "Series"
      ],
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
      ]
    ]
  ],
  [
    "today",
    [
      [
        "return",
        "Timestamp"
      ],
      [
        "naive",
        "Default[bool=False]"
      ],
      [
        "tz",
        "Default[str=\\"UTC\\"]"
      ]
    ]
  ],
  [
    "priority",
    [
      [
        "return",
        "Series"
      ],
      [
        "serieslist",
        "Packed[Series]"
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
-----------------------------------------------------------------------------

    Series

series
    arguments:
        name: SeriesName
    optional_arguments:
        fill: Union[String, Number] Default=None
        weight: Number Default=None
    return: Series
priority
    arguments:
        serieslist: Packed[Series]
    return: Series
-----------------------------------------------------------------------------

    Timestamp

today
    optional_arguments:
        naive: Bool Default=False
        tz: String Default="UTC"
    return: Timestamp
timedelta
    arguments:
        date: Timestamp
    optional_arguments:
        years: Int Default=0
        months: Int Default=0
    return: Timestamp
-----------------------------------------------------------------------------
"""
    ]
