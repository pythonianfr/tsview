module SpecSuite exposing (testParsing)

import Test

import Editor.SpecParser exposing (parseSpecString)
import Editor.SpecRender exposing (renderSpec)

import TestUtil exposing (T)


render : (Bool, String) -> String
render (doReduce, spec) = parseSpecString {reduce = doReduce} spec
    |> Tuple.second
    |> renderSpec

testParsing : Test.Test
testParsing =
    TestUtil.buildTest render <|
    [ T "series"
        (False, """
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
        "Default[Union[Literal['ffill,bfill', 'bfill,ffill'], Number]=None]"
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
    )
        """
-----------------------------------------------------------------------------

    Series

series
    arguments:
        name: SeriesName
        date: Timestamp
    optional_arguments:
        fill: Union[Literal['ffill,bfill', 'bfill,ffill'], Number] Default=None
        prune: Int Default=None
        weight: Number Default=None
    return: Series
-----------------------------------------------------------------------------
"""
    , T "series, today, priority"
        (False, """
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
    )
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
    , T "mul operation, noreduce"
    (False, """
[
  [
    "*",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "num",
        "Number"
      ],
      [
        "num_or_series",
        "Union[Number, Series]"
      ]
    ]
  ]
]
"""
    )
    """
-----------------------------------------------------------------------------

    Number

*
    arguments:
        num: Number
        num_or_series: Union[Number, Series]
    return: Number
-----------------------------------------------------------------------------

    Series

*
    arguments:
        num: Number
        num_or_series: Union[Number, Series]
    return: Series
-----------------------------------------------------------------------------
"""
    , T "mul operation, reduce"
    (True, """
[
  [
    "*",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "num",
        "Number"
      ],
      [
        "num_or_series",
        "Union[Number, Series]"
      ]
    ]
  ]
]
"""
    )
    """
-----------------------------------------------------------------------------

    Number

*
    arguments:
        num: Number
        num_or_series: Number
    return: Number
-----------------------------------------------------------------------------

    Series

*
    arguments:
        num: Number
        num_or_series: Series
    return: Series
-----------------------------------------------------------------------------
"""
    , T "constant, reduce"
    (True, """
[
  [
    "constant",
    [
      [
        "return",
        "Series"
      ],
      [
        "fromdate",
        "Timestamp"
      ],
      [
        "todate",
        "Timestamp"
      ],
      [
        "freq",
        "Union[Freq, str]"
      ]
    ]
  ]
]
"""
    )
    """
-----------------------------------------------------------------------------

    Series

constant
    arguments:
        fromdate: Timestamp
        todate: Timestamp
        freq: Union[Freq, String]
    return: Series
-----------------------------------------------------------------------------
"""
    ]
