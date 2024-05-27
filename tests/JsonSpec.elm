module JsonSpec exposing (..)

import Either

import Editor.Type exposing (Spec)
import Editor.UI.Type exposing (GSpec, buildGSpec)
import Editor.SpecParser exposing (parseSpecString)


spec : Spec
spec = parseSpecString jsonSpec |> Either.unpack Tuple.first identity

gSpec : GSpec
gSpec = buildGSpec spec

jsonSpec : String
jsonSpec =
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
        "Default[Union[str, int]=None]"
      ],
      [
        "weight",
        "Default[Number=None]"
      ]
    ]
  ],
  [
    "+",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "a",
        "Number"
      ],
      [
        "b",
        "Union[Number, Series]"
      ],
      [
        "flag",
        "Default[bool=False]"
      ]
    ]
  ],
  [
    "*",
    [
      [
        "return",
        "Union[Number, Series]"
      ],
      [
        "a",
        "Number"
      ],
      [
        "b",
        "Union[Series, Number]"
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
        "List[Union[Series, Number]]"
      ],
      [
        "k1",
        "Default[Union[str, Number]=None]"
      ],
      [
        "k2",
        "Default[Union[Number, Timestamp]=None]"
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
      ],
      [
        "weeks",
        "Default[int=0]"
      ],
      [
        "days",
        "Default[int=0]"
      ],
      [
        "hours",
        "Default[int=0]"
      ],
      [
        "minutes",
        "Default[int=0]"
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
        "Default[str=None]"
      ]
    ]
  ],
  [
   "**",
   [
     [
       "return",
       "Series"
     ],
     [
       "series",
       "Series"
     ],
     [
       "num",
       "Number"
     ]
   ]
  ]
]
"""
