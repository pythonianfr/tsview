# TSVIEW


## Features


This is a [tshistory][tshistory] plugin which provides a `view` subcommand to the
`tsh` command.

[tshistory]: https://hg.sr.ht/~pythonian/tshistory

It works like this:

```shell
 $ tsh view postgres://babar:celeste@data_server:5432/
  * Running on http://192.168.56.1:5678/ (Press CTRL+C to quit)
 192.168.56.1 - - [16/Jun/2017 17:23:25] "GET /tsview HTTP/1.1" 200 -
 192.168.56.1 - - [16/Jun/2017 17:23:25] "GET /tsplot? HTTP/1.1" 200 -
```

At the same time, a browser tab opens, and we can select some series:

![tsh view](tsview.png)

For each series we can get it complete history (its versions at a
given date range):

![tsh history](tshistory.png)

This applies also to formulas if you use [tshistory_formula][tshistory_formula] !

[tshistory_formula]: https://hg.sr.ht/~pythonian/tshistory_formula

We also get a browser for the whole history _log_, by a
slight modification of the url, from `http://192.168.56.1:5678/tsview`
to `http://192.168.56.1:5678/tsviewlog`:

![tsh viewlog](tsviewlog.png)


## Formula editor tests

Make sure the relevant node modules are there:

```shell
 $ npm install yargs --exact
 $ npm install csv-parse --exact
```

Run the tests.

```shell
 $ make validation

elm make --output elm/FormulaParserValidation/tsformula_elm_parser.js --optimize \
	elm/FormulaParserValidation/Main.elm
Success!

    FormulaParserValidation.Main ───> elm/FormulaParserValidation/tsformula_elm_parser.js

...

--------------------------------------------------------------------------------
test_comma_on_str [OK]
ReturnTypes : Number, Series, Timestamp
  Operator selector : Series
    Operator : *
      Argument
        Input operator selector : Number
          ExpType : Number = 0.0572
      Argument
        ExpType : Union[Number, Series]
          Operator selector : Series
            Operator : series
              Argument
                ExpType : SearchString = "x"
              Options :
                OptionalArgument fill
                  ExpType : Union[String, Number]
                    ExpType : String = "a b"
                OptionalArgument limit
                  ExpType : Int
                OptionalArgument weight
                  Input operator selector : Number
                    ExpType : Number
--------------------------------------------------------------------------------
```

Going into elm/FormulaParserValidation:

```shell
 $ ./tsformula-elm-parser

tsformula-elm-parser <cmd> [args]

Commands:
  tsformula-elm-parser check            Check JSON specification
  tsformula-elm-parser inspect          Inspect JSON specification
  tsformula-elm-parser parse [catalog]  Parse formula CSV with name, code header

Options:
  --version   Show version number                                      [boolean]
  --spec, -s  JSON specification file                                 [required]
  --help      Show help                                                [boolean]

Missing required argument: spec
```

You can get the real spec file from `http://<instance>/spec`.
You can get the formulas lilst from `http://<instance>/downloadformulas`.
