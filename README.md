# Formula editor tests

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
