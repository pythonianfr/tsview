FLAGS ?= --optimize
NODE_PATH ?= /usr/lib/node_modules

all: delete rename info plot formula pygmentize

delete:
	elm make elm/Delete.elm $(FLAGS) --output tsview/tsview_static/delete_elm.js

rename:
	elm make elm/Rename.elm $(FLAGS) --output tsview/tsview_static/rename_elm.js

info:
	elm make elm/Info.elm $(FLAGS) --output tsview/tsview_static/info_elm.js

plot:
	elm make elm/Plot.elm $(FLAGS) --output tsview/tsview_static/plot_elm.js

formula:
	elm make elm/TsView/Formula/Editor.elm $(FLAGS) --output tsview/tsview_static/formula_elm.js

pygmentize:
	pygmentize -S default -f html -a .highlight > tsview/tsview_static/pygmentize.css

elm-test:
	elm-test

elm-validation:
	elm make --output elm/FormulaParserValidation/tsformula_elm_parser.js elm/FormulaParserValidation/Main.elm

# need : $ npm install -g csv-parse
validation: elm-validation
	cd elm/FormulaParserValidation && NODE_PATH=$(NODE_PATH):. ./tsformula-elm-parser -s spec.json parse formula.csv

clean: cleanstuff cleanbuild

cleanstuff:
	rm elm-stuff -rf

cleanbuild:
	rm tsview/tsview_static/delete_elm.js -f
	rm tsview/tsview_static/rename_elm.js -f
	rm tsview/tsview_static/plot_elm.js -f
	rm tsview/tsview_static/formula_elm.js -f
	rm elm/FormulaParserValidation/validation.js -f
