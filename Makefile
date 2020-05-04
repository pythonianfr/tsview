FLAGS ?=# --optimize
NODE_PATH ?= /usr/lib/node_modules

ACE_TAR ?= v1.4.11.tar.gz
ACE_URL ?= https://github.com/ajaxorg/ace-builds/archive/

FA_ZIP ?= fontawesome-free-5.13.0-web.zip
FA_URL ?= https://github.com/FortAwesome/Font-Awesome/releases/download/5.13.0/

all: delete rename info search plot formula pygmentize

delete:
	elm make elm/Delete.elm $(FLAGS) --output tsview/tsview_static/delete_elm.js

rename:
	elm make elm/Rename.elm $(FLAGS) --output tsview/tsview_static/rename_elm.js

info:
	elm make elm/Info.elm $(FLAGS) --output tsview/tsview_static/info_elm.js

search:
	elm make elm/Search.elm $(FLAGS) --output tsview/tsview_static/search_elm.js

plot:
	elm make elm/Plot.elm $(FLAGS) --output tsview/tsview_static/plot_elm.js

tsview/tsview_static/ace:
	(cd tsview/tsview_static && curl -L -O $(ACE_URL)/$(ACE_TAR) && tar xzf $(ACE_TAR))
	touch tsview/tsview_static/ace

tsview/tsview_static/fa:
	(cd tsview/tsview_static && curl -L -O $(FA_URL)/$(FA_ZIP) && unzip $(FA_ZIP))
	touch tsview/tsview_static/fa

formula: tsview/tsview_static/ace tsview/tsview_static/fa
	elm make elm/TsView/Formula/Main.elm $(FLAGS) --output tsview/tsview_static/formula_elm.js

pygmentize:
	pygmentize -S default -f html -a .highlight > tsview/tsview_static/pygmentize.css

elm-test:
	elm-test

elm-validation:
	elm make --output elm/FormulaParserValidation/tsformula_elm_parser.js $(FLAGS) \
		elm/FormulaParserValidation/Main.elm

# need : $ npm install -g csv-parse
validation: elm-validation
	cd elm/FormulaParserValidation && \
		NODE_PATH=$(NODE_PATH):. ./tsformula-elm-parser -s spec.json parse formula.csv

clean: cleanstuff cleanbuild

cleanstuff:
	rm elm-stuff -rf

cleanbuild:
	rm tsview/tsview_static/delete_elm.js -f
	rm tsview/tsview_static/rename_elm.js -f
	rm tsview/tsview_static/plot_elm.js -f
	rm tsview/tsview_static/info_elm.js -f
	rm tsview/tsview_static/search_elm.js -f
	rm tsview/tsview_static/formula_elm.js -f
	rm elm/FormulaParserValidation/tsformula_elm_parser.js -f
