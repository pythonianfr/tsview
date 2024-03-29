FLAGS ?= --optimize
NODE_PATH ?= /usr/lib/node_modules

ACE_TAR ?= v1.4.11.tar.gz
ACE_URL ?= https://github.com/ajaxorg/ace-builds/archive

all: delete info groupinfo search plot cache formula pygmentize

delete:
	elm make elm/Delete.elm $(FLAGS) --output tsview/tsview_static/delete_elm.js

info:
	elm make elm/Tsinfo.elm $(FLAGS) --output tsview/tsview_static/tsinfo_elm.js

groupinfo:
	elm make elm/Groupinfo.elm $(FLAGS) --output tsview/tsview_static/groupinfo_elm.js

search:
	elm make elm/Search.elm $(FLAGS) --output tsview/tsview_static/search_elm.js

plot:
	elm make elm/Plot.elm $(FLAGS) --output tsview/tsview_static/plot_elm.js

cache:
	elm make elm/Cache.elm $(FLAGS) --output tsview/tsview_static/cache_elm.js


# now vendored in tsview_static
# tsview/tsview_static/ace:
# 	(cd tsview/tsview_static && curl -L -O $(ACE_URL)/$(ACE_TAR) && tar xzf $(ACE_TAR))
# 	touch tsview/tsview_static/ace

formula:
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
	rm tsview/tsview_static/plot_elm.js -f
	rm tsview/tsview_static/info_elm.js -f
	rm tsview/tsview_static/search_elm.js -f
	rm tsview/tsview_static/formula_elm.js -f
	rm tsview/tsview_static/cache.js -f
	rm elm/FormulaParserValidation/tsformula_elm_parser.js -f
