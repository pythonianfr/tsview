all: delete rename plot

delete:
	elm make elm/Delete.elm --optimize --output tsview/tsview_static/delete_elm.js

rename:
	elm make elm/Rename.elm --optimize --output tsview/tsview_static/rename_elm.js

plot:
	elm make elm/Plot.elm --optimize --output tsview/tsview_static/plot_elm.js

clean: cleanstuff cleanbuild

cleanstuff:
	rm elm-stuff -rf

cleanbuild:
	rm tsview/tsview_static/delete_elm.js -f
	rm tsview/tsview_static/rename_elm.js -f
	rm tsview/tsview_static/plot_elm.js -f
