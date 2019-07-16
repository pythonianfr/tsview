all: delete rename

delete:
	elm make elm/Delete.elm --output tsview/tsview_static/delete_elm.js

rename:
	elm make elm/Rename.elm --output tsview/tsview_static/rename_elm.js

clean: cleanstuff cleanbuild

cleanstuff:
	rm elm-stuff -rf

cleanbuild:
	rm tsview/tsview_static/delete_elm.js -f
	rm tsview/tsview_static/rename_elm.js -f
