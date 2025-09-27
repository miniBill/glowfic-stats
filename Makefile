.PHONY: all

all: \
	build/facecasts-circo.png \
	build/facecasts-fdp.png \
	build/facecasts-neato.png \
	build/facecasts-sfdp.png \
	build/facecasts-twopi.png \
	build/facecasts-dot.png

build/facecasts-%.png: build/facecasts.dot
	$* -Tpng < $< > $@

build/facecasts.dot: build/facecasts.csv src/FacecastsCsvToDot.elm
	bunx elm-pages run src/FacecastsCsvToDot.elm < $< > $@

build/facecasts.csv: build/facecasts.html src/FacecastsHtmlToCsv.elm
	bunx elm-pages run src/FacecastsHtmlToCsv.elm < $< > $@

build/facecasts.html:
	mkdir -p build
	curl https://glowfic.com/characters/facecasts > $@
