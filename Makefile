MD_SOURCE = markdown/slides.md

all: slidy/index.html reveal/index.html

slidy/index.html: $(MD_SOURCE) slidy.template Makefile
	pandoc -t slidy --template slidy.template -o $@ $(MD_SOURCE)

reveal/index.html: $(MD_SOURCE) reveal.template Makefile
	pandoc --no-highlight -t revealjs --template reveal.template -o $@ $(MD_SOURCE)

clean:
	-rm -f slidy/index.html
	-rm -f reveal/index.html
