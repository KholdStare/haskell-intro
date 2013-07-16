MD_SOURCE = markdown/slides.md

all: slidy/index.html reveal/index.html

slidy/index.html: $(MD_SOURCE) pre.html post.html Makefile
	(cat pre.html; pandoc -t slidy $(MD_SOURCE); cat post.html) > $@

reveal/index.html: $(MD_SOURCE) reveal.template Makefile
	pandoc --no-highlight -t revealjs --template reveal.template -o $@ $(MD_SOURCE)

clean:
	-rm -f slidy/index.html
	-rm -f reveal/index.html
