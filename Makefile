MD_SOURCE = markdown/slides.md

.PHONY: all diagrams

all: slidy/index.html reveal/index.html diagrams

slidy/index.html: $(MD_SOURCE) slidy.template Makefile
	pandoc -t slidy --template slidy.template -o $@ $(MD_SOURCE)

reveal/index.html: $(MD_SOURCE) reveal.template Makefile
	pandoc --no-highlight -t revealjs --template reveal.template -o $@ $(MD_SOURCE)

diagrams:
	$(MAKE) -C diagrams

clean:
	-rm -f slidy/index.html
	-rm -f reveal/index.html
