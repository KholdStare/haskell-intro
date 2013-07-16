all: slides.html reveal/index.html

slides.html: slides.md pre.html post.html Makefile
	(cat pre.html; pandoc -t slidy slides.md; cat post.html) > $@

reveal/index.html: slides.md reveal.template Makefile
	pandoc --no-highlight -t revealjs --template reveal.template -o reveal/index.html slides.md

clean:
	-rm -f slides.html
	-rm -f reveal/index.html
