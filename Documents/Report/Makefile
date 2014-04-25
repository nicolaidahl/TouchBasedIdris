all:    report
report: Touch\ Based\ Idris.tex Bibliography.bib sections/*.tex
	pdflatex Touch\ Based\ Idris
	bibtex Touch\ Based\ Idris
	pdflatex Touch\ Based\ Idris
	pdflatex Touch\ Based\ Idris
clean:
	rm -f Touch\ Based\ Idris.aux
	rm -f Touch\ Based\ Idris.bbl
	rm -f Touch\ Based\ Idris.blg
	rm -f Touch\ Based\ Idris.dvi
	rm -f Touch\ Based\ Idris.log
	rm -f Touch\ Based\ Idris.toc
	rm -f Touch\ Based\ Idris.pdf