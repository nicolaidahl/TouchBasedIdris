all:    report.pdf
report.pdf: report.tex report.bib sections/*.tex
	pdflatex report
	bibtex report
	pdflatex report
	pdflatex report
clean:
	rm -f report.aux
	rm -f report.bbl
	rm -f report.blg
	rm -f report.dvi
	rm -f report.log
	rm -f report.toc
	rm -f report.pdf