all:
	cd programas/lambda && make
	pdflatex main.tex
	bibtex main
	pdflatex main.tex
	pdflatex main.tex

clean:
	rm -rf *.bbl *.aux *.log *.out *.toc
