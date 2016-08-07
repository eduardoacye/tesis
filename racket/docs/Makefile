all: weave tangle

weave: lambda.nw
	noweave -delay -index lambda.nw > lambda.tex
	pdflatex lambda.tex

tangle: codigo-estructuras codigo-lector

codigo-estructuras: lambda.nw
	notangle -Restructuras.rkt lambda.nw | cpif estructuras.rkt

codigo-lector: lambda.nw codigo-estructuras
	notangle -Rlector.rkt lambda.nw | cpif lector.rkt

tests: prueba-lector

prueba-lector: codigo-lector lector-pruebas.rkt
	racket lector-pruebas.rkt

clean:
	rm -rf lambda.tex lambda.aux lambda.log lambda.out

clean-all: clean
	rm -rf estructuras.rkt lector.rkt lambda.pdf
