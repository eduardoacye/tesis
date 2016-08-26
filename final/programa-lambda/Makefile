NOWEAVE = noweave -delay -index -latex
NOTANGLE = notangle
CPIF = cpif
TEST = racket
LATEX = pdflatex

all: weave tangle test

weave: lambda.nw
	$(NOWEAVE) lambda.nw > lambda.tex
	$(LATEX) lambda.tex
	$(LATEX) lambda.tex

tangle: weave
	$(NOTANGLE) -Rlambda-lib.rkt lambda.nw | $(CPIF) lambda-lib.rkt
	$(NOTANGLE) -Rpruebas-lambda-lib.rkt lambda.nw | $(CPIF) pruebas-lambda-lib.rkt
	$(NOTANGLE) -Rgui-lib.rkt lambda.nw | $(CPIF) gui-lib.rkt
	$(NOTANGLE) -Rlambda-gui.rkt lambda.nw | $(CPIF) lambda-gui.rkt
	$(NOTANGLE) -Rlambda-init.rkt lambda.nw | $(CPIF) lambda-init.rkt

test: weave tangle
	$(TEST) pruebas-lambda-lib.rkt
