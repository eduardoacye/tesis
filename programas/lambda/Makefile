NOWEAVE = noweave -delay -index -latex
NOTANGLE = notangle
CPIF = cpif
TEST = racket
LATEX = pdflatex

all: weave tangle test

weave: main.nw
	$(NOWEAVE) main.nw > main.tex
	$(LATEX) main.tex
	$(LATEX) main.tex

tangle: weave
	$(NOTANGLE) -Rlambda-lib.rkt main.nw | $(CPIF) lambda-lib.rkt
	$(NOTANGLE) -Rpruebas-lambda-lib.rkt main.nw | $(CPIF) pruebas-lambda-lib.rkt
	$(NOTANGLE) -Rgui-lib.rkt main.nw | $(CPIF) gui-lib.rkt
	$(NOTANGLE) -Rlambda-gui.rkt main.nw | $(CPIF) lambda-gui.rkt
	$(NOTANGLE) -Rlambda-init.rkt main.nw | $(CPIF) lambda-init.rkt

test: weave tangle
	$(TEST) pruebas-lambda-lib.rkt
