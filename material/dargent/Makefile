SECS:=introduction cogent dargent verification applications timerdriver related-and-future conclusion opening-ex

FIGS:=imgs/*.tex
STYS:=util.sty

all: paper.pdf

paper.pdf: paper.tex $(addsuffix .tex, $(SECS)) $(FIGS) $(STYS) references.bib Makefile
	latexmk -pdf paper.tex

clean:
	rm *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out paper.pdf
