TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
BIB = biber

.PHONY: all view
all : thesis.pdf

view :
	open thesis.pdf 

thesis.pdf : thesis.tex thesis.bbl thesis.blg     
	$(TEX) thesis.tex

thesis.bbl thesis.blg : bibliography/biblio.bib thesis.aux
	$(BIB) thesis

thesis.aux : thesis.tex
	$(TEX) thesis.tex 

biliography/biblio.bib : biblio.tex
	$(TEX) biblio.tex 


