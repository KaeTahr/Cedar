# Thesis
Thesis was compiled usitng pdflatex.

Wordcount written inside the thesis is calculated by the accompanying thesis.sh script file.

It expects a UNIX-like system, however rest of the file should compile without it.

Bibliography was written using biber.


## Steps to compile
A Makefile is included, but if manual compilation is required, these should be the basic steps:

1. Run pfdlatex on thesis.tex
2. Run biber on thesis
3. Re-run pfdlatex


