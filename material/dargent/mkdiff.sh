#!/bin/bash

cd tree-submitted
latexpand paper.tex -o everything.tex
cd ../

latexpand paper.tex -o everything.tex


latexdiff tree-submitted/everything.tex everything.tex --append-mboxsafecmd="cc,cg" --add-to-config "PICTUREENV=lstlisting,VERBATIMENV=CCSXML" > diff.tex

sed -i "s/\\hspace{0pt}/\\hskip0pt/g" diff.tex

latexmk -pdf diff.tex
