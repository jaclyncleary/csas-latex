Rscript -e "library(knitr);knit('./resDoc.Rnw')" 1> knitrOutput.log 2>&1

(@pdflatex -synctex=1 "resDoc.tex" && bibtex "resDoc" && pdflatex "resDoc.tex" && pdflatex "resDoc.tex") 1> latexOutput.log 2>&1
