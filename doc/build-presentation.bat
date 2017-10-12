Rscript -e "library(knitr);knit('./Presentation.rnw')" 1> knitr-pres-output.log 2>&1

(@pdflatex -synctex=1 "Presentation.tex" && pdflatex "Presentation.tex" && bibtex "Presentation" && pdflatex "Presentation.tex" && pdflatex "Presentation.tex") 1> latex-pres-output.log 2>&1
