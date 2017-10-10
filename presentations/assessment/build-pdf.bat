Rscript -e "library(knitr);knit('./beamer-herring-assessment.rnw')" 1> knitr-output.log 2>&1

(@pdflatex -synctex=1 "beamer-herring-assessment.tex" && pdflatex "beamer-herring-assessment.tex" && bibtex "beamer-herring-assessment" && pdflatex "beamer-herring-assessment.tex" && pdflatex "beamer-herring-assessment.tex") 1> latex-output.log 2>&1
