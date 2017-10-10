# Clean up the directory after a LaTeX build. Windows version
del *.aux
del *.bbl
del *.blg
del *.dvi
del *.log
del *.lof
del *.lot
del *.nav
del *.ps
del *.snm
del *.toc
del *.txt
del *.out
del *-concordance.tex
del *.synctex.gz*
del beamer-herring-assessment.tex
del beamer-herring-assessment.pdf
rmdir out-csv /S /Q
rmdir /S /Q knitr-cache
