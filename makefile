all: presentation.pdf

clean:
	rm *.aux *.log *.nav *.out *.pdf *.snm *.toc

presentation.pdf: presentation.tex
	pdflatex -halt-on-error presentation.tex
