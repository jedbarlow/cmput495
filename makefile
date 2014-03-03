all: presentation.pdf

clean:
	rm *.aux *.log *.nav *.out *.pdf *.snm *.toc

monad_intro.pdf: monad_intro.tex
	pdflatex -halt-on-error monad_intro.tex

presentation.pdf: presentation.tex monad_intro.tex
	pdflatex -halt-on-error presentation.tex
