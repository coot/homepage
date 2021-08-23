lhs := $(patsubst %.lhs, %.pdf, $(wildcard lhs/*.lhs))

presentations := $(patsubst %.tex, %.pdf, $(wildcard *.tex))

$(lhs): %.pdf: %.lhs
	lhs2TeX -o $(patsubst %.lhs, %.tex, $<) $<
	pdflatex -output-directory ./lhs $(patsubst %.lhs, %.tex, $<)

$(presentations): %.pdf: %.tex
	latexmk -shell-escape $<

all: $(presentations) $(lhs)

