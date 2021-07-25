presentations := $(patsubst %.tex, %.pdf, $(wildcard *.tex))

$(presentations): %.pdf: %.tex
	latexmk -shell-escape $<

all: $(presentations)

