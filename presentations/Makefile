lhs_handouts      := $(patsubst %.lhs,%.pdf,$(wildcard lhs/*.lhs))
lhs_presentations := $(patsubst %.lhs,%-presentation.pdf,$(wildcard lhs/*.lhs))

presentations := $(patsubst %.tex,%.pdf,$(wildcard *.tex))

# build handouts, published on coot.me
$(lhs_handouts): %.pdf: %.lhs
	lhs2TeX --poly -o $(patsubst %.lhs,%.tex,$<) $<
	pdflatex -halt-on-error -jobname=x -interaction=nonstopmode -output-directory=./lhs "\documentclass[handout,t,dvipsnames,hyperref={colorlinks,citecolor=NavyBlue,linkcolor=NavyBlue,anchorcolor=NavyBlue,urlcolor=NavyBlue},xcolor={x11names}]{beamer}" "\include{$(patsubst lhs/%.lhs,%,$<)}"
	mv lhs/x.pdf $@
	rm lhs/x.* 2>/dev/null || true

# build presentations
$(lhs_presentations): %-presentation.pdf: %.lhs
	lhs2TeX --poly -o $(patsubst %.lhs,%.tex,$<) $<
	pdflatex -halt-on-error -jobname=x -interaction=nonstopmode -output-directory=./lhs "\documentclass[t,dvipsnames,hyperref={colorlinks,citecolor=NavyBlue,linkcolor=NavyBlue,anchorcolor=NavyBlue,urlcolor=NavyBlue},xcolor={x11names}]{beamer}" "\include{$(patsubst lhs/%.lhs,%,$<)}"
	mv lhs/x.pdf $@
	rm lhs/x.* 2>/dev/null || true

$(presentations): %.pdf: %.tex
	latexmk -shell-escape $<

all: $(presentations) $(lhs_presentations) $(lhs_handouts)

.DEFAULT_GOAL := all

