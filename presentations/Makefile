lhs_handouts      := $(patsubst %.lhs,%.pdf,$(wildcard lhs/*.lhs))
lhs_presentations := $(patsubst %.lhs,%-presentation.pdf,$(wildcard lhs/*.lhs))

hs_srcs           := $(patsubst lhs/%.lhs,src/Presentation/%.hs,$(wildcard lhs/*.lhs))

# build handouts, published on coot.me
$(lhs_handouts): %.pdf: %.lhs
	lhs2TeX --poly -u compile -o $(patsubst %.lhs,%.tex,$<) $<
	pdflatex -halt-on-error -jobname=x -interaction=nonstopmode -output-directory=./lhs "\documentclass[handout,t,dvipsnames,hyperref={colorlinks,citecolor=NavyBlue,linkcolor=NavyBlue,anchorcolor=NavyBlue,urlcolor=NavyBlue},xcolor={x11names}]{beamer}" "\include{$(patsubst lhs/%.lhs,%,$<)}"
	mv lhs/x.pdf $@
	rm lhs/x.* 2>/dev/null || true

# build presentations
$(lhs_presentations): %-presentation.pdf: %.lhs
	lhs2TeX --poly -u compile -i $(patsubst %.lhs,%.fmt,$<) -o $(patsubst %.lhs,%.tex,$<) $<
	pdflatex -halt-on-error -jobname=x -interaction=nonstopmode -output-directory=./lhs "\documentclass[t,dvipsnames,hyperref={colorlinks,citecolor=NavyBlue,linkcolor=NavyBlue,anchorcolor=NavyBlue,urlcolor=NavyBlue},xcolor={x11names}]{beamer}" "\include{$(patsubst lhs/%.lhs,%,$<)}"
	mv lhs/x.pdf $@
	rm lhs/x.* 2>/dev/null || true

$(presentations): %.pdf: %.tex
	latexmk -shell-escape $<

$(hs_srcs): src/Presentation/%.hs: lhs/%.lhs
	mkdir -p src/Presentation
	lhs2TeX --newcode -s compile -o $@ $< 

code: $(hs_srcs)
.PHONY: code

all: $(presentations) $(lhs_presentations) $(lhs_handouts) $(hs_srcs)

.DEFAULT_GOAL := all

