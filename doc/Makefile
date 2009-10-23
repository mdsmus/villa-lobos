NAME = spec
OTHER += $(DIA_EPS)

#USE_PDFLATEX = 1

XDVI_FLAGS = -expertmode 0 -useTeXpages -watchfile 5 -s 4

FIGSDIR = figs
DIA_PNG= $(patsubst %.dia,%.png,$(wildcard $(FIGSDIR)/*.dia))
DIA_EPS= $(patsubst %.dia,%.eps,$(wildcard $(FIGSDIR)/*.dia))
CLEAN_FILES+= $(DIA_EPS:.dia=.eps)
CLEAN_FILES+= $(DIA_PNG:.dia=.png)

vpath %.eps $(FIGSDIR)

%.eps: %.dia
	dia --export=$@ $<

%.eps: %.png
	convert $< $@

%.png: %.dia
	dia --export=$@ $<

include /usr/share/latex-mk/latex.gmk
