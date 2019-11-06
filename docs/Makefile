#
# OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
# OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
# OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
# TRIM Copyright (c) 1994,2001 Lutz Hamel
# All Rights Reserved
#

#
# $Id: Makefile,v 206.1 2003/09/23 13:27:16 kiniry Exp $
#

all:	NewFeatures Built-Ins cleanup

NewFeatures: NewFeatures.dvi NewFeatures.ps NewFeatures.pdf

Built-Ins: Built-Ins.dvi Built-Ins.ps Built-Ins.pdf

clean:
	rm -rf .auto *.gz *.dvi *.ps *.pdf *.aux \
		*.log *.toc *.bbl *.blg *# *~

cleanup:
	rm -f *.bbl *.blg *.aux *.log *.toc *# *~

%.pdf: %.tex
	pdflatex $< $@

%.ps:	%.dvi
	dvips -Pcmz -Pamz -f < $< > $@

%.dvi:	%.tex
	latex $<; bibtex `basename $< .tex`; latex $<; latex $<; 

%.pdf.gz:	%.pdf
	gzip -c $< > $@

%.ps.gz:	%.ps
	gzip -c $< > $@

