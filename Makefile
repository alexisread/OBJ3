#
# OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
# OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
# OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
# TRIM Copyright (c) 1994,2001 Lutz Hamel
# All Rights Reserved
#

#
# $Id: Makefile,v 209.4 2003/09/29 15:10:24 kiniry Exp $
#

project =	OBJ3
version =	2.09
release =	$(project)-$(version)
archives =	$(release).tar.gz $(release).zip
binary =	bin/*
trim_examples_binaries =	FIB INT PEANO

release-dir =	release
release-temp =	release-temp

textfiles = 	BUGS.txt FAQ.txt LICENSE.txt README.txt \
		RELEASE_NOTES.txt TODO.txt index.txt \
		docs/index.txt docs/obj_bib.txt \
		source/README.txt

.PHONY: all distribution archives build_archives cleanup documentation \
	sources clean clean_archives textfiles version

all:	documentation sources $(textfiles)

distribution:	clean
	find . -name "*.gz" -exec rm -f {} \;
	make archives

archives:	sources documentation textfiles \
		cleanup clean_archives build_archives

build_archives: clean_archives
	mkdir -p $(release-dir) $(release-temp)
	rsync -av --exclude '**/CVS' --exclude 'CVS' \
		--exclude 'release*' \
		--exclude '**/.cvsignore' --exclude '.cvsignore' . $(release-temp)
	(cd $(release-temp); tar czf ../$(release-dir)/$(release).tar.gz .)
	(cd $(release-temp); zip -ryo ../$(release-dir)/$(release).zip .)

cleanup:
	find . -name "*~" -exec rm -f {} \;
	find . -name "*#" -exec rm -f {} \;
	find . -name "core" -exec rm -f {} \;
	find . -name ".auto" -exec rm -f {} \;
	(cd docs; make cleanup)
	(cd source/trim/src; make realclean)
	(cd source/trim/examples; rm -f $(trim_examples_binaries))

documentation:
	(cd docs; make all)

sources:
	mkdir -p bin
	(cd source/obj3; make all)
	(cd source/trim/src; make all)
	(cd source/trim/src; make install)

clean:	cleanup clean_archives
	rm -f $(textfiles) $(binary) *# *~ core
	(cd docs; make -k clean)
	(cd source/obj3; make -k clean)
	(cd source/trim/src; make -k realclean)
	(make -k clean_archives)

clean_archives:
	rm -rf $(release-dir) $(release-temp)

textfiles: $(textfiles)

source/README.txt: source/README.html
	lynx -dump source/README.html > source/README.txt

docs/index.txt: docs/index.html
	lynx -dump docs/index.html > docs/index.txt

docs/obj_bib.txt: docs/obj_bib.html
	lynx -dump docs/obj_bib.html > docs/obj_bib.txt

%.txt: %.html
	lynx -dump $< > `basename $< .html`.txt

version:
	find . -type f -and -not -name "*,v*" | xargs grep "\$$Id" | \
	gawk "{ print \$$4 }" | gawk "{ x = x + \$$0 ; print x}"