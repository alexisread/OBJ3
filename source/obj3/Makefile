#
# OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
# OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
# OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
# TRIM Copyright (c) 1994,2001 Lutz Hamel
# All Rights Reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#   o Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
#   o Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
#   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
#   International, nor the names of its contributors may be used to
#   endorse or promote products derived from this software without
#   specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
# INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

#
# $Id: Makefile,v 206.1 2003/09/26 13:01:30 kiniry Exp $
#

BASEDIR =	../..

SOURCEFILES =	$(wildcard *.lsp) \
		$(wildcard */*.lsp)

# GCL uses .o, CMU CL uses .x86f, CLISP uses .fas and .lib
OBJECTFILES =	$(SOURCEFILES:.lsp=.o) $(SOURCEFILES:.lsp=.x86f) \
		$(SOURCEFILES:.lsp=.fas) $(SOURCEFILES:.lsp=.lib)

ALLSOURCE = 	$(SOURCEFILES)

BINARY =	$(BASEDIR)/bin/obj3


.PHONY: all TAGS clean cleanup

all:	$(BINARY) TAGS

TAGS:	$(ALLSOURCE)
	etags $(SOURCEFILES)

clean:
	rm -f $(OBJECTFILES) TAGS *# *~ core tags
	find . -name "*.c" -exec rm -f {} \;
	find . -name "*.h" -exec rm -f {} \;
	find . -name "*.data" -exec rm -f {} \;

cleanup:
	rm -f *.aux *.log *# *~

$(BINARY):	$(SOURCEFILES)
	./obj3-make.sh
	make clean

