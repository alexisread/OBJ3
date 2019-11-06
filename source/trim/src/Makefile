###################################################################
# TRIM
#
# Written by Lutz H. Hamel
# (c) copyright 1994.
#
# Available targets:
# all (default)
# clean
# realclean
# install
###################################################################
# choose your  favorite C++ compiler
CCP = g++
#CCP = CC 

# choose your  favorite C compiler
CC = gcc

# the archive program to use
AR = ar

###################################################################
# the directory where the software will be installed
# *NOTE* you need to set up the `config.h' file appropriately.

INSTALL_TARGET = ../../../bin

BIN_TARGET = $(INSTALL_TARGET)
LIB_TARGET = $(INSTALL_TARGET)
INC_TARGET = $(INSTALL_TARGET)

###################################################################
# conditional compilation macros:
#
# OBJ_OUTPUT	- set this if you want OBJ3 compatible output
#		  from TRIM
# USE_YYWRAP	- define this if your lexer needs a `yywrap' function.
# DEBUG		- defining this macro will prompt TRIM to be rather
#		  verbose and it will tell you everything you will
#		  ever want to know about its execution.
# HAVE_FORK	- define this if your system supports `fork'.
# HAVE_VPRINTF	- define this if your system supports `vprintf'.
# USG 		- define this if you are on system V
# _POSIX_SOURCE	- interactive Unix needs this defined.
# SUNC 		- define this for SUN C compiler...
# INSTR_INLINE	- define this if you want the TRIM instruction
#		  implementations to be C++ 'inline' code (yields
#		  faster code).
#
###################################################################
# set the options and macros you need for C++.
# CCPFLAGS = -O $(XCFLAGS) -DUSE_YYWRAP	-DOBJ_OUTPUT
CCPFLAGS = -O $(XCFLAGS) -DUSE_YYWRAP -DOBJ_OUTPUT -DINSTR_INLINE

# CCPFLAGS = -O $(XCFLAGS)             	-DOBJ_OUTPUT
# CCPFLAGS = -O $(XCFLAGS)             	-DOBJ_OUTPUT -DINSTR_INLINE

# CCPFLAGS = $(XCFLAGS) -DUSE_YYWRAP -g -DDEBUG -DOBJ_OUTPUT
# CCPFLAGS = $(XCFLAGS) -DUSE_YYWRAP -g -DDEBUG -DOBJ_OUTPUT -DINSTR_INLINE

# CCPFLAGS = $(XCFLAGS)              -g -DDEBUG -DOBJ_OUTPUT
# CCPFLAGS = $(XCFLAGS)              -g -DDEBUG -DOBJ_OUTPUT -DINSTR_LINE

# CCPFLAGS = $(XCFLAGS)              -g 	-DOBJ_OUTPUT
# CCPFLAGS = $(XCFLAGS)              -g 	-DOBJ_OUTPUT -DINSTR_LINE

# set the options and macros you need for C.
CCFLAGS = -I. $(XCFLAGS) -DHAVE_FORK -DHAVE_VPRINTF 

# set the options you need for the AR program.
ARFLAGS = rcv

###################################################################
# your favorite lex and yacc compatible lexer and parser generator.
LEX = flex 
YACC = bison -y -d

###################################################################
LIBS =

###################################################################
# build the programs

all: trimcc eqcomp trimasm trimopt libtrim.a

###################################################################
# shared sources

mem.o : mem.cxx
	$(CCP) mem.cxx -c $(CCPFLAGS)

fileio.o : fileio.c
	$(CCP) fileio.c -c $(CCPFLAGS)

basics.o : basics.cxx
	$(CCP) basics.cxx -c $(CCPFLAGS)

trimgram.c : trimgram.y
	$(YACC) trimgram.y
	mv y.tab.c trimgram.c
	mv y.tab.h trimgram.h

trimgram.o : trimgram.c
	$(CCP) trimgram.c -c $(CCPFLAGS)

trimscan.c : trimscan.l
	$(LEX) trimscan.l
	mv lex.yy.c trimscan.c

trimscan.o : trimscan.c
	$(CCP) trimscan.c -c $(CCPFLAGS)

asmlists.o : asmlists.cxx
	$(CCP) asmlists.cxx -c $(CCPFLAGS)

###################################################################
# libtrim.a (for dependencies see below)

TRIMOBJ = trimmain.o trim.o fileio.o basics.o mem.o


trimmain.o : trimmain.cxx
	$(CCP) trimmain.cxx -c $(CCPFLAGS)

trim.o : trim.cxx
	$(CCP) trim.cxx -c $(CCPFLAGS)

libtrim.a : $(TRIMOBJ) 
	$(AR) $(ARFLAGS) libtrim.a $(TRIMOBJ)
	ranlib libtrim.a

###################################################################
# trimasm (for dependencies see below)

TRIMASMOBJ = trimasm.o trimgram.o trimscan.o asmlists.o fileio.o basics.o \
	     mem.o

trimasm.o : trimasm.cxx
	$(CCP) trimasm.cxx -c $(CCPFLAGS)

trimasm : $(TRIMASMOBJ)
	$(CCP) $(CCPFLAGS) -o trimasm $(TRIMASMOBJ) $(LIBS)

###################################################################
# trimopt (for dependencies see below)

TRIMOPTOBJ = trimopt.o opt.o trimgram.o trimscan.o asmlists.o fileio.o \
		basics.o

opt.o : opt.cxx
	$(CCP) opt.cxx -c $(CCPFLAGS)

trimopt.o : trimopt.cxx
	$(CCP) trimopt.cxx -c $(CCPFLAGS)

trimopt : $(TRIMOPTOBJ)
	$(CCP) $(CCPFLAGS) -o trimopt $(TRIMOPTOBJ) $(LIBS)

###################################################################
# trimcc -- the driver program

obstack.o: obstack.c obstack.h
	$(CC) $(CCFLAGS) -c obstack.c

trimcc.o: config.h trimcc.h obstack.h trimcc.c 
	$(CC) $(CCFLAGS) -c trimcc.c

trimcc: obstack.o trimcc.o
	$(CC) $(CCFLAGS) -o trimcc obstack.o trimcc.o

###################################################################
# eqcomp (for dependencies see below)

EQCOMPOBJ = eqcomp.o eqgram.o eqscan.o fileio.o basics.o mem.o

eqgram.c : eqgram.y
	$(YACC) eqgram.y
	mv y.tab.c eqgram.c
	mv y.tab.h eqgram.h

eqgram.o : eqgram.c
	$(CCP) eqgram.c -c $(CCPFLAGS)

eqscan.c : eqscan.l
	$(LEX) eqscan.l
	mv lex.yy.c eqscan.c

eqscan.o : eqscan.c
	$(CCP) eqscan.c -c $(CCPFLAGS)

eqcomp.o : eqcomp.cxx
	$(CCP) eqcomp.cxx -c $(CCPFLAGS)

eqcomp : $(EQCOMPOBJ)
	$(CCP) $(CCPFLAGS) -o eqcomp $(EQCOMPOBJ) $(LIBS)

###################################################################
# clean

clean :
	-rm -f *.a
	-rm -f *.o
	-rm -f *~
	-rm -f *.stb

realclean : clean
	-rm -f eqscan.c
	-rm -f eqgram.c
	-rm -f eqgram.h
	-rm -f trimscan.c
	-rm -f trimgram.c
	-rm -f trimgram.h
	-rm -f libtrim.a
	-rm -f trimcc
	-rm -f trimasm
	-rm -f trimopt
	-rm -f eqcomp

###################################################################
# install the package

install:
	install -s trimcc $(BIN_TARGET)/trimcc
	install -s trimasm $(BIN_TARGET)/trimasm
	install -s trimopt $(BIN_TARGET)/trimopt
	install -s eqcomp $(BIN_TARGET)/eqcomp
	install libtrim.a $(LIB_TARGET)/libtrim.a
	ranlib $(LIB_TARGET)/libtrim.a
	install trim.hxx  $(INC_TARGET)/trim.hxx 
	install trim_instr.imp  $(INC_TARGET)/trim_instr.imp 
	install const.hxx  $(INC_TARGET)/const.hxx 
	install config.hxx  $(INC_TARGET)/config.hxx 
	install util.hxx  $(INC_TARGET)/util.hxx 
	install basics.hxx  $(INC_TARGET)/basics.hxx 

###################################################################
# dependencies

mem.o : config.hxx

trimmain.o : trim_instr.imp trim.hxx const.hxx config.hxx util.hxx basics.hxx 

trim.o : trim_instr.imp trim.hxx const.hxx config.hxx util.hxx basics.hxx 

asmlists.o : asmlists.hxx const.hxx config.hxx util.hxx fileio.h basics.hxx

fileio.o : fileio.h

basics.o : basics.hxx const.hxx config.hxx util.hxx 

trimasm.o : asm.hxx const.hxx config.hxx asmlists.hxx util.hxx fileio.h \
		basics.hxx

opt.o : opt.hxx const.hxx config.hxx asmlists.hxx util.hxx fileio.h \
		basics.hxx

trimopt.o : opt.hxx asm.hxx const.hxx config.hxx asmlists.hxx util.hxx \
		fileio.h basics.hxx 

trimscan.o :   config.hxx const.hxx asmlists.hxx util.hxx \
		trimgram.h asm.hxx basics.hxx

trimgram.o : config.hxx asmlists.hxx const.hxx util.hxx asm.hxx \
		basics.hxx

eqcomp.o : eqcomp.hxx const.hxx config.hxx util.hxx fileio.h basics.hxx

eqgram.o : config.hxx eqcomp.hxx const.hxx util.hxx basics.hxx

eqscan.o : eqcomp.hxx const.hxx util.hxx eqgram.h basics.hxx
