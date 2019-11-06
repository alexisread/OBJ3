#define INSTALL_TARGET "../../../bin"

/************************************************************************/
#define VERSION "TRIM Compiler Rev 1.3 -- (c) Copyright 1994-96, Lutz H. Hamel"

/* Compiler driver program that can handle many languages.
   Copyright (C) 1987,1989 Free Software Foundation, Inc. */

/* Adapted for TRIM by Lutz Hamel, 1994. */

/* The Language Definition...

Specs are strings containing lines, each of which (if not blank)
is made up of a program name, and arguments separated by spaces.
The program name must be exact and start from root, since no path
is searched and it is unreliable to depend on the current working directory.
Redirection of input or output is not supported; the subprograms must
accept filenames saying what files to read and write.

In addition, the specs can contain %-sequences to substitute variable text
or for conditional text.  Here is a table of all defined %-sequences.
Note that spaces are not generated automatically around the results of
expanding these sequences; therefore, you can concatenate them together
or with constant text in a single argument.

 %%	substitute one % into the program name or argument.
 %i     substitute the name of the input file being processed.
 %b     substitute the basename of the input file being processed.
	This is the substring up to (and not including) the last period.
 %g     substitute the temporary-file-name-base.  This is a string chosen
	once per compilation.  Different temporary file names are made by
	concatenation of constant strings on the end, as in `%g.s'.
	%g also has the same effect of %d.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted if CC exits
	successfully.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
 %o	substitutes the names of all the output files, with spaces
	automatically placed around them.  You should write spaces
	around the %o as well or the results are undefined.
	%o is for use in the specs for running the linker.
	Input files whose names have no recognized suffix are not compiled
	at all, but they are included among the output files, so they will
	be linked.
 %p	substitutes the standard macro predefinitions for the
	current target machine.  Use this when running cpp.
 %P	like %p, but puts `__' before and after the name of each macro.
	This is for ANSI C.
 %s     current argument is the name of a library or startup file of some sort.
        Search for that file in a standard list of directories
	and substitute the full pathname found.
 %eSTR  Print STR as an error message.  STR is terminated by a newline.
        Use this when inconsistent options are detected.
 %a     process ASM_SPEC as a spec.
        This allows config.h to specify part of the spec for running as.
 %l     process LINK_SPEC as a spec.
 %L     process LIB_SPEC as a spec.
 %S     process STARTFILE_SPEC as a spec.  A capital S is actually used here.
 %c	process SIGNED_CHAR_SPEC as a spec.
 %C     process CPP_SPEC as a spec.  A capital C is actually used here.
 %1	process CC1_SPEC as a spec.
 %{S}   substitutes the -S switch, if that switch was given to CC.
	If that switch was not specified, this substitutes nothing.
	Here S is a metasyntactic variable.
 %{S*}  substitutes all the switches specified to CC whose names start
	with -S.  This is used for -o, -D, -I, etc; switches that take
	arguments.  CC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S:X} substitutes X, but only if the -S switch was given to CC.
 %{!S:X} substitutes X, but only if the -S switch was NOT given to CC.
 %{|S:X} like %{S:X}, but if no S switch, substitute `-'.
 %{|!S:X} like %{!S:X}, but if there is an S switch, substitute `-'.

The conditional text X in a %{S:X} or %{!S:X} construct may contain
other nested % constructs or spaces, or even newlines.
They are processed as usual, as described above.

The character | is used to indicate that a command should be piped to
the following command, but only if -pipe is specified.

Note that it is built into CC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  CC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

CC also knows implicitly that arguments starting in `-l' are to
be treated as compiler output files, and passed to the linker in their proper
position among the other output files.

*/

/* define this macro for an informational output if no input files
   have been given to the driver. */
#define USAGE \
"usage: trimcc [switches] <source file> \n \
switches: \n \
  -c : generate C++ intermediate code.\n \
  -g : generate debugging info.\n \
  -O : optimize code (all levels).\n \
  -s : generate native assembler code.\n \
  -t : generate TRIM assembler code.\n \
  -v : run the TRIM compiler in verbose mode.\n \
\n \
  -I<dirname> : give alternate include path name.\n \
  -L<dirname> : give alternate library path name.\n \
  -n<name>    : give the program an internal name.\n \
  -o<file>    : give an alternate name for output file.\n \
"


/* define which switches take arguments. */

#define SWITCH_TAKES_ARG(c) \
((c) == 'I' || (c) == 'L' || (c) == 'n' || (c) == 'o')

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* Names to predefine in the preprocessor for this target machine.  */

#ifdef __unix__
#define CPP_PREDEFINES "-Dsun -Dunix "
#endif

#ifdef __MSDOS__
#define CPP_PREDEFINES "-DMSDOS"
#endif


/* Machine dependent ccp options.  */

#define CPP_SPEC ""


/* Machine dependent ld options.  */

#define LINK_SPEC ""


/* Machine dependent libraries.  */

#define LIB_SPEC ""
 

/* We specify crt0.o as -lcrt0.o so that ld will search the library path. */

#define STARTFILE_SPEC  "" 


/* Standard exec prefix on this release. */

#ifdef __unix__
#define STANDARD_EXEC_PREFIX 0
#endif

#ifdef __MSDOS__
#define STANDARD_EXEC_PREFIX 0
#endif


/* Give a Name to the environment variable that carries a prefix
   which we will try. */

#define ENV_EXEC_PREFIX "TRIM_EXEC"


/* how do we separate directory names. */

#ifdef __unix__
#define DIRDELIMITER '/'
#endif

#ifdef __MSDOS__
#define DIRDELIMITER '\\'
#endif


/* some machine need special extensions on their executables. */

#ifdef __MSDOS__
#define MACHINE_SUFFIX ".exe"
#endif

/* select a base for temporary files. The environment variable TMPDIR
   is also consulted. */

#if 0
//def __unix__
#define P_tmpdir "/tmp"
#endif

#ifdef __MSDOS__
#define P_tmpdir "\\TMP"
#endif



/* 
   This macro defines the compilers used during the compilation of
   a source file. 

   The format of this macro is: 

      {"<.extension_name>", "<specs>\n<specs>\n"}[, {...}[, {...}]]

   For a definition of `specs' see above. 
*/

/*
   *NOTE* the following needs to be adjusted for your TRIM installation:

   -I...  : needs to point to the value of INC_TARGET in the makefile.
   -L...  : needs to point to the value of LIB_TARGET in the makefile.
*/

#define	EQ_COMPILERS \
"\
eqcomp %{v} %{V:-v} %{g} \
       %{!O: %{t: -o %b.trm} %{!t: -o %g.tmp}} \
       %{O: -O -o %g.trm} \
       %i \n \
%{O: trimopt %{v} %{V:-v} \
             %{t:  -o %b.trm} \
             %{!t: -o %g.tmp} \
             %g.trm \n } \
"

#define	TRIM_COMPILERS(input) \
"\
%{c: trimasm %{v} %{V:-v} %{n*} %{g} -o %b.cxx " input "  \n } \
%{!c: trimasm %{v} %{V:-v} %{n*} %{g} -o %g.cxx " input " \n } \
%{!c: %{s: g++ -I" INSTALL_TARGET " %{I*} \
               %{v} %{V:-v} %{g: -g -DDEBUG} -S %{O} \
               %{o*} %{!o: -o %b.s} %g.cxx \n }} \
%{!c: %{!s: g++ -c -I" INSTALL_TARGET " %{I*} %{v} %{V:-v} \
                %{g: -g -DDEBUG} %{O} \
                -o %g.o %g.cxx \n }} \
%{!c: %{!s: g++ %{v} %{V:-v} %{g: -g -DDEBUG} \
                %{o*} %{!o: -o %b} %g.o -L" INSTALL_TARGET " \
  	        %{L*} -ltrim \n }} \
"

#define COMPILER_DEFINITION \
{ ".trm", TRIM_COMPILERS("%i") }, \
{ ".eq",  EQ_COMPILERS "%{!t: " TRIM_COMPILERS("%g.tmp") "}" } 

/* define this macro to do any post processing or linking... 
   the format of this macro is:

        "<specs>\n[<specs>\n[<specs>\n]]"

   for a definition of `specs' see above. */

#define LINKER_DEFINITION  ""


