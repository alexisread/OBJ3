%{	
/////////////////////////////////////////////////////////////////////////////
//  The scanner definition for the Eq equational language.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////
%}

D			[0-9]
O			[0-7]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
ws                      [ \t]
wsn                     [ \t\n]
CH                      [#!@$%^&\*_+\-=|\\~,\./:<>?']

%{
extern "C" {
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <io.h>
#define read _read
#define fileno _fileno
#endif

#if (defined(sun) || defined(__LPI__) || defined(__OS2__)) && !defined(WIN32) && !defined(__GNUC__)
int read(int fildes, char *buf, unsigned int nbyte);
#endif
}

#include "config.hxx"
#include "eqcomp.hxx"
#include "eqgram.h"

#ifdef USE_YYWRAP
extern "C" int 		yywrap(); 
#endif

static long curr_line = 1;
static char curr_file [MAX_NAME_LEN] = "";

void comment();

%}

%%
"---"			{ comment(); }

"eq"			{ return(TEQ); }
"cq"			{ return(TCQ); }
"=>"			{ return(TRIGHT_ARROW); }
"->"			{ return(TARROW); }
"if"			{ return(TIF); }
"subsorts"		{ return(TSUBSORTS); }
"op"			{ return(TOP); }
"var"			{ return(TVAR); }

":"			{ return(':'); }
"<"			{ return('<'); }
"["			{ return('['); }
"]"			{ return(']'); }
"{"			{ return('{'); }
"}"			{ return('}'); }
"("			{ return('('); }
")"			{ return(')'); }
"."			{ return('.'); }

({L}|{D}|{CH})+		{
				yylval.y_string = STRING(yytext); 
				return(TIDENTIFIER); 
			}

"\n"                    { curr_line++; }
[ \t\v\f]		{ ; }
.			{ 
				if (vflag)
					fprintf(stderr, 
						"warning -- "
						"unrecognized character "
						"`%s'.\n",
						isprint(*yytext) ?
						yytext :
						"\?");
			}
%%

////////////////////////////////////////////////////////////////////////////

#ifdef USE_YYWRAP

extern "C" int yywrap()
{
	return 1;
}

#endif
////////////////////////////////////////////////////////////////////////////

void comment()
{
	char c;

	while ((c = yyinput()) != '\n');

	unput(c);
}

////////////////////////////////////////////////////////////////////////////
// our scanner interface routines

int scanner_token (void)
{
	return yylex();
}

char * scanner_lexeme_val (void)
{
	return (char*)yytext;
}

int scanner_line (void)
{
	return curr_line;
}

char * scanner_filename (void)
{
	return curr_file;
}

////////////////////////////////////////////////////////////////////////////
