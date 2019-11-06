%{	
/////////////////////////////////////////////////////////////////////////////
//  The scanner definition for the TRIM assembly language.
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
CH                      [!@#$%^&\*()_+\-=|\\~,\./:<>?']

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
#include "asm.hxx"
#include "trimgram.h"

#ifdef USE_YYWRAP
extern "C" int 		yywrap(); 
#endif

static long curr_line = 1;
static char curr_file [MAX_NAME_LEN] = "";

void comment();

%}

%%
"---"			{ comment(); }

%{
	/* TRIM Modes */	
%}
"NORMAL"		{ return(TNORMAL); }
"MATCHED"		{ return(TMATCHED); }
"REDUCED"		{ return(TREDUCED); }
"FAILURE"		{ return(TFAILURE); }
"DONE"			{ return(TDONE); }
"NFORM"			{ return(TNFORM); }

%{
	/* Z-Instructions */	
%}
"LINK"			{ return(TLINK); }
"APPLY"			{ return(TAPPLY); }
"LOAD"			{ return(TLOAD); }
"EXIT"			{ return(TEXIT); }
"ABORT"			{ return(TABORT); }
"RESTORE"		{ return(TRESTORE); }
"SAVEOP"		{ return(TSAVEOP); }
"KILLOP"		{ return(TKILLOP); }
"PUSHFRAME"		{ return(TPUSHFRAME); }
"POPFRAME"		{ return(TPOPFRAME); }

%{
	/* Compute Instructions */	
%}
"MATCH"			{ return(TMATCH); }
"BIND"			{ return(TBIND); }
"GET"			{ return(TGET); }
"BUILD"			{ return(TBUILD); }
"NOP"			{ return(TNOP); }

%{
	/* Mode Instructions */	
%}
"SET"			{ return(TSET); }
"IS"			{ return(TIS); }
"PEEK"			{ return(TPEEK); }

%{
	/* Jump Instructions */	
%}
"JUMPT"			{ return(TJUMPT); }
"JUMPF"			{ return(TJUMPF); }
"JUMP"			{ return(TJUMP); }
"CALL"			{ return(TCALL); }
"RETURN"		{ return(TRETURN); }


%{
	/* Misc. */	
%}
"subsorts"		{ return(TSUBSORTS); }
"op"			{ return(TOP); }
"->"			{ return(TARROW); }
"<"			{ return('<'); }
"["			{ return('['); }
"]"			{ return(']'); }
":"			{ return(':'); }
";"			{ return(';'); }

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
