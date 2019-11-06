%{

/////////////////////////////////////////////////////////////////////////////
//  grammar for the Eq equational language. 
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////


#include "config.hxx"
#include "eqcomp.hxx"

extern int Oflag;

void yyerror (char * str);

%}


%union
{
	int		y_arity;
	String		y_string;
	EqMod *		y_eq_mod;
	EqList *	y_eq_list;
	Eq *		y_eq;
	EqTermList *	y_eq_term_list;
	EqTerm *	y_eq_term;
	SortOrder *	y_so;
	OpTable *	y_ot;
	Operator *	y_op;
	SortList *	y_sort_list;
};


%type	<y_so>			SortRelList
%type	<y_ot>			OpList
%type	<y_op>			Op
%type	<y_sort_list>		SortList
%type	<y_eq_mod>	 	Mod
%type	<y_eq_list>		EqList
%type	<y_eq>			Eq
%type	<y_eq_term>		LTerm
%type	<y_eq_term>		RTerm
%type	<y_eq_term>		BTerm
%type	<y_eq_term_list>	LTermList
%type	<y_eq_term_list>	RTermList
%type	<y_arity>		Arity
%type	<y_string>		OpName
%type	<y_string>		SortName
%type	<y_string>		VarName

%token	<y_string>		TIDENTIFIER

%token	TEQ
%token	TCQ
%token	TRIGHT_ARROW
%token	TIF
%token	TSUBSORTS
%token	TOP
%token	TVAR
%token	TARROW

%start OSMod

%%

OSMod:
	  SortRelList OpList Mod
		{
			$1->TransClosure();

			module = mem(new EqOSMod($1,$2,$3));
		}
	;


SortRelList:
 	  SortRelList TSUBSORTS SortName '<' SortName
		{
			$1->PutSort($3,$5);
			$$ = $1;
		}
	| /* empty */
		{
			$$ = mem(new SortOrder(SORT_ORDER_SIZE));
		}
	;
	  
OpList:
	  OpList Op		
		{ 
			$1->Append($2);
			$$ = $1;
		}
	| /* empty */		
		{ 
			$$ = mem(new OpTable(OP_TABLE_SIZE));
		}
	;

Op:
	  TOP OpName ':' SortList TARROW SortName
		{
			$$ = mem(new Operator($2,$4,$6));
		}
	;


SortList:
	  SortList SortName	
		{ 
			$1->Append($2); 
			$$ = $1; 
		}
	| /* empty */		
		{ 
			$$ = mem(new SortList(SORT_LIST_SIZE)); 
		}
	;


Mod:
	  '{' EqList '}' 	{ $$ = mem(new EqMod($2)); }
	;


EqList:
	  EqList Eq 		
		{ 
			// if the optimization flag is set,
			// sort the equations by topmost lhs
			// operator.
			if (Oflag)
			{
				$1->InsertSorted($2);
			}
			else
			{
				$1->Append($2); 
			}
			$$ = $1; 
		}
	| /* empty */		
		{ 
			$$ = mem(new EqList); 
		}
	;


Eq:
	  TEQ LTerm TRIGHT_ARROW RTerm '.'	
		{ 
			$$ = mem(new UEq($2,$4)); 
		}
	| TCQ LTerm TRIGHT_ARROW RTerm TIF BTerm '.'	
		{ 
			$$ = mem(new CEq($2,$4,$6)); 
		}
	;



LTerm:
	  '(' TOP Arity OpName SortName LTermList ')'
		{
			if ($3 != $6->Count())
			{
				yyerror("arity mismatch");
			}

			$$ = mem(new EqLOp($4, $5, $6));
		}
	| '(' TVAR VarName '[' SortList ']' ')'
		{
			$$ = mem(new EqLVar($3, $5));
		}
	;



RTerm:
	  '(' TOP Arity OpName SortName RTermList ')'
		{
			if ($3 != $6->Count())
			{
				yyerror("arity mismatch");
			}

			$$ = mem(new EqROp($4, $5, $6));
		}
	| '(' TVAR VarName '[' SortList ']' ')'
		{
			$$ = mem(new EqRVar($3,$5));
		}
	;


BTerm:
	  RTerm		
		{
			if (!streq($1->GetSort(), STRING(EQ_BOOL_SORT)))
			{
				yyerror(
				    "the term in condition is not of sort `"
				    EQ_BOOL_SORT
				    "'");
			}

			$$ = $1;
		}
	;


LTermList:
	  LTermList LTerm
		{
			$1->Append($2);
			$$ = $1;
		} 
	| /* empty */	
		{ 
			$$ = mem(new EqTermList(MAX_TDL_ARITY)); 
		}
	;



RTermList:
	  RTermList RTerm
		{
			$1->Append($2);
			$$ = $1;
		} 
	| /* empty */
		{ 
			$$ = mem(new EqTermList(MAX_TDL_ARITY)); 
		}
	;



Arity:
	  TIDENTIFIER	
		{ 
			if (!isdigit(*($1)))
			{
				yyerror("illegal arity specifier");
			}
			$$ = atoi($1); 
		}
	;
	

OpName:
	  TIDENTIFIER	{ $$ = $1; }
	;


SortName:
	  TIDENTIFIER	{ $$ = $1; }
	;


VarName:
	  TIDENTIFIER	{ $$ = $1; }
	;


%%

void yyerror (char * str)
{
        extern char *yytext;

	fprintf(stderr, "error -- %s near `%s' (%s:%d)\n", 
		str, yytext, scanner_filename(), scanner_line());

	errors++;
}


