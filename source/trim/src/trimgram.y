%{

/////////////////////////////////////////////////////////////////////////////
//  parser for TRIM assembly language. 
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////


#include "config.hxx"
#include "asm.hxx"

void yyerror (char * str);

%}


%union
{
	int		y_arity;
	String		y_string;
	TrimMode	y_mode;
	Operator *	y_op;
	SortList *	y_sort_list;
	Instr *		y_instr;
};

%type <y_op>		op
%type <y_sort_list>	sort_list
%type <y_instr>		instr
%type <y_instr>		z_instr
%type <y_instr>		compute_instr
%type <y_instr>		mode_instr
%type <y_instr>		jump_instr
%type <y_mode>		mode
%type <y_string>	var_name 
%type <y_string>	op_name 
%type <y_string>	sort_name 
%type <y_string>	label
%type <y_arity>		arity

%token	<y_string>	TIDENTIFIER

%token	TNORMAL
%token	TMATCHED
%token	TREDUCED
%token	TFAILURE
%token	TDONE
%token	TNFORM
%token	TLINK
%token	TAPPLY
%token	TLOAD
%token	TEXIT
%token	TABORT
%token	TRESTORE
%token	TSAVEOP
%token	TKILLOP
%token	TPUSHFRAME
%token	TPOPFRAME
%token	TMATCH
%token	TBIND
%token	TGET
%token	TBUILD
%token	TNOP
%token	TSET
%token	TIS
%token	TPEEK
%token	TJUMPT
%token	TJUMPF
%token	TJUMP
%token	TCALL
%token	TRETURN
%token	TSUBSORTS
%token	TOP
%token	TARROW

%start module

%%

module:
	  sort_rel_list op_list program
	;

program:
	  labeled_instr semi_opt program
	| /* empty */
	;


sort_rel_list:
 	  sort_rel_list TSUBSORTS sort_list '<' sort_name
		{
			SortTuple * st = mem(new SortTuple($5,$3));
			instr_handler->GetSortOrder()->
				GetSortTupleList()->Append(st);
		}
	| /* empty */
		{
			/* nothing to do */
		}
	;
	

op_list:
	  op_list op		
		{ 
			instr_handler->GetOpTable()->Append($2);
		}
	| /* empty */		
		{ 
			/* nothing to do */
		}
	;



op:
	  TOP op_name ':' sort_list TARROW sort_name
		{
			$$ = mem(new Operator($2,$4,$6));
		}
	;


sort_list:
	  sort_list sort_name	
		{ 
			$1->Append($2); 
			$$ = $1; 
		}
	| /* empty */		
		{ 
			$$ = mem(new SortList(SORT_LIST_SIZE)); 
		}
	;


labeled_instr:
	  label ':' instr	
		{ 
			label_tab->LabelDef($1, $3); 
			$3->GetLabel() = $1;
		}
	| instr			
		{ 
			/* nothing to do */ 
		}
	;

instr:
	  z_instr		{ $$ = $1; }
	| compute_instr		{ $$ = $1; }
	| mode_instr		{ $$ = $1; }
	| jump_instr		{ $$ = $1; }
	;

z_instr:
	  TLINK		{ $$ = append_instr(mem(new LinkInstr)); }
	| TAPPLY	{ $$ = append_instr(mem(new ApplyInstr)); }
	| TLOAD		{ $$ = append_instr(mem(new LoadInstr)); }
	| TEXIT		{ $$ = append_instr(mem(new ExitInstr)); }
	| TABORT	{ $$ = append_instr(mem(new AbortInstr)); }
	| TRESTORE	{ $$ = append_instr(mem(new RestoreInstr)); }
	| TSAVEOP	{ $$ = append_instr(mem(new SaveOpInstr)); }
	| TKILLOP	{ $$ = append_instr(mem(new KillOpInstr)); }
	| TPUSHFRAME	{ $$ = append_instr(mem(new PushFrameInstr)); }
	| TPOPFRAME	{ $$ = append_instr(mem(new PopFrameInstr)); }
	;

compute_instr:
	  TMATCH op_name arity
		{ 
			$$ = append_instr(mem(new MatchInstr($2, $3))); 
		}
	| TBIND var_name '[' sort_name ']'
		{ 
			$$ = append_instr(mem(new BindInstr($2, $4))); 
		}
	| TGET var_name 
		{ 
			$$ = append_instr(mem(new GetInstr($2)));
		}
	| TBUILD op_name sort_name arity
		{ 
			$$ = append_instr(mem(new BuildInstr($2, $3, $4))); 
		}
	| TNOP
		{ 
			$$ = append_instr(mem(new NopInstr));
		}
	;

mode_instr:
	  TSET mode	
		{ 
			if ($2 & (DONE|NFORM))
			{
				fprintf(stderr,
					"error -- "
					"mode field `%s' illegal in this "
					"context (%s:%d).\n",
					ConvertTrimMode($2),
					scanner_filename(),
					scanner_line());
				errors++;

				$$ = append_instr(mem(new SetInstr(NONE))); 
			}
			else
			{
				$$ = append_instr(mem(new SetInstr($2))); 
			}
		}
	| TIS mode	
		{ 
			$$ = append_instr(mem(new IsInstr($2))); 
		}
	| TPEEK op_name sort_name arity
		{ 
			$$ = append_instr(mem(new PeekInstr($2, $3, $4))); 
		}
	;

jump_instr:
	  TJUMPT label	
		{ 
			$$ = append_instr(mem(new JumpTInstr($2)));
			label_tab->LabelRef($2, $$);
		}
	| TJUMPF label
		{ 
			$$ = append_instr(mem(new JumpFInstr($2)));
			label_tab->LabelRef($2, $$);
		}
	| TJUMP label
		{ 
			$$ = append_instr(mem(new JumpInstr($2)));
			label_tab->LabelRef($2, $$);
		}
	| TCALL label
		{ 
			$$ = append_instr(mem(new CallInstr($2)));
			label_tab->LabelRef($2, $$);
		}
	| TRETURN
		{ 
			$$ = append_instr(mem(new ReturnInstr));
		}
	;	  

mode:
	  TNORMAL	{ $$ = NORMAL; }
	| TMATCHED	{ $$ = MATCHED; }
	| TREDUCED	{ $$ = REDUCED; }
	| TFAILURE	{ $$ = FAILURE; }
	| TDONE		{ $$ = DONE; }
	| TNFORM	{ $$ = NFORM; }
	;

var_name:
	TIDENTIFIER 	{ $$ = $1; }
	;

op_name: 
	TIDENTIFIER 	{ $$ = $1; }
	;

sort_name:
	TIDENTIFIER 	{ $$ = $1; }
	;

arity:
	TIDENTIFIER	
		{ 
			if (!isdigit(*($1)))
			{
				yyerror("illegal arity specifier");
			}
			$$ = atoi($1); 
		}
	;

label:
	TIDENTIFIER 	{ $$ = clean_up_iden($1); }
	;
		
semi_opt:
	  ';'
	| /* empty */
	;
	
%%

void yyerror (char * str)
{
  	extern char * yytext;

	fprintf(stderr, "error -- %s near `%s' (%s:%d)\n", 
		str, yytext, scanner_filename(), scanner_line());

	errors++;	
}
