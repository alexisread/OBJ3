/////////////////////////////////////////////////////////////////////////////
//  eqcomp.cxx
//
//  implementations for the Eq compiler.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "eqcomp.hxx"
#include "fileio.h"

/////////////////////////////////////////////////////////////////////////////

int 		errors = 0;
LabelStack *	label_stack = NULL;
StringHash *	hash = NULL;
EqEnv *		all_env = NULL;
char *		infilename = NULL;
char *		outfilename = NULL;
int		vflag = 0;
int		gflag = 0;
int		Oflag = 0;
EqOSMod *	module = NULL;

/////////////////////////////////////////////////////////////////////////////

Label * LabelList::FindLabel (String label)
{
	for (int i = 0; i < Count(); i++)
	{
		if (streq(Get(i)->GetLabelName(), label))
		{
			return Get(i);
		}
	}

	return NULL;
}

/////////////////////////////////////////////////////////////////////////////

int LabelStack::GetLabelId (String name)
{
	if (Count() && Top()->FindLabel(name))
	{
		return Top()->FindLabel(name)->GetLabelId();
	}
	else if (Count())
	{
		Label * l = mem(new Label(name, IncLabelCount()));
		Top()->Append(l);

		return l->GetLabelId();
	}
	else
	{
		LabelList * lid = mem(new LabelList(LABEL_ID_LIST_LENGTH));

		Push(lid);

		Label * l = mem(new Label(name, IncLabelCount()));
		Top()->Append(l);

		return l->GetLabelId();
	}
}
				
/////////////////////////////////////////////////////////////////////////////

int LabelStack::GetLabelIdPush (String name)
{
	LabelList * lid = mem(new LabelList(LABEL_ID_LIST_LENGTH));

	Push(lid);

	return GetLabelId(name);
}

/////////////////////////////////////////////////////////////////////////////

int LabelStack::GetLabelIdPushReset (String name)
{
	ResetLabelCount();

	return GetLabelIdPush(name);
}

/////////////////////////////////////////////////////////////////////////////

int LabelStack::GetLabelIdPop (String name)
{
	int l = GetLabelId(name);

	Pop();

	return l;
}

/////////////////////////////////////////////////////////////////////////////

void EqVar::Print (FILE * fp)
{
	int i;

	fprintf(fp, "EqVar: `%s':\n", GetVar());
	fprintf(fp, "  LHS Occur (%d):", GetLHS()->Count());

	for (i = 0; i < GetLHS()->Count(); i++)
	{	
		if (i%5 == 0)
		{	
			fprintf(fp, "\n    ");
		}

		EqTerm * t = GetLHS()->Get(i);

		fprintf(fp, "(%s:%d) ", t->GetFileName(), t->GetLine());
	}
	
	fprintf(fp, "\n");

	fprintf(fp, "  RHS Occur (%d)", GetRHS()->Count());

	for (i = 0; i < GetRHS()->Count(); i++)
	{	
		if (i%5 == 0)
		{	
			fprintf(fp, "\n    ");
		}

		EqTerm * t = GetRHS()->Get(i);

		fprintf(fp, "(%s:%d) ", t->GetFileName(), t->GetLine());
	}

	fprintf(fp, "\n");
}

/////////////////////////////////////////////////////////////////////////////

EqVar * EqEnv::Find(String x)
{
	for (int i = 0; i < Count(); i++)
	{
		EqVar * v = Get(i);

		if (streq(x, v->GetVar()))
		{
			return v;
		}
	}

	// nothing found -- enter a new variable record.

	return Enter(x);
}

/////////////////////////////////////////////////////////////////////////////

EqVar * EqEnv::Fetch(String x)
{
	for (int i = 0; i < Count(); i++)
	{
		EqVar * v = Get(i);

		if (streq(x, v->GetVar()))
		{
			return v;
		}
	}

	// nothing found -- return NULL;

	return NULL;
}
			
/////////////////////////////////////////////////////////////////////////////
// does an append of environment env to `this' environment while respecting
// the semantics of variable declarations.

EqEnv * EqEnv::CleanAppend (EqEnv * env)
{
	assert(env);

	for (int i = 0; i < env->Count(); i++)
	{
		EqVar * vs = env->Get(i);
		EqVar * vt = this->Find(vs->GetVar());

		for (int j = 0; j < vs->GetLHS()->Count(); j++)
		{
			vt->GetLHS()->Append(vs->GetLHS()->Get(j));
		}

		for (int k = 0; k < vs->GetRHS()->Count(); k++)
		{
			vt->GetRHS()->Append(vs->GetRHS()->Get(k));
		}
	}
	
	return this;
}

/////////////////////////////////////////////////////////////////////////////

void EqEnv::Print (FILE * fp)
{
	fprintf(fp, "*** EqEnv ***\n");

	for (int i = 0; i < Count(); i++)
	{
		Get(i)->Print(fp);
	}
}

/////////////////////////////////////////////////////////////////////////////

int EqTreeNode::indent_level = 0;

/////////////////////////////////////////////////////////////////////////////

void EqTreeNode::Indent (FILE * fp)
{
	for (int i = 0 ; i < indent_level; i++)
	{	
		fprintf(fp, "  ");
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqTreeNode::FillEqEnv (EqEnv *)
{
	fprintf(stderr, "fatal -- illegal call to `FillEqEnv' (%s)\n",
		ConvertEqTreeNodeType(GetTag()));
	abort();
}

/////////////////////////////////////////////////////////////////////////////

void EqTreeNode::LeftLinear (void)
{
	fprintf(stderr, "fatal -- illegal call to `LeftLinear' (%s)\n",
		ConvertEqTreeNodeType(GetTag()));
	abort();
}

/////////////////////////////////////////////////////////////////////////////

void EqOSMod::Parse (FILE * fp)
{
	yyin = fp;

	yyparse();
}

/////////////////////////////////////////////////////////////////////////////

void EqOSMod::Print (FILE * fp)
{
	GetSortOrder()->Print(fp);
	GetOpTable()->Print(fp);
	GetEqMod()->Print(fp);
}

/////////////////////////////////////////////////////////////////////////////

void EqOSMod::Translate (FILE * fp)
{
	// write out the sort order -- in closed form!
	GetSortOrder()->Gen(fp);

	// write out the op table.
	GetOpTable()->Gen(fp);

	// translate the equational module.
	GetEqMod()->Translate(fp);
}

/////////////////////////////////////////////////////////////////////////////

void EqMod::Print (FILE * fp)
{
	fprintf(fp, "{\n");

        indent_level++;
	eq_list->Print(fp);
        indent_level--;

	fprintf(fp, "}\n");
}

/////////////////////////////////////////////////////////////////////////////

void EqMod::Translate (FILE * fp)
{
	if (eq_list->Count() == 0)
	{
		fprintf(fp,"\tEXIT ;\n");
		return;
	}

	GEN("BEGINMOD :\n", NULL);
	GEN("\tIS NFORM ;\n", NULL);
	GEN("\tJUMPF L%d ;\n", 
		label_stack->GetLabelIdPushReset(STRING("CONTMOD"))) ;
	GEN("\tSAVEOP ;\n", NULL);
	GEN("\tJUMP BEGINMOD ;\n", NULL);
	GEN("L%d :\n", label_stack->GetLabelId(STRING("CONTMOD")));
	GEN("\tLINK ;\n", NULL);
	GEN("\tRESTORE ;\n", NULL);

	eq_list->Translate(fp);

	GEN("\tIS DONE ;\n", NULL);
	GEN("\tJUMPT L%d ;\n", label_stack->GetLabelId(STRING("ENDMOD")));
	GEN("\tIS FAILURE ;\n", NULL);
	GEN("\tJUMPF BEGINMOD ;\n", NULL);
	GEN("\tSAVEOP ;\n", NULL);
	GEN("\tJUMP BEGINMOD ;\n", NULL);
	GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("ENDMOD"))) ; 
	GEN("\tRESTORE ;\n", NULL);
	GEN("\tRETURN ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void EqMod::FillEqEnv (EqEnv * env)
{
	eq_list->FillEqEnv(env);
}

/////////////////////////////////////////////////////////////////////////////

void EqMod::LeftLinear (void)
{
	eq_list->LeftLinear();
}

/////////////////////////////////////////////////////////////////////////////
// inserts a new equation `eq'

void EqMod::AddEq (Eq * e)
{
	extern int Oflag;

	assert(e);

	if (Oflag)
	{
		eq_list->InsertSorted(e);
	}
	else
	{
		eq_list->Append(e); 
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqList::Print (FILE * fp)
{
	for (int i = 0; i < this->Count(); i++)
	{
		this->Get(i)->Print(fp);
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqList::Translate (FILE * fp)
{
	if (this->Count())
	{
		this->TranslateEquation(fp,0);
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqList::TranslateEquation (FILE * fp, int i)
{
	this->Get(i)->Translate(fp);
	i++;

	if (i < this->Count())
	{
		GEN("\tIS REDUCED ;\n", NULL);
		GEN("\tJUMPT L%d ;\n", 
			label_stack->GetLabelIdPush(STRING("CONTINUE")));
		GEN("\tRESTORE ;\n", NULL);

		this->TranslateEquation(fp,i);

		GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("CONTINUE")));
		GEN("\tNOP ;\n", NULL);
	}

}

/////////////////////////////////////////////////////////////////////////////

void EqList::FillEqEnv (EqEnv * env)
{
	assert(env);

	for (int i = 0; i < this->Count(); i++)
	{
		this->Get(i)->FillEqEnv(env);
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqList::LeftLinear (void)
{
	for (int i = 0; i < this->Count(); i++)
	{
		this->Get(i)->LeftLinear();
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqList::InsertSorted (Eq * e)
{
	String op_e = e->GetLHS()->GetName();

	for (int i = 0; i < this->Count(); i++)
	{
		String op = this->Get(i)->GetLHS()->GetName();

		if (op_e == op)
		{
			this->AppendPos(i,e);
			return;
		}
	}

	this->Append(e);
}

/////////////////////////////////////////////////////////////////////////////

void UEq::Print (FILE * fp)
{
	Indent(fp);
	fprintf(fp, "eq\n");

	lhs->Print(fp);

	fprintf(fp, "\n");
	Indent(fp);
	fprintf(fp, "  =>\n");

	indent_level++;
	rhs->Print(fp);
	indent_level--;

	fprintf(fp, " .\n");
}

/////////////////////////////////////////////////////////////////////////////

void UEq::Translate (FILE * fp)
{
	assert(lhs && rhs);

	lhs->Translate(fp);

	GEN("\tIS FAILURE ;\n", NULL);
	GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("EQLABEL")));
	GEN("\tKILLOP ;\n", NULL);

	rhs->Translate(fp);

	GEN("\tAPPLY ;\n", NULL);
	GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("EQLABEL")));
	GEN("\tNOP ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void UEq::FillEqEnv (EqEnv * env)
{
	// this is the local equation environment

	this->env = mem(new EqEnv(ENV_SIZE));

	// initialize it with the variables from the current equation

	GetLHS()->FillEqEnv(this->env);
	GetRHS()->FillEqEnv(this->env);

#ifdef DEBUG
	fprintf(stderr, "*** equation at (%s:%d) ***\n", 
		GetFileName(), GetLine());

	this->Print(stderr);
	this->env->Print(stderr);
#endif

	// append the local environment to the global environment

	env->CleanAppend(this->env);
}

/////////////////////////////////////////////////////////////////////////////
// *NOTE* this is where we should put the translation from non-leftlinear
// stuff to leftlinear equations.

void UEq::LeftLinear (void)
{
	for (int i = 0; i < GetEnv()->Count(); i++)
	{
		if (GetEnv()->Get(i)->GetLHS()->Count() > 1)
		{
			fprintf(stderr, 
				"error -- "
      				"equation is not left-linear (%s:%d).\n",
				GetFileName(),
				GetLine());
			errors++;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

void CEq::Print (FILE * fp)
{
	Indent(fp);
	fprintf(fp, "ceq\n");

	lhs->Print(fp);

	fprintf(fp, "\n");
	Indent(fp);
	fprintf(fp, "  =>\n");

	{
		indent_level++;
		rhs->Print(fp);

		fprintf(fp, "\n");
		Indent(fp);
		fprintf(fp, "  if\n");

		{
			indent_level++;
			cond->Print(fp);
			indent_level--;
		}

		indent_level--;
	}

	fprintf(fp, " .\n");
}

/////////////////////////////////////////////////////////////////////////////

void CEq::Translate (FILE * fp)
{
	assert(lhs && rhs && cond);

	lhs->Translate(fp);

	GEN("\tIS FAILURE ;\n", NULL);
	GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("EQLABEL")));
	GEN("\tPUSHFRAME ;\n", NULL);

	cond->Translate(fp);

	GEN("\tLOAD ;\n", NULL);
	GEN("\tCALL BEGINMOD ;\n", NULL);

	fprintf(fp, "\tPEEK %s %s 0 ;\n", EQ_BOOL_TRUE, EQ_BOOL_SORT);

	GEN("\tJUMPF L%d ;\n", label_stack->GetLabelId(STRING("CONDFAIL")));
	GEN("\tPOPFRAME ;\n", NULL);
	GEN("\tKILLOP ;\n", NULL);

	rhs->Translate(fp);

	GEN("\tAPPLY ;\n", NULL);
	GEN("\tJUMP L%d ;\n", label_stack->GetLabelId(STRING("EQLABEL")));
        GEN("L%d :\n", label_stack->GetLabelId(STRING("CONDFAIL"))); 
	GEN("\tPOPFRAME ;\n", NULL);
	GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("EQLABEL")));
	GEN("\tNOP ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void CEq::FillEqEnv (EqEnv * env)
{
	// this is the local equation environment

	this->env = mem(new EqEnv(ENV_SIZE));

	// initialize it with the variables from the current equation

	GetLHS()->FillEqEnv(this->env);
	GetRHS()->FillEqEnv(this->env);
	GetCOND()->FillEqEnv(this->env);

#ifdef DEBUG
	fprintf(stderr, "*** equation at (%s:%d) ***\n", 
		GetFileName(), GetLine());

	this->Print(stderr);
	this->env->Print(stderr);
#endif

	// append the local environment to the global environment

	env->CleanAppend(this->env);
}

/////////////////////////////////////////////////////////////////////////////
// *NOTE* this is where we should put the translation from non-leftlinear
// stuff to leftlinear equations.

void CEq::LeftLinear (void)
{
	for (int i = 0; i < GetEnv()->Count(); i++)
	{
		if (GetEnv()->Get(i)->GetLHS()->Count() > 1)
		{
			fprintf(stderr, 
				"error -- "
      				"equation is not left-linear (%s:%d).\n",
				GetFileName(),
				GetLine());
			errors++;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqLOp::Print (FILE * fp)
{
	Indent(fp);

	fprintf(fp, "(op %d %s %s", GetArity(), GetOp(), GetSort());

	if (GetArity() == 0)
	{
		fprintf(fp, ")");
	}
	else
	{
		indent_level++;

		for (int i = 0; i < GetArity(); i++)
		{
			fprintf(fp, "\n");
	
			children->Get(i)->Print(fp);
		}

		fprintf(fp, ")");
		indent_level--;
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqLOp::Translate (FILE * fp)
{
	GEN("\tIS FAILURE ;\n", NULL);
	GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("LABEL")));

	fprintf(fp, "\tMATCH %s %d ;\n", GetOp(), GetArity());

	if (GetArity() > 0)
	{
		GEN("\tIS FAILURE ;\n", NULL);
		GEN("\tJUMPT L%d ;\n", 
		    label_stack->GetLabelId(STRING("LABEL")));
		
		
		for (int i = 0; i < GetArity(); i++)
		{
			assert(children->Get(i));

			children->Get(i)->Translate(fp);
		}
	}

	GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("LABEL")));
	GEN("\tNOP ;\n", NULL);

}

/////////////////////////////////////////////////////////////////////////////

void EqLOp::FillEqEnv (EqEnv * env)
{
	for (int i = 0; i < GetArity(); i++)
	{	
		GetChildren()->Get(i)->FillEqEnv(env);
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqLVar::Print (FILE * fp)
{
	Indent(fp);
	fprintf(fp, "(var %s [ ", GetVar());
	GetSortList()->Print(fp);
	fprintf(fp, "])");
}

/////////////////////////////////////////////////////////////////////////////

void EqLVar::Translate (FILE * fp)
{
	GEN("\tIS FAILURE ;\n", NULL);
	GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("LABEL")));

	fprintf(fp, "\tBIND %s [ ", GetVar());
	GetSortList()->Print(fp);
	fprintf(fp, "] ;\n");

	GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("LABEL"))); 
	GEN("\tNOP ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void EqLVar::FillEqEnv (EqEnv * env)
{
	assert(env);

	EqVar * v = env->Find(GetVar());

	v->GetLHS()->Append(this);
}

/////////////////////////////////////////////////////////////////////////////

void EqROp::Print (FILE * fp)
{
	Indent(fp);

	fprintf(fp, "(op %d %s %s", GetArity(), GetOp(), GetSort());

	if (GetArity() == 0)
	{
		fprintf(fp, ")");
	}
	else
	{
		indent_level++;

		for (int i = 0; i < GetArity(); i++)
		{
			fprintf(fp, "\n");
	
			children->Get(i)->Print(fp);
		}

		fprintf(fp, ")");
		indent_level--;
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqROp::Translate (FILE * fp)
{
	if (GetArity() > 0)
	{
		for (int i = 0; i < GetArity(); i++)
		{
			assert(children && children->Get(i));

			children->Get(i)->Translate(fp);
		}
	}

	// *NOTE* lhh: more efficient code generation.
	// GEN("\tIS FAILURE ;\n", NULL);
	// GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("LABEL")));

	fprintf(fp, "\tBUILD %s %s %d ;\n", GetOp(), GetSort(), GetArity());

	// GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("LABEL")));
	// GEN("\tNOP ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void EqROp::FillEqEnv (EqEnv * env)
{
	for (int i = 0; i < GetArity(); i++)
	{	
		assert(children && children->Get(i));

		GetChildren()->Get(i)->FillEqEnv(env);
	}
}

/////////////////////////////////////////////////////////////////////////////

void EqRVar::Print (FILE * fp)
{
	Indent(fp);
	fprintf(fp, "(var %s [ ", GetVar());
	GetSortList()->Print(fp);
	fprintf(fp, "])");
}

/////////////////////////////////////////////////////////////////////////////

void EqRVar::Translate (FILE * fp)
{
	// *NOTE* lhh: more efficient code generation.
	// GEN("\tIS FAILURE ;\n", NULL);
	// GEN("\tJUMPT L%d ;\n", label_stack->GetLabelIdPush(STRING("LABEL")));

	fprintf(fp, "\tGET %s ;\n", GetVar());

	// GEN("L%d :\n", label_stack->GetLabelIdPop(STRING("LABEL"))); 
	// GEN("\tNOP ;\n", NULL);
}

/////////////////////////////////////////////////////////////////////////////

void EqRVar::FillEqEnv (EqEnv * env)
{
	assert(env);

	if (!env->Fetch(GetVar()))
	{
		fprintf(stderr, 
			"error -- "
			"variable `%s' did not appear on left-hand side "
			"(%s:%d).\n",
			GetVar(),
			GetFileName(),
			GetLine());
		errors++;
	}
			
	EqVar * v = env->Find(GetVar());

	v->GetRHS()->Append(this);
}

/////////////////////////////////////////////////////////////////////////////

String GenName (void)
{
	static int var_count = 0;

	char buf [MAX_NAME_LEN];

	sprintf(buf, "%s_%d", EQ_VAR_TAG, var_count++);

	return STRING(buf);
}

/////////////////////////////////////////////////////////////////////////////

main(int argc, char * argv[])
{
	FILE * fp; 
	int i = 1;
	argc--;

        while (argc && argv[i][0] == '-')
        {
                switch (argv[i][1])
                {
                        case 'o':
                                if (argv[i][2])
                                        outfilename = &argv[i][2];
                                else
                                        outfilename = argv[(argc--,++i)];
                                break;

                        case 'O':
                                Oflag++;
                                break;

                        case 'v':
                                vflag++;
                                break;

                        case 'g':
                                gflag++;
                                break;

                        default:
                                fprintf(stderr,
					"warning -- unknown switch `%s'.\n", 
					argv[i]);
                                break;
                }
                i++;
		argc--;
        }

	if (vflag)
	{
		fprintf(stderr, 
		    	"Eq Compiler Rev %s -- "
			"(c) Copyright 1995, Lutz H. Hamel\n",
			EQCOMP_VERSION);
	}

	if (!argc)
	{
		fprintf(stderr, 
			"usage: eqcomp [-g|-v|-o <out_file>] <eq_file>\n");
		fprintf(stderr, 
			"  -g : generate debugging info.\n");
		fprintf(stderr, 
			"  -O : optimze.\n");
		fprintf(stderr, 
			"  -o : specifiy alternate output file name.\n");
		fprintf(stderr, 
			"  -v : run compiler in verbose mode.\n");
		exit(1);
	}

        infilename = argv[i++];
	strcpy(scanner_filename(), infilename);

	// make our output file name
	if (!outfilename)
	{
		char * base = find_base(infilename);

	        outfilename = (char *)malloc(strlen(base) + 
				strlen(TRIM_FILE_EXT) + 1);
		mem_assert(outfilename);

		strcpy(outfilename, base);
		strcat(outfilename, TRIM_FILE_EXT);
	}

	if (vflag)
	{
		fprintf(stderr, "compiling `%s' => `%s'\n", 
			infilename, outfilename);
	}


	// init our basic building blocks of our compiler.

	label_stack 	= mem(new LabelStack(LABEL_STACK_DEPTH));
	hash 		= mem(new StringHash(HASH_TAB_SIZE, BUCKET_SIZE));
	all_env 	= mem(new EqEnv(ENV_SIZE));


	// open the input file for reading

        fp = text_read_open(infilename);

        if (!fp)
        {
                fprintf(stderr,
			"fatal -- unable to open `%s' for input.\n", 
			infilename);
		exit(1);
        }


	// build the abstract syntax tree -- `Parse' sets the variable
	// `module' to the point to the abstract syntax tree of the 
	// module parsed.

	EqOSMod::Parse(fp);

	text_close(fp);


	// do some semantic checks:
	// * check that every variable on the right appears on the left
	// * check for left-linearity

	if (!errors)
	{
		module->GetEqMod()->FillEqEnv(all_env);
	}

	if (!errors)
	{
		module->GetEqMod()->LeftLinear();
	}

	if (gflag && !errors)
	{
		fprintf(stderr, "*** the module ***\n");
		module->Print(stderr);

		fprintf(stderr, "*** the `all_env' ***\n");
		all_env->Print(stderr);
	}


	// intermediate translation phases (?):
	// * order sortedness => many sortedness
	// * conditional equations => unconditional equations


	// open and prepare output file for writing...

	fp = text_write_open(outfilename, "w");

	if (!fp)
	{
		fprintf(stderr, "fatal -- could not open file `%s'.\n",
			outfilename);
		exit(1);
	}

	time_t clock = time(NULL);

	fprintf(fp, "--- Generated by the Eq Compiler Rev %s -- %s", 
		EQCOMP_VERSION,	ctime(&clock));
	fprintf(fp, "--- %s => %s\n", infilename, outfilename);
	fprintf(fp, "\n");



	// final translation step: Eq => TRIM code.

	if (!errors)
	{
		module->Translate(fp);
	}


	// all done.

	text_close(fp);


	// if there were errors get rid of the outputfile.

	if (errors)
	{
		unlink(outfilename);
	}


	// all done...

	if (vflag)
	{
		if (errors)
		{
			fprintf(stderr, 
				"compilation failed with %d error(s).\n", 
				errors);
		}
		else
		{
			fprintf(stderr, "compilation successful.\n");
		}
	}

	exit(errors);
}

/////////////////////////////////////////////////////////////////////////////
