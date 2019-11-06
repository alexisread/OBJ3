/////////////////////////////////////////////////////////////////////////////
//  basics.hxx
//
//  the "basics" from the TRIM specification.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef BASICS_HXX
#define BASICS_HXX

#include "const.hxx"
#include "util.hxx"

/////////////////////////////////////////////////////////////////////////////

class SortOrder;
class OpTable;

/////////////////////////////////////////////////////////////////////////////
// internal TRIM terms.
//
// terms may be described in a term description language which is
// very similar to LISP s-expressions:
//
// syntax: (<arity> <op_name> <ty_name> [<list of child nodes>])
//
// e.g.:  (2 PLUS INT (0 ONE INT) (0 TWO INT))
//
// the `#' character starts a comment which lasts for the rest of
// the line.
//


class iTerm;
typedef iTerm * iTermPtr;

LIST(iTermList,iTermPtr);

class iTerm : public iTermList
{
	String	op_name;
	String	sort_name;
	TState	tstate;

	static int 	indent_level;
	static int 	curr_line;

private:

	void Indent (FILE *);

	void ResetIndent (void)
		{ indent_level = 0; }
	
	static char * ScanFile (FILE * infp);

	static iTerm * ParseNode (FILE * fp);

	void PrintNode (FILE * fp);

public:

	iTerm (String op, String ty, long arity);

	iTerm (String op, String ty); 

	iTerm (String op, String ty, iTermPtr t); 

	iTerm (String op, String ty, iTermPtr t1, iTermPtr t2);

	static iTerm * ReadTerm (FILE * fp)
		{ return ParseNode(fp); }

	static String MakeRetrName (String from_sort, String to_sort);

	void WriteTerm (FILE * fp)
		{ PrintNode(fp); }

	BOOL Match (String op, String ty, int arity);

	String GetOp (void)
		{ return op_name; }

	String & GetSort (void)
		{ return sort_name; }

	long GetArity (void)
		{ return Count(); }

	TState & GetTState (void)
		{ return tstate; }

	void FlagNormalForm (void)
		{ tstate = NormalForm; }

	BOOL IsNormalForm (void)
		{ return tstate == NormalForm; }

	void BuildLowest (SortOrder *, OpTable *);

	~ iTerm ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////
// generic cstr for iTerms.

inline
iTerm::iTerm (String op, String s, long arity) : 
	iTermList(arity)
{
	assert(op && s);

	op_name = op;
	sort_name = s;

	tstate = TermForm;
}

/////////////////////////////////////////////////////////////////////////////
// build a k.

inline
iTerm::iTerm (String op, String s) : 
	iTermList(1) // alloc at least one (otherwise C++ complains).
{
	assert(op && s);

	op_name = op;
	sort_name = s;

	tstate = TermForm;
}
	
/////////////////////////////////////////////////////////////////////////////
// build a uop.

inline
iTerm::iTerm (String op, String s, iTermPtr t) : 
	iTermList(1)
{
	assert(op && s);

	op_name = op;
	sort_name = s;

	tstate = TermForm;

	Append(t);
}

/////////////////////////////////////////////////////////////////////////////
// build a bop.

inline
iTerm::iTerm (String op, String s, iTermPtr t1, iTermPtr t2) : 
	iTermList(2)
{
	assert(op && s);

	op_name = op;
	sort_name = s;

	tstate = TermForm;

	Append(t1);
	Append(t2);
}

/////////////////////////////////////////////////////////////////////////////
// *NOTE* depending on the arity this function implements
// the `match.bop' etc. functions in the spec.

inline
BOOL iTerm::Match (String op, String s, int arity)
{
	if (streq(op_name, op) && streq(sort_name, s) && GetArity() == arity)
	{
		return TRUE;
	}	
	else
	{
		return FALSE;
	}
}

/////////////////////////////////////////////////////////////////////////////
// make a stack of iTerm's.

STACK(AuxiTermStack,iTermPtr);

class iTermStack : public AuxiTermStack
{
public:

	iTermStack (long max_size) :
			AuxiTermStack(max_size)
		{;}

	void Print (void);


	~ iTermStack ()
		{;}
};


/////////////////////////////////////////////////////////////////////////////

inline
void iTermStack::Print (void)
{
	for (int i = 0; i < Count(); i++)
	{
		fprintf(stderr, "entry %d:\n", i);
		Get(i)->WriteTerm(stderr);
		fprintf(stderr, "\n");
	}
}
	
/////////////////////////////////////////////////////////////////////////////
// define a variable record for keeping track of variable bindings in
// the environment.

class Var
{
	String	var_name;
	iTerm *	term;
	

public:

	Var ()
		{
			var_name = NULL;
			term = NULL;
		}

	Var (String var, iTerm * t)
		{
			assert(var && t);
			var_name = var;
			term = t;
		}

	String & GetVar (void)
		{ return var_name; }

	iTerm * & GetTerm (void)
		{ return term; }

	~ Var ()
		{;}	
};

/////////////////////////////////////////////////////////////////////////////
// an environment for TRIM variable bindings.

// *NOTE* this implementation is only correct w.r.t the spec
//	  under the assumption that each variable name only occurs 
//	  once in the environment.

LIST(VarList,Var);

class TrimEnv : public VarList
{
public:

	TrimEnv (long max_size) :
			VarList(max_size)
		{;}

	void Assign (String x, iTerm * t)
		{
			Var v(x,t);
			Append(v);
		}

	iTerm * Fetch (String x);

	void operator =(TrimEnv & e);

	void Print (void);

	~ TrimEnv()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

inline
iTerm * TrimEnv::Fetch(String x)
{
	for (int i = 0; i < Count(); i++)
	{
		Var v = Get(i);

		if (streq(x, v.GetVar()))
		{
			return v.GetTerm();
		}
	}

	return NULL;
}
		
/////////////////////////////////////////////////////////////////////////////

inline
void TrimEnv::operator =(TrimEnv & e)
{
	for (int i = 0; i < e.Count(); i++)
	{
		Var v = e.Get(i);

		this->Append(v);
	}

#ifdef DEBUG
	fprintf(stderr, "*** Environment Assignment Operator:\n");
	fprintf(stderr, "RHS Env:\n");
	e.Print();

	fprintf(stderr, "LHS Env:\n");
	this->Print();
#endif
}
		
/////////////////////////////////////////////////////////////////////////////
// Sort List

LIST(AuxSortList,String);

class SortList : public AuxSortList
{
public:

	SortList (long max_size) :
			AuxSortList(max_size)
		{}

	String Find (String s);

	void Gen (FILE * fp)
		{ Print(fp); }

	void Print (FILE * fp);

	~SortList ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

inline
String SortList::Find (String s)
{
        for (int i = 0; i < Count(); i++)
        {
                if (streq(s, Get(i)))
                {
                        return s;
                }
        }

        return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Sort Tuple

class SortTuple
{
	String		sort;
	SortList *	sort_list;

public:

	SortTuple (String s) 
		{
			assert(s);
			sort = s;
			sort_list = mem(new SortList(SORT_LIST_SIZE));
		}

	SortTuple (String s, SortList * sl)
		{
			assert(s && sl);
			sort = s;
			sort_list = sl;
		}

	String GetSortName (void)
		{ return sort; }

	SortList * GetSortList (void)
		{ return sort_list; }

	void Gen (FILE * fp);

	void Print (FILE * fp);

	~SortTuple ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

inline 
void SortTuple::Gen (FILE * fp)
{
	fprintf(fp, "subsorts ");
	GetSortList()->Gen(fp);
	fprintf(fp, "< %s\n", GetSortName());
}

/////////////////////////////////////////////////////////////////////////////

inline 
void SortTuple::Print (FILE * fp)
{
	fprintf(fp, "<< %s,", GetSortName());
	GetSortList()->Print(fp);
	fprintf(fp, " >>\n");
}

/////////////////////////////////////////////////////////////////////////////
// Sort Tuple List

LIST(AuxSortTupleList,SortTuple*);

class SortTupleList : public AuxSortTupleList
{
public:
	SortTupleList (long size) :
			AuxSortTupleList(size)
		{}

	SortTuple * Find (String s);

	void Gen (FILE * fp);

	void Print (FILE * fp);

	~SortTupleList ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

inline
SortTuple * SortTupleList::Find (String s)
{
	for (int i = 0; i < Count(); i++)
	{
		SortTuple * tuple = Get(i);

		if (streq(s, tuple->GetSortName()))
		{
			return tuple;
		}
	}

	return NULL;
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortTupleList::Gen (FILE * fp)
{
	for (int i = 0; i < Count(); i++)
	{
		Get(i)->Gen(fp);
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortTupleList::Print (FILE * fp)
{
	for (int i = 0; i < Count(); i++)
	{
		Get(i)->Print(fp);
	}
}

/////////////////////////////////////////////////////////////////////////////
// the Sort Order

class SortOrder
{
	SortTupleList	sort_tuple_list;

public:

	SortOrder (long max_size) :
			sort_tuple_list(max_size)
		{}

	SortTupleList * GetSortTupleList (void)
		{ return &sort_tuple_list; }

	BOOL InOrder (String s1, String s2);

	BOOL InOrderUnion (String s, SortList * sl);

	SortTuple * FindSortTuple (String s)
		{ return sort_tuple_list.Find(s); }

	SortList * FindSubSorts (SortList * sl);

	void PutSort (String s1, String s2);

	void TransClosure (void);

	void Gen (FILE *);

	void Print (FILE *);

	~SortOrder ()
		{}	
};

/////////////////////////////////////////////////////////////////////////////
// see if sort 's1' is in the sort list of sort 's2'.

inline
BOOL SortOrder::InOrder (String s1, String s2)
{
	SortTuple * st;
	
	if (streq(s1,s2))
	{
		return TRUE;
	}
	else if ((st = this->FindSortTuple(s2)) &&
		 st->GetSortList()->Find(s1))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
BOOL SortOrder::InOrderUnion (String s, SortList * sl)
{
	for (int i = 0; i < sl->Count(); i++)
	{
		String s1 = sl->Get(i);

		if (this->InOrder(s,s1))
		{
			return TRUE;
		}
	}

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// *NOTE* very inefficient memory usage...

inline
SortList * SortOrder::FindSubSorts (SortList * sl)
{
	SortList * l = mem(new SortList(SORT_LIST_SIZE));

	for (int i = 0; i < sl->Count(); i++)
	{
		String sn = sl->Get(i);

		SortTuple * st = this->FindSortTuple(sn);

		if (st)
		{
			l->ListAppend(st->GetSortList());
			l->ListAppend(this->FindSubSorts(st->GetSortList()));
		}
	}

	return l;
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortOrder::PutSort (String s1, String s2)
{
	SortTuple * st = this->FindSortTuple(s2);
	
	if (!st)
	{
		SortTuple * st1 = mem(new SortTuple(s2));

		st1->GetSortList()->Append(s1);
		GetSortTupleList()->Append(st1);
	}
	else
	{
		if (!st->GetSortList()->Find(s1))
		{
			st->GetSortList()->Append(s1);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortOrder::TransClosure (void)
{
	for (int i = 0; i < GetSortTupleList()->Count(); i++)
	{
		SortTuple * st = GetSortTupleList()->Get(i);
		SortList * sl = st->GetSortList();
		
		sl->ListAppend(FindSubSorts(sl));
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortOrder::Gen (FILE * fp)
{
	GetSortTupleList()->Gen(fp);

	fprintf(fp, "\n\n");
}

/////////////////////////////////////////////////////////////////////////////

inline
void SortOrder::Print (FILE * fp)
{
	GetSortTupleList()->Print(fp);
}

/////////////////////////////////////////////////////////////////////////////

class Operator
{
	String		op_name;
	SortList *	arity;
	String		coarity;

public:

	Operator (String on, SortList * ar, String coar)
		{
			op_name = on;
			arity = ar;
			coarity = coar;
		}

	String GetOpName (void)
		{ return op_name; }

	SortList * GetArity (void)
		{ return arity; }

	String GetCoArity (void)
		{ return coarity; }

	void Gen (FILE * fp);

	void Print (FILE * fp);

	~Operator ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

inline 
void Operator::Gen (FILE * fp)
{
	fprintf(fp, "op %s : ", GetOpName());
	GetArity()->Gen(fp);
	fprintf(fp, " -> %s\n", GetCoArity());
}

/////////////////////////////////////////////////////////////////////////////

inline 
void Operator::Print (FILE * fp)
{
	fprintf(fp, "*** operator: %s\n", GetOpName());
	fprintf(fp, "arity (%d): ", (int)GetArity()->Count());
	GetArity()->Print(fp);
	fprintf(fp, "\n");
	fprintf(fp, "coarity: %s\n", GetCoArity());
}

/////////////////////////////////////////////////////////////////////////////

LIST(AuxOpTable,Operator*);

class OpTable : public AuxOpTable
{
public:

	OpTable (long max_size) :
			AuxOpTable(max_size)
		{}

	iTerm * LowestOp (iTerm * t, SortOrder * so);

	void Gen (FILE * fp);

	void Print (FILE * fp);

	~OpTable ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

inline
iTerm * OpTable::LowestOp (iTerm * t, SortOrder * so)
{
	for (int i = 0; i < this->Count(); i++)
	{
		Operator * op = Get(i);

		if (streq(t->GetOp(),op->GetOpName()) &&
		    t->Count() == op->GetArity()->Count() &&
		    so->InOrder(op->GetCoArity(), t->GetSort()))
		{
			// check the children...

			BOOL is_lower = TRUE;

			for (int j = 0; j < t->Count(); j++)
			{
				if (!so->InOrder(op->GetArity()->Get(j),
						t->Get(j)->GetSort()))
				{
					is_lower = FALSE;
					break;
				}
			}

			if (is_lower)
			{
				t->GetSort() = op->GetCoArity();
			}	
		}
	}

	return t;
}

/////////////////////////////////////////////////////////////////////////////

inline
void OpTable::Gen (FILE * fp)
{
	for (int i = 0; i < Count(); i++)
	{
		Get(i)->Gen(fp);
	}

	fprintf(fp, "\n\n");
}

/////////////////////////////////////////////////////////////////////////////

inline
void OpTable::Print (FILE * fp)
{
	for (int i = 0; i < Count(); i++)
	{
		Get(i)->Print(fp);
	}
}

/////////////////////////////////////////////////////////////////////////////
// belongs to iTerm but needs to have seen OpTable.

inline
void iTerm::BuildLowest (SortOrder * so, OpTable * ot)
{
	ot->LowestOp(this,so);
}

/////////////////////////////////////////////////////////////////////////////
// the TRIM machine state.
//
// a state is a tuple where:
//	1) eval stack -- contains the term to be reduced.
//	2) operand stack -- contains rewritten operands to be used in
//			    in the reduction of the next operator.
//	3) work stack -- a scratch area for the machine.
//	4) binding environment.
//	5) the stateregister.
//	6) the `is' register.
//	7) see note...
//	8) the sort order
//	9) the operator table
//
// *NOTE* we use the call instruction for only one thing -- to jump to
//	  the beginning of the module and then to return to where we came
//	  from -- 
//	  therefore, rather than keeping track of the return addresses in the 
//	  state we let the underlying C++ runtime system deal with it -- it is
//	  much more efficient that way.
//
// *NOTE* should the use of call change in any way the appropriate adjustments
//	  need to be made in the state and calling convention.

class State
{
	iTermStack	eval_stack;
	iTermStack	operand_stack;
	iTermStack	work_stack;
	TrimEnv		env;
	StateReg	state_reg;
	BOOL		is_reg;
	SortOrder *  	so;
	OpTable * 	ot;

public:

	State () :
			eval_stack(EVAL_STACK_DEPTH),
			operand_stack(OPERAND_STACK_DEPTH),
			work_stack(WORK_STACK_DEPTH),
			env(ENV_SIZE)
		{ 
			state_reg = Normal;
			is_reg = TRUE;
			so = NULL;
			ot = NULL;
		}

	State (SortOrder * init_so, OpTable * init_ot) :
			eval_stack(EVAL_STACK_DEPTH),
			operand_stack(OPERAND_STACK_DEPTH),
			work_stack(WORK_STACK_DEPTH),
			env(ENV_SIZE)
		{ 
			state_reg = Normal;
			is_reg = TRUE;
			so = init_so;
			ot = init_ot;
		}

	iTermStack * GetEvalStack (void)
		{ return &eval_stack; }

	iTermStack * GetOperandStack (void)
		{ return &operand_stack; }

	iTermStack * GetWorkStack (void)
		{ return &work_stack; }

	TrimEnv * GetEnv (void)
		{ return &env; }

	StateReg & GetStateReg (void)
		{ return state_reg; }

	BOOL & GetIsReg (void)
		{ return is_reg; }

	SortOrder *& GetSortOrder (void)
		{ return so; }

	OpTable *& GetOpTable (void)
		{ return ot; }

	void RLoad (iTerm * t);

	iTerm * RUnload (void);

	void Print (void);

	~ State ()
		{;}
};


/////////////////////////////////////////////////////////////////////////////

inline
void State::RLoad(iTerm * t)
{
	if (t->GetTState() != NormalForm)
	{
		eval_stack.Push(t);
		
		// we push the arguments such that the
		// left outer most is on top -- see 
		// specification.

		for (int i = t->GetArity()-1; i >= 0; i--)
		{
			RLoad(t->Get(i));
		}
	}
	else if (t->GetTState() == NormalForm)
	{
		eval_stack.Push(t);
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
iTerm * State::RUnload(void)
{
	if (eval_stack.Count() == 0)
	{
		return operand_stack.Pop();
	}
	else if (eval_stack.Count() == 1 && operand_stack.Count() == 0)
	{
		// this is the case where the reduced term is at the
		// top of the eval stack.

		return eval_stack.Pop();
	}
	else 
	{
		// the general case -- reconstruct a term from
		// the bits and pieces which exist on the
		// eval stack and the operand stack.

		iTermStack tmpstk(OPERAND_STACK_DEPTH);

		long ar = eval_stack.Top()->GetArity();

		if (ar > operand_stack.Count())
		{
			fprintf(stderr, "fatal -- output `runload' failed\n");
			abort();
		}


		// pop the arguments off the operand stack.

		for (int j = 0; j < ar; j++)
		{
			iTerm * t = operand_stack.Pop();
			tmpstk.Push(t);
		}


		// put the operands into the operator.

		eval_stack.Top()->Clear();

		for (int i = 0; i < ar; i++)
		{
			eval_stack.Top()->Append(tmpstk.Pop());
		}


		// put the term onto the operand_stack.

		operand_stack.Push(eval_stack.Pop());


		// do it recursively.

		return this->RUnload();
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
void State::Print (void)
{
	int i;

	fprintf(stderr, "*** State Dump ***\n");
	
	fprintf(stderr, "eval stack:\n");
	eval_stack.Print();

	fprintf(stderr, "operand stack:\n");
	operand_stack.Print();

	fprintf(stderr, "work stack:\n");
	work_stack.Print();

	fprintf(stderr, "environment:\n");
	env.Print();

	fprintf(stderr, "state reg: %s\n", ConvertStateReg(state_reg));

	fprintf(stderr, "is reg: %d\n", is_reg);
}

/////////////////////////////////////////////////////////////////////////////
// a stack of TRIM states -- needed to deal with conditional equations.

STACK(StateStack, State *);

/////////////////////////////////////////////////////////////////////////////
#endif /* BASICS_HXX */
