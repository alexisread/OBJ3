/////////////////////////////////////////////////////////////////////////////
//  trim.cxx
//
//  define the actual TRIM machine.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "trim.hxx"

/////////////////////////////////////////////////////////////////////////////

#ifndef INSTR_INLINE
#include "trim_instr.imp"
#endif

/////////////////////////////////////////////////////////////////////////////
// the following instructions are always implemented out-of-line, otherwise
// the optimization phase of the C++ compiler will take too long.

void Trim::link()
{
	iTermStack * 	ek 	= GetState()->GetEvalStack();
	iTermStack * 	ok 	= GetState()->GetOperandStack();
	iTerm * 	ek_top	= ek->Top();
	long 		e_arity	= ek->Top()->GetArity();
	long 		o_count	= ok->Count();

	if (o_count == 0) // no operands available.
	{
		return;
	}

	switch(e_arity)
	{
	case 0:  // k
		{
			return;
		}

	case 1:  // uop
		{
			assert(o_count >= e_arity);

			ek_top->Clear();
			ek_top->GetTState() = TermForm;

			iTerm * t = ok->Pop();

			ek_top->Append(t);
			ek_top->BuildLowest(GetState()->GetSortOrder(),
						    GetState()->GetOpTable());
			return;
		}

	case 2:  // bop
		{
			assert(o_count >= e_arity);

			ek_top->Clear();
			ek_top->GetTState() = TermForm;

			iTerm * t2 = ok->Pop();
			iTerm * t1 = ok->Pop();

			ek_top->Append(t1);
			ek_top->Append(t2);
			ek_top->BuildLowest(GetState()->GetSortOrder(),
					    GetState()->GetOpTable());

			return;
		}

	default:  // arbitrary operators
		{
			assert(o_count >= e_arity);

			// *NOTE* the arguments from the operand stack
			// need to be swapped -- in case you were
			// wondering about the additional complexity
			// in the following code.

			iTermStack tmp_stk(OPERAND_STACK_DEPTH);

			for (int i = 0; i < e_arity; i++)
			{
				tmp_stk.Push(ok->Pop());
			}

			ek_top->Clear();
			ek_top->GetTState() = TermForm;
		
			for (int j = 0; j < e_arity; j++)
			{
				ek_top->Append(tmp_stk.Pop());
			}

			ek_top->BuildLowest(GetState()->GetSortOrder(),
					    GetState()->GetOpTable());
			return;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

void Trim::match (String op, long ar)
{
	iTermStack * wk = GetState()->GetWorkStack();

	String 	wk_op = wk->Top()->GetOp();
	long 	wk_ar = wk->Top()->GetArity();

	BOOL match = (wk_op == op) && (wk_ar == ar);

	if (!match)
	{
		GetState()->GetStateReg() = Failure;
		GetState()->GetIsReg() = FALSE;
		return;
	}

	iTerm * t;

	switch (wk_ar)
	{
	case 0: // k
		wk->Pop();
		GetState()->GetIsReg() = TRUE;
		return;

	case 1: // uop
		t = wk->Pop();
		wk->Push(t->Get(0));
		GetState()->GetIsReg() = TRUE;
		return;

	case 2: // bop
		t = wk->Pop();
		wk->Push(t->Get(1));
		wk->Push(t->Get(0));
		GetState()->GetIsReg() = TRUE;
		return;

	default:
		t = wk->Pop();

		for (int i = t->GetArity()-1; i >= 0; i--)
		{	
			wk->Push(t->Get(i));			
		}

		GetState()->GetIsReg() = TRUE;
		return;
	}
}

/////////////////////////////////////////////////////////////////////////////

void Trim::build (String op, String ty, long ar)
{
	iTermStack * ek = GetState()->GetEvalStack();
	iTerm * t;
	iTerm * new_t;

	switch(ar)
	{
	case 0: // k
		{
			new_t = mem(new iTerm(op, ty));
			ek->Push(new_t);
			new_t->BuildLowest(GetState()->GetSortOrder(),
					   GetState()->GetOpTable());  
			return;
		}

	case 1: // uop
		{
			assert(ek->Count() >= ar); // has to be here

			t = ek->Pop();
			new_t = mem(new iTerm(op, ty, t));
			new_t->BuildLowest(GetState()->GetSortOrder(),
						   GetState()->GetOpTable());  
			ek->Push(new_t);
			return;
		}


	case 2: // bop
		{
			assert(ek->Count() >= ar); // has to be here

			iTerm * t2 = ek->Pop();
			iTerm * t1 = ek->Pop();

			new_t = mem(new iTerm(op, ty, t1, t2));

			new_t->BuildLowest(GetState()->GetSortOrder(),
					   GetState()->GetOpTable());  

			ek->Push(new_t);
			return;
		}

	default:
		{
			assert(ek->Count() >= ar); // has to be here

			// *NOTE* the arguments from the eval stack
			// need to be swapped -- in case you were
			// wondering about the additional complexity
			// in the following code.


			iTermStack tmp_stk(OPERAND_STACK_DEPTH);

			for (int i = 0; i < ar; i++)
			{
				tmp_stk.Push(ek->Pop());
			}

			new_t = mem(new iTerm(op, ty, ar));

			for (int j = 0; j < ar; j++)
			{
				new_t->Append(tmp_stk.Pop());
			}

			new_t->BuildLowest(GetState()->GetSortOrder(),
					   GetState()->GetOpTable());  

			ek->Push(new_t);

			return;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

void Trim::is (TrimMode m)
{
	iTermStack * ek; 
	iTermStack * ok;
	BOOL b;

	StateReg sreg = GetState()->GetStateReg();

	switch(m)
	{
	case DONE:

		ek = GetState()->GetEvalStack();
		ok = GetState()->GetOperandStack();
		b = ((sreg == Failure || 
		      ek->Top()->GetTState() == NormalForm) &&
		     (ek->Count() == 1) &&
		     (ok->Count() == 0));

		if (b)
		{
			GetState()->GetIsReg() = TRUE;
			return;
		}
		else 
		{
			GetState()->GetIsReg() = FALSE;
			return;
		}
		break;

	case NFORM:

		ek = GetState()->GetEvalStack();
		ok = GetState()->GetOperandStack();
		b = ek->Top()->IsNormalForm();

		if (ek->Count() && b)
		{
			GetState()->GetIsReg() = TRUE;
			return;
		}
		else if (ek->Count() && !b)
		{
			GetState()->GetIsReg() = FALSE;
			return;
		}
		break;

	default:

		if (sreg == InterpretMode(m))
		{
			GetState()->GetIsReg() = TRUE;
			return;
		}	
		else
		{
			GetState()->GetIsReg() = FALSE;
			return;
		}	
		break;
	}

	// *NOTE* if we got here we have a problem.
	fprintf(stderr, "fatal -- error in IS code.\n");
	abort();
}

/////////////////////////////////////////////////////////////////////////////

void Trim::Run (void)
{
	GetState()->RLoad(iTerm::ReadTerm(stdin));

	// run the user program.

	Program();

#ifdef OBJ_OUTPUT
	fprintf(stdout, "parse ");
#endif

	GetState()->RUnload()->WriteTerm(stdout);

#ifdef OBJ_OUTPUT
	fprintf(stdout, " .");
#endif
	fprintf(stdout, "\n");
}

/////////////////////////////////////////////////////////////////////////////

