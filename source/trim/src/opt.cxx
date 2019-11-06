/////////////////////////////////////////////////////////////////////////////
//  opt.cxx
//
//  the TRIM optimizer (implementations).
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "opt.hxx"

/////////////////////////////////////////////////////////////////////////////
// jump retargeting:
//
//	IS FAILURE		IS FAILURE
//	JUMPT L1		JUMPT L2  <<==
//	 ...			 ...
//  L1: IS FAILURE	=> L1:	IS FAILURE
//	JUMPT L2		JUMPT L2
//	 ...			 ...
//  L2: Instr		   L2:  Instr

void Optimizer::JumpRetarg (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 2; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr00;
			Instr * instr01;
		
			// match the pattern...

			if (instr0->GetInstr() == IS &&
			    instr1->GetInstr() == JUMPT &&
			    streq(instr0->GetOperand(0),STRING("FAILURE")))
			{
				String L1 = instr1->GetOperand(0);
				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);

				// find the next pair of instructions.

				instr00 = r1->GetDefPoint();
				long ix_01 = instr_list->FindElem(instr00) + 1;
				instr01 = instr_list->Get(ix_01);

				if (instr00->GetInstr() == IS &&
				    instr01->GetInstr() == JUMPT &&
				    streq(instr00->GetOperand(0),
					  STRING("FAILURE")))
				{
					// retarget jump

					String L2 = instr01->GetOperand(0);
					LabelRecord * r2 = 
						label_tab->FindLabelRecord(L2);

					r1->GetRefPoints()->DeleteElem(instr1);
					r2->GetRefPoints()->Append(instr1);

					instr1->SetOperand(L2);

					if (vflag)
					{
						fprintf(stderr, 
							"retargeting jump "
							"(%s:%d): %s -> %s\n",
							instr0->GetFileName(),
							instr0->GetLine(),
							L1,
							L2);
					}

					global_changes = TRUE;
					local_changes = TRUE;

					// start from the top.

					break;
				}
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// pattern 1:
//
//	IS FAILURE	
//	JUMPT L1      		IS FAILURE
//	IS FAILURE	=>	JUMPT L1
//	JUMPT L2		  ...
//	  ...


void Optimizer::Pattern1 (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 4; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr2 = instr_list->Get(i+2);
			Instr * instr3 = instr_list->Get(i+3);
		
			// match the pattern...

			if (instr0->GetInstr() == IS &&
			    instr1->GetInstr() == JUMPT &&
			    instr2->GetInstr() == IS &&
			    instr3->GetInstr() == JUMPT &&
			    streq(instr0->GetOperand(0),STRING("FAILURE")) &&
			    streq(instr2->GetOperand(0),STRING("FAILURE")) &&
			    // cannot be targets of jumps.
			    instr2->GetLabel() == NULL &&
			    instr3->GetLabel() == NULL)
			{
				// we want to kill 'instr2' and 'instr3' --
				// in order to do that we must get rid
				// of the label ref for 'L2' in the label
				// table.

				String l = instr3->GetOperand(0);

				LabelRecord * r = 
					label_tab->FindLabelRecord(l);

				r->GetRefPoints()->DeleteElem(instr3);


				// kill the instructions.

				if (vflag)
				{
					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr2->GetFileName(),
						instr2->GetLine());
					instr2->Gen(stderr);

					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr3->GetFileName(),
						instr3->GetLine());
					instr3->Gen(stderr);
				}

				instr_list->DeleteElem(instr2);
				instr_list->DeleteElem(instr3);

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.

				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// pattern 2:
//
//	RESTORE			
//	IS FAILURE	=>	RESTORE
//	JUMPT L1

void Optimizer::Pattern2 (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 3; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr2 = instr_list->Get(i+2);
		
			// match the pattern...

			if (instr0->GetInstr() == RESTORE &&
			    instr1->GetInstr() == IS &&
			    instr2->GetInstr() == JUMPT &&
			    streq(instr1->GetOperand(0),STRING("FAILURE")) &&
			    // cannot be targets of jumps.
			    instr1->GetLabel() == NULL &&
			    instr2->GetLabel() == NULL)
			{
				// we want to kill 'instr1' and 'instr2' --
				// in order to do that we must get rid
				// of the label ref for 'L1' in the label
				// table.

				String l = instr2->GetOperand(0);

				LabelRecord * r = 
					label_tab->FindLabelRecord(l);

				r->GetRefPoints()->DeleteElem(instr2);


				// kill the instructions.

				if (vflag)
				{
					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr1->GetFileName(),
						instr1->GetLine());
					instr1->Gen(stderr);

					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr2->GetFileName(),
						instr2->GetLine());
					instr2->Gen(stderr);
				}

				instr_list->DeleteElem(instr1);
				instr_list->DeleteElem(instr2);

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.

				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// pattern 3:
//
//	APPLY ;			APPLY ;
//  L1 :			JUMP L2 ;
//	IS REDUCED ;	=>  L1 :
//	JUMPT L2 ;		RESTORE ;
//	RESTORE ;

void Optimizer::Pattern3 (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 4; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr2 = instr_list->Get(i+2);
			Instr * instr3 = instr_list->Get(i+3);
		
			// match the pattern...

			if (instr0->GetInstr() == APPLY &&
			    instr1->GetInstr() == IS &&
			    instr2->GetInstr() == JUMPT &&
			    instr3->GetInstr() == RESTORE &&
			    streq(instr1->GetOperand(0),STRING("REDUCED")) &&
			    instr1->GetLabel() != NULL &&
			    instr2->GetLabel() == NULL)
			{
				String L1 = instr1->GetLabel();
				String L2 = instr2->GetOperand(0);

				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);
				LabelRecord * r2 = 
					label_tab->FindLabelRecord(L2);

				// make a 'JUMP' instr -- hook it into the
				// list right after the 'APPLY'
				Instr * new_instr = mem(new JumpInstr(L2));
				instr_list->AppendElem(instr0, new_instr);
			
				// retarget def-point of L1
				r1->GetDefPoint() = instr3;
				instr3->GetLabel() = L1;

				// clean up the ref-points for L2
				r2->GetRefPoints()->DeleteElem(instr2);
				r2->GetRefPoints()->Append(new_instr);
				
				// kill the instructions.

				if (vflag)
				{
					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr1->GetFileName(),
						instr1->GetLine());
					instr1->Gen(stderr);

					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr2->GetFileName(),
						instr2->GetLine());
					instr2->Gen(stderr);
				}

				instr_list->DeleteElem(instr1);
				instr_list->DeleteElem(instr2);

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.

				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// pattern 4:
//
//	{ MATCH | BIND }	{ MATCH | BIND }
//	IS FAILURE	   =>	JUMPF L1
//	JUMPT L1		

void Optimizer::Pattern4 (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 3; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr2 = instr_list->Get(i+2);
		
			// match the pattern...

			if ((instr0->GetInstr() == MATCH ||
			     instr0->GetInstr() == BIND) &&
			    instr1->GetInstr() == IS &&
			    instr2->GetInstr() == JUMPT &&
			    streq(instr1->GetOperand(0),STRING("FAILURE")) &&
			    instr1->GetLabel() == NULL &&
			    instr2->GetLabel() == NULL)
			{
				String L1 = instr2->GetOperand(0);

				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);

				// make a 'JUMPF' instr -- hook it into the
				// list right after instr0
				Instr * new_instr = mem(new JumpFInstr(L1));
				instr_list->AppendElem(instr0, new_instr);
			
				// clean up the ref-points for L1
				r1->GetRefPoints()->DeleteElem(instr2);
				r1->GetRefPoints()->Append(new_instr);
				
				// kill the instruction
				if (vflag)
				{
					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr1->GetFileName(),
						instr1->GetLine());
					instr1->Gen(stderr);

					fprintf(stderr, 
						"deleting instruction "
						"(%s:%d): ",
						instr2->GetFileName(),
						instr2->GetLine());
					instr2->Gen(stderr);
				}

				instr_list->DeleteElem(instr1);
				instr_list->DeleteElem(instr2);

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.
				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// gather:
//
//	JUMPT L1		JUMPT L1
//	 ...			 ...
//	JUMPT L2	=>	JUMPT L1
//	 ...			 ...
//   L1:  NOP		    L1: NOP
//   L2:  Instr			Instr    

void Optimizer::Gather (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 2; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
		
			// match the pattern...

			if (instr0->GetInstr() == NOP &&
			    instr0->GetLabel() != NULL &&
			    instr1->GetLabel() != NULL)
			{
				// rewrite all the label refs for 'L2' 
				// to 'L1' in the label table 
				// and in the code stream.

				String L1 = instr0->GetLabel();
				String L2 = instr1->GetLabel();

				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);
				LabelRecord * r2 = 
					label_tab->FindLabelRecord(L2);

				if (vflag)
				{
					fprintf(stderr, 
						"gathering on label (%s:%d): "
						"%s\n",
						instr0->GetFileName(),
						instr0->GetLine(),
						L1);
				}

				// copy the ref-points from L2 to L1 and
				// rewrite the label names in the instructions.

				for (long j = 0; 
				     j < r2->GetRefPoints()->Count();
				     j++)
				{
					Instr * instr = 
						r2->GetRefPoints()->Get(j);
					instr->SetOperand(L1);
					r1->GetRefPoints()->Append(instr);
				}

				r2->GetRefPoints()->Clear();

				// kill the label definition.

				r2->GetDefPoint() = LABEL_NOT_DEFINED;
				instr1->GetLabel() = NULL;

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.

				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// rule-base:
//
//	RESTORE			RESTORE
//	MATCH op1 ar1		MATCH op1 ar1
//	JUMPF L1		JUMPF L2 <<==
//	 ...			 ...
// L1:			=> L1:			if op1 == op2 && 
//	RESTORE			RESTORE		   ar1 == ar2 &&
//	MATCH op2 ar2		MATCH op2 ar2	   L1 =/= L2
//	JUMPF L2		JUMPF L2
//
// *NOTE* the assumption is that the compiler front-end outputs all the
//	  equations with the same toplevel lhs op in a cluster...

void Optimizer::RuleBase (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 3; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
			Instr * instr2 = instr_list->Get(i+2);
			Instr * instr00;
			Instr * instr01;
			Instr * instr02;
		
			// match the pattern...

			if (instr0->GetInstr() == RESTORE &&
			    instr1->GetInstr() == MATCH &&
			    instr2->GetInstr() == JUMPF)
			{
				String L1 = instr2->GetOperand(0);
				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);

				// find the next set of instructions.

				instr00 = r1->GetDefPoint();
				long ix_01 = instr_list->FindElem(instr00) + 1;
				instr01 = instr_list->Get(ix_01);
				long ix_02 = instr_list->FindElem(instr00) + 2;
				instr02 = instr_list->Get(ix_02);

				if (instr00->GetInstr() == RESTORE &&
				    instr01->GetInstr() == MATCH &&
				    instr02->GetInstr() == JUMPF &&
				    streq(instr1->GetOperand(0),
					  instr01->GetOperand(0)) &&
			   	    instr1->GetArity() == 
					instr01->GetArity() &&
				    !streq(instr2->GetOperand(0),
					   instr02->GetOperand(0)))
				{
					// retarget jump

					String L2 = instr02->GetOperand(0);
					LabelRecord * r2 = 
						label_tab->FindLabelRecord(L2);

					r1->GetRefPoints()->DeleteElem(instr2);
					r2->GetRefPoints()->Append(instr2);

					instr2->SetOperand(L2);

					if (vflag)
					{
						fprintf(stderr, 
							"rule base opt: "
							"(%s:%d): %s -> %s\n",
							instr0->GetFileName(),
							instr0->GetLine(),
							L1,
							L2);
					}

					global_changes = TRUE;
					local_changes = TRUE;

					// start from the top.

					break;
				}
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// compact:
//
//   L1:  NOP      =>    L1:  Instr
//        Instr

void Optimizer::Compact (void)
{
	BOOL local_changes;

	do
	{
		local_changes = FALSE;
		long cnt = instr_list->Count();

		for (long i = 0; 
		     i < cnt && (cnt - i + 1) > 2; 
		     i++)
		{
			Instr * instr0 = instr_list->Get(i);
			Instr * instr1 = instr_list->Get(i+1);
		
			// match the pattern...

			if (instr0->GetInstr() == NOP &&
			    instr0->GetLabel() != NULL &&
			    instr1->GetLabel() == NULL)
			{
				String L1 = instr0->GetLabel();

				LabelRecord * r1 = 
					label_tab->FindLabelRecord(L1);

				if (vflag)
				{
					fprintf(stderr, 
						"compacting label (%s:%d): "
						"%s\n",
						instr0->GetFileName(),
						instr0->GetLine(),
						L1);
				}

				// kill the instruction.

				r1->GetDefPoint() = instr1;
				instr1->GetLabel() = L1;

				instr_list->DeleteElem(instr0);

				global_changes = TRUE;
				local_changes = TRUE;

				// start from the top.

				break;
			}
		}
	}
	while (local_changes);
}

/////////////////////////////////////////////////////////////////////////////
// labels: remove unused label definitions from the code stream.

void Optimizer::Labels (void)
{
	long cnt = instr_list->Count();

	for (long i = 0; i < cnt; i++)
	{
		Instr * instr = instr_list->Get(i);
		
		if (instr->GetLabel() != NULL)
		{
			String l 	= instr->GetLabel();
			LabelRecord * r	= label_tab->FindLabelRecord(l);


			// make sure we are looking at the right label.

			assert(r->GetDefPoint() == instr);


			// kill the label.

			if (r->GetRefPoints()->Count() == 0)
			{
				if (vflag)
				{
					fprintf(stderr, 
						"deleting label (%s:%d): %s\n",
						instr->GetFileName(),
						instr->GetLine(),
						instr->GetLabel());
				}

				r->GetDefPoint() = LABEL_NOT_DEFINED;
				instr->GetLabel() = NULL;

				global_changes = TRUE;
			}

		}
	}
}

/////////////////////////////////////////////////////////////////////////////

void Optimizer::Run (void)
{
	do
	{
		global_changes = FALSE;

		JumpRetarg();
		Pattern1();
		Pattern2();
		Pattern3();
		// *NOTE* don't do it here, it detroys contexts for
		// other peep-hole optimizations in this phase.
		// Pattern4(); 
		Gather();
		Compact();
		Labels();
	} 
	while (global_changes);

	do
	{
		global_changes = FALSE;

		JumpRetarg();
		Pattern1();
		Pattern2();
		Pattern3();
		Pattern4();
		Gather();
		RuleBase();
		Compact();
		Labels();
	} 
	while (global_changes);
}

/////////////////////////////////////////////////////////////////////////////
