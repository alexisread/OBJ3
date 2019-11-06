/////////////////////////////////////////////////////////////////////////////
//  opt.hxx
//
//  the TRIM optimizer.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef OPT_HXX
#define OPT_HXX

#include "const.hxx"
#include "config.hxx"
#include "asmlists.hxx"

/////////////////////////////////////////////////////////////////////////////

extern int             vflag;

/////////////////////////////////////////////////////////////////////////////

class Optimizer
{
	InstrList *	instr_list;
	LabelTable *	label_tab;
	BOOL		global_changes;

private:

	void JumpRetarg (void);

	void Pattern1 (void);

	void Pattern2 (void);

	void Pattern3 (void);

	void Pattern4 (void);

	void Gather (void);

	void RuleBase (void);

	void Compact (void);

	void Labels (void);

public:

	Optimizer (InstrList * l, LabelTable * tab)
		{ 
			instr_list = l;
			label_tab = tab;
			global_changes = FALSE;
		}

	void Run (void);

	~ Optimizer ()
		{;}

};


/////////////////////////////////////////////////////////////////////////////

#endif /* OPT_HXX */
