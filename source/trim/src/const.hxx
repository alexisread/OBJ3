/////////////////////////////////////////////////////////////////////////////
//  const.hxx
//
//  constant definitions for TRIM and related programs.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef CONST_HXX
#define CONST_HXX

#include "config.hxx"

/////////////////////////////////////////////////////////////////////////////

enum TState 
{ 
	NormalForm, 
	TermForm
};

/////////////////////////////////////////////////////////////////////////////

inline 
char * ConvertTState(TState s)
{
	switch(s)
	{
	case NormalForm:
		return "NormalForm";
		break;

	case TermForm:
		return "TermForm";
		break;

	default:
		fprintf(stderr, "fatal -- (%d) no such TState.\n", (int) s);
		abort();
		return NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////

enum StateReg 
{ 
	Normal, 
	Matched,
	Reduced,
	Failure,
	Fatal
};

/////////////////////////////////////////////////////////////////////////////

inline 
char * ConvertStateReg(StateReg r)
{
	switch(r)
	{
	case Normal:
		return "Normal";
		break;

	case Matched:
		return "Matched";
		break;

	case Reduced:
		return "Reduced";
		break;

	case Failure:
		return "Failure";
		break;

	case Fatal:
		return "Fatal";
		break;


	default:
		fprintf(stderr, "fatal -- (%d) no such StateReg.\n", (int) r);
		abort();
		return NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////

enum TrimMode
{
	NONE	= 0x00000000,
	NORMAL 	= 0x00000001,
	MATCHED = 0x00000002,
	REDUCED	= 0x00000004,
	FAILURE = 0x00000008,
	DONE	= 0x00000010,
	NFORM	= 0x00000020
};

/////////////////////////////////////////////////////////////////////////////

inline 
char * ConvertTrimMode(TrimMode m)
{
	switch(m)
	{
	case NONE:
		return "NONE";
		break;

	case NORMAL:
		return "NORMAL";
		break;

	case MATCHED:
		return "MATCHED";
		break;

	case REDUCED:
		return "REDUCED";
		break;

	case FAILURE:
		return "FAILURE";
		break;

	case DONE:
		return "DONE";
		break;

	case NFORM:
		return "NFORM";
		break;


	default:
		fprintf(stderr, "fatal -- (%d) no such TrimMode.\n", (int) m);
		abort();
		return NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////

enum TrimInstr
{
	// Z-Instructions.
	LINK,
	APPLY,
	LOAD,
	RESTORE,
	SAVEOP,
	KILLOP,
	EXIT,
	ABORT,
	PUSHFRAME,
	POPFRAME,

	// Compute instructions.
	MATCH,
	BIND,
	GET,
	BUILD,
	NOP,

	// Mode instructions.
	SET,
	IS,
	PEEK,

	// Jump instructions.
	JUMPT,
	JUMPF,
	JUMP,
	CALL,
	RETURN,

	NO_INSTR
};

/////////////////////////////////////////////////////////////////////////////

inline 
char * ConvertTrimInstr(TrimInstr i)
{
	switch(i)
	{
	// Z-Instructions.
	case LINK:
		return "LINK";
		break;

	case APPLY:
		return "APPLY";
		break;

	case LOAD:
		return "LOAD";
		break;

	case RESTORE:
		return "RESTORE";
		break;

	case SAVEOP:
		return "SAVEOP";
		break;

	case KILLOP:
		return "KILLOP";
		break;

	case EXIT:
		return "EXIT";
		break;

	case ABORT:
		return "ABORT";
		break;

	case PUSHFRAME:
		return "PUSHFRAME";
		break;

	case POPFRAME:
		return "POPFRAME";
		break;

	// Compute instructions.
	case MATCH:
		return "MATCH";
		break;

	case BIND:
		return "BIND";
		break;

	case GET:
		return "GET";
		break;

	case BUILD:
		return "BUILD";
		break;

	case NOP:
		return "NOP";
		break;

	// Mode instructions.
	case SET:
		return "SET";
		break;

	case IS:
		return "IS";
		break;

	case PEEK:
		return "PEEK";
		break;

	// Jump instructions.
	case JUMPT:
		return "JUMPT";
		break;

	case JUMPF:
		return "JUMPF";
		break;

	case JUMP:
		return "JUMP";
		break;

	case CALL:
		return "CALL";
		break;

	case RETURN:
		return "RETURN";
		break;

	case NO_INSTR:
		return "NO_INSTR";
		break;

	default:
		fprintf(stderr, "fatal -- (%d) no such TrimInstr.\n", (int) i);
		abort();
		return NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////

enum EqTreeNodeType
{
	OSMOD,
	MOD,
	EQLIST,
	EQ,
	CEQ,
	LOP,
	LVAR,
	ROP,
	RVAR
};

/////////////////////////////////////////////////////////////////////////////

inline
char * ConvertEqTreeNodeType (EqTreeNodeType i)
{
	switch (i)
	{
	case OSMOD:	return "OSMOD";
	case MOD:	return "MOD";
	case EQLIST:	return "EQLIST";
	case EQ:	return "EQ";
	case CEQ:	return "CEQ";
	case LOP:	return "LOP";
	case LVAR:	return "LVAR";
	case ROP:	return "ROP";
	case RVAR:	return "RVAR";
	default:
		fprintf(stderr, "fatal -- (%d) no such EqTreeNodeType.\n", 
			(int) i);
		abort();
		return NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////
#endif /* CONST_HXX */


