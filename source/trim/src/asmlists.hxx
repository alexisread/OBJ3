/////////////////////////////////////////////////////////////////////////////
//  asmlists.hxx
//
//  basic list definitions for TRIM assembler.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef ASMLISTS_HXX
#define ASMLISTS_HXX

#include "const.hxx"
#include "config.hxx"
#include "basics.hxx"

/////////////////////////////////////////////////////////////////////////////
// functions from the scanner and parser.

// the following stuff is needed for error reporting.
int scanner_line (void);
char * scanner_filename (void);
extern int errors;
extern FILE *yyin;

int yyparse();
int yylex();

/////////////////////////////////////////////////////////////////////////////

extern int gflag;
extern int errors;
extern class LabelTable *    label_tab;

/////////////////////////////////////////////////////////////////////////////

class Instr;
class InstrList;

/////////////////////////////////////////////////////////////////////////////
// this is an error token we return if we have to return something
// by reference.

static long error_token = ERROR;

/////////////////////////////////////////////////////////////////////////////

class AsmSortOrder : public SortOrder
{
public:

	AsmSortOrder (long max_size) :
			SortOrder(max_size)
		{}

	void Write (FILE * fp);

	~AsmSortOrder ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

class AsmOpTable : public OpTable
{
public:

	AsmOpTable (long max_size) :
			OpTable(max_size)
		{}

	void Write (FILE * fp, AsmSortOrder * so);

	~AsmOpTable ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

typedef	Instr *	LabelAddr;

LLIST(LabelAddrList, LabelAddr);

class LabelRecord 
{
	String		label;
	LabelAddr	def_point;
	LabelAddrList	ref_points;

public:

	LabelRecord () :
			ref_points(MAX_LABEL_REFS)
		{ 
			label = NULL;
			def_point = LABEL_NOT_DEFINED; 
		}

	String & GetLabel (void)
		{ return label; }

	LabelAddr & GetDefPoint (void)
		{ return def_point; }

	LabelAddrList * GetRefPoints (void)
		{ return &ref_points; }

	void Print (FILE * fp);

	~ LabelRecord ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

LIST(LabelRecordList, LabelRecord);

class LabelTable : public LabelRecordList
{
public:

	LabelTable (long size) :
			LabelRecordList(size)
		{;}

	LabelRecord * FindLabelRecord (String label);

	LabelRecord * FindLabelRecord (LabelAddr instr_ix);

	void LabelDef (String label, LabelAddr instr_ix);

	void LabelRef (String label, LabelAddr instr_ix);

	void CheckInstrs (InstrList * instrs);

	void Print (FILE * fp);

	~ LabelTable ()
		{;}
		
};

/////////////////////////////////////////////////////////////////////////////
// this is what an abstract TRIM instruction looks like to the assembler.

class Instr
{
	TrimInstr	instr;
	String		label_def;
	String		file_name;
	int		line;

public:

	Instr (TrimInstr i)
		{ 
			instr = i; 
			label_def = NULL; 
			file_name = scanner_filename();
			line = scanner_line();
		}

	TrimInstr GetInstr (void)
		{ return instr; }

	String & GetLabel (void)
		{ return label_def; }

	String GetFileName (void)
		{ return file_name; }

	int GetLine (void)
		{ return line; }

	void WriteLabel (FILE * fp)
		{ if (label_def) fprintf(fp, "%s :\n", label_def); }

	virtual void SetOperand (String)
		{ 
			fprintf(stderr, "fatal -- illegal operation.\n"); 
			abort(); 
		}

	virtual String GetOperand (long i) = 0;

	virtual long & GetIxOperand (long i) = 0;

	virtual long GetArity (void) = 0;

	virtual void Translate (FILE * fp) = 0;

	virtual long CountStringArgs (void) = 0;

	virtual void Gen (FILE * fp) = 0;
				
	virtual void Print (FILE * fp) = 0;
				
	virtual ~ Instr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class LinkInstr : public Instr
{
public:

	LinkInstr () :
			Instr(LINK)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tlink();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tLINK ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"LINK\")\n"); }
				
	~ LinkInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class ApplyInstr : public Instr
{
public:

	ApplyInstr () :
			Instr(APPLY)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tapply();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tAPPLY ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"APPLY\")\n"); }
				
	~ ApplyInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class LoadInstr : public Instr
{
public:

	LoadInstr () :
			Instr(LOAD)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tload();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tLOAD ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"LOAD\")\n"); }
				
	~ LoadInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class RestoreInstr : public Instr
{
public:

	RestoreInstr () :
			Instr(RESTORE)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\trestore();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tRESTORE ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"RESTORE\")\n"); }
				
	~ RestoreInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class SaveOpInstr : public Instr
{
public:

	SaveOpInstr () :
			Instr(SAVEOP)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tsaveop();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tSAVEOP ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"SAVEOP\")\n"); }
				
	~ SaveOpInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class KillOpInstr : public Instr
{
public:

	KillOpInstr () :
			Instr(KILLOP)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tkillop();\n"); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tKILLOP ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"KILLOP\")\n"); }
				
	~ KillOpInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class ExitInstr : public Instr
{
public:

	ExitInstr () :
			Instr(EXIT)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\texit(0);\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tEXIT ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"EXIT\")\n"); }
				
	~ ExitInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class AbortInstr : public Instr
{
public:

	AbortInstr () :
			Instr(ABORT)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tGetState()->GetStateReg() = Fatal;\n");
			fprintf(fp, "\tabort();\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{
			WriteLabel(fp);
			fprintf(fp,"\tABORT ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"ABORT\")\n"); }
				
	~ AbortInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class PushFrameInstr : public Instr
{
public:

	PushFrameInstr () :
			Instr(PUSHFRAME)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tpushframe();\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tPUSHFRAME ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"PUSHFRAME\")\n"); }
				
	~ PushFrameInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class PopFrameInstr : public Instr
{
public:

	PopFrameInstr () :
			Instr(POPFRAME)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tpopframe();\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tPOPFRAME ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"POPFRAME\")\n"); }
				
	~ PopFrameInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class MatchInstr : public Instr
{
	String	string_op;
	long	str_ix_op;
	long	ar;

public:

	MatchInstr (String op, long arity) :
			Instr(MATCH)
		{
			assert(op);

			string_op = op;
			ar = arity;
		}

	String GetOperand (long i)
		{ 
			assert(0 == i);

			return string_op;
		}

	long & GetIxOperand (long i)
		{
			assert(0 == i);

			return str_ix_op;
		}

	long GetArity (void)
		{ return ar; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tmatch(s%d, %d);\n", 
				str_ix_op, 
				ar); 
		}

	long CountStringArgs (void)
		{ return 1; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tMATCH %s %d ;\n", 
				string_op, 
				ar); 
		}
				
	void Print (FILE * fp)
		{ 
			fprintf(fp, "INSTR(\"MATCH %s %d\")\n", 
				string_op, 
				ar); 
		}
				
	~ MatchInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class BindInstr : public Instr
{
	String		string_op[2]; // 0 - varname, 1 - sortname
	long 		str_ix_op[2]; // 0 - varname, 1 - sortname

public:

	BindInstr (String varname, String sortname) :
			Instr(BIND)
		{
			assert(varname && sortname);

			string_op[0] = varname;
			string_op[1] = sortname;
		}

	String GetOperand (long i)
		{ 
			assert(0 <= i && i < 2);

			return string_op[i];
		}

	long & GetIxOperand (long i)
		{
			assert(0 <= i && i < 2);

			return str_ix_op[i];
		}

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tbind(s%d, s%d);\n", 
				str_ix_op[0], 
				str_ix_op[1]);
		}

	long CountStringArgs (void)
		{ return 2; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tBIND %s [ %s ] ;\n", 
				string_op[0], string_op[1]);
		}
				
	void Print (FILE * fp)
		{ 
			fprintf(fp, "INSTR(\"BIND %s [ %s ]\")\n", 
				string_op[0], string_op[1]);
		}
				
	~ BindInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class GetInstr : public Instr
{
	String	string_op; // varname
	long	str_ix_op;

public:

	GetInstr (String op) :
			Instr(GET)
		{
			assert(op);

			string_op = op;
		}

	String GetOperand (long i)
		{ 
			assert(0 == i);

			return string_op;
		}

	long & GetIxOperand (long i)
		{
			assert(0 == i);

			return str_ix_op;
		}

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tget(s%d);\n", str_ix_op);
		}

	long CountStringArgs (void)
		{ return 1; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tGET %s ;\n", string_op);
		}
				
	void Print (FILE * fp)
		{ 
			fprintf(fp, "INSTR(\"GET %s\")\n", string_op);
		}
				
	~ GetInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class BuildInstr : public Instr
{
	String	string_op [2]; // 0 - op, 1 - ty
	long	str_ix_op [2];
	long	ar;

public:

	BuildInstr (String op0, String op1, long arity) :
			Instr(BUILD)
		{
			assert(op0 && op1);

			string_op[0] = op0;
			string_op[1] = op1;
			ar = arity;
		}

	String GetOperand (long i)
		{ 
			assert(0 <= i && i < 2);

			return string_op[i];
		}

	long & GetIxOperand (long i)
		{
			assert(0 <= i && i < 2);

			return str_ix_op[i];
		}

	long GetArity (void)
		{ return ar; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tbuild(s%d, s%d, %d);\n", 
				str_ix_op[0], 
				str_ix_op[1], 
				ar); 
		}

	long CountStringArgs (void)
		{ return 2; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tBUILD %s %s %d ;\n", 
				string_op[0], 
				string_op[1], 
				ar); 
		}
				
	void Print (FILE * fp)
		{ 
			fprintf(fp, "INSTR(\"BUILD %s %s %d\")\n", 
				string_op[0], 
				string_op[1], 
				ar); 
		}
				
	~ BuildInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class NopInstr : public Instr
{
public:

	NopInstr () :
			Instr(NOP)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\t;\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tNOP ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"NOP\")\n"); }
				
	~ NopInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class SetInstr : public Instr
{
	TrimMode	m;
public:

	SetInstr (TrimMode mode) :
			Instr(SET)
		{
			m = mode;

			if (m & (DONE|NFORM))
			{
				fprintf(stderr,
					"error -- "
					"mode field `%s' illegal in this "
					"context.\n",
					ConvertTrimMode(m));
				errors++;
			}
		}

	String GetOperand (long i)
		{ assert(i == 0); return STRING(ConvertTrimMode(m)); }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tset(%s);\n", ConvertTrimMode(m)); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tSET %s ;\n", ConvertTrimMode(m)); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"SET %s\")\n", ConvertTrimMode(m)); }
				
	~ SetInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class IsInstr : public Instr
{
	TrimMode	m;
public:

	IsInstr (TrimMode mode) :
			Instr(IS)
		{ m = mode; }

	String GetOperand (long i)
		{ assert(i == 0); return STRING(ConvertTrimMode(m)); }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tis(%s);\n", ConvertTrimMode(m)); 
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tIS %s ;\n", ConvertTrimMode(m)); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"IS %s\")\n", ConvertTrimMode(m)); }
				
	~ IsInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class PeekInstr : public Instr
{
	String	string_op [2]; // 0 - op, 1 - ty
	long	str_ix_op [2];
	long	ar;

public:

	PeekInstr (String op0, String op1, long arity) :
			Instr(PEEK)
		{
			assert(op0 && op1);

			string_op[0] = op0;
			string_op[1] = op1;
			ar = arity;
		}

	String GetOperand (long i)
		{ 
			assert(0 <= i && i < 2);

			return string_op[i];
		}

	long & GetIxOperand (long i)
		{
			assert(0 <= i && i < 2);

			return str_ix_op[i];
		}

	long GetArity (void)
		{ return ar; }

	void Translate (FILE * fp)
		{ 
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tpeek(s%d, s%d, %d);\n", 
				str_ix_op[0], 
				str_ix_op[1], 
				ar); 
		}

	long CountStringArgs (void)
		{ return 2; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tPEEK %s %s %d ;\n", 
				string_op[0], 
				string_op[1], 
				ar); 
		}
				
	void Print (FILE * fp)
		{ 
			fprintf(fp, "INSTR(\"PEEK %s %s %d\")\n", 
				string_op[0], 
				string_op[1], 
				ar); 
		}
				
	~ PeekInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class JumpTInstr : public Instr
{
	String	label_ref;

public:

	JumpTInstr (String label) :
			Instr(JUMPT)
		{ assert(label); label_ref = label; }

	void SetOperand (String s)
		{ label_ref = s; }

	String GetOperand (long i)
		{ assert(i == 0); return label_ref; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tif (GetState()->GetIsReg()) goto %s;\n",
					label_ref);
			// *NOTE* we diverge here from the specification --
			// we do this because we know that we always test
			// with an 'IS','MATCH', or 'BIND' instructions
			// before we 'JUMP' -- if that wouldn't be the case
			// would not be allowed to take the reinit statement
			// out.
			// fprintf(fp, "\tGetState()->GetIsReg() = TRUE;\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tJUMPT %s ;\n", label_ref); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"JUMPT %s\")\n", label_ref); }
				
	~ JumpTInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class JumpFInstr : public Instr
{
	String	label_ref;

public:

	JumpFInstr (String label) :
			Instr(JUMPF)
		{ assert(label); label_ref = label; }

	void SetOperand (String s)
		{ label_ref = s; }

	String GetOperand (long i)
		{ assert(i == 0); return label_ref; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tif (!GetState()->GetIsReg())\n");
                        fprintf(fp, "\t{\n");

			// *NOTE* we diverge here from the specification --
			// we do this because we know that we always test
			// with an 'IS','MATCH', or 'BIND' instructions
			// before we 'JUMP' -- if that wouldn't be the case
			// would not be allowed to take the reinit statement
			// out.
			// fprintf(fp, "\t\tGetState()->GetIsReg() = TRUE;\n");

			fprintf(fp, "\t\tgoto %s;\n", label_ref);
                        fprintf(fp, "\t}\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tJUMPF %s ;\n", label_ref); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"JUMPF %s\")\n", label_ref); }
				
	~ JumpFInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class JumpInstr : public Instr
{
	String	label_ref;

public:

	JumpInstr (String label) :
			Instr(JUMP)
		{ assert(label); label_ref = label; }

	void SetOperand (String s)
		{ label_ref = s; }

	String GetOperand (long i)
		{ assert(i == 0); return label_ref; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\tgoto %s;\n", label_ref);
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tJUMP %s ;\n", label_ref); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"JUMP %s\")\n", label_ref); }
				
	~ JumpInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////
// *NOTE* currently we only support one form of call statement and
//	  that is a call to the program itself -- any other call
//	  will be flagged as an error.

class CallInstr : public Instr
{
	String	label_ref;

public:

	CallInstr (String label) :
			Instr(CALL)
		{ assert(label); label_ref = label; }

	void SetOperand (String s)
		{ label_ref = s; }

	String GetOperand (long i)
		{ assert(i == 0); return label_ref; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			
			// only calls to the top of the module are allowed...

			LabelRecord * l = 
				label_tab->FindLabelRecord(label_ref);

			if (!l)
			{
				fprintf(stderr,
					"error -- "
					"call to undefined label `%s'\n.",
					label_ref);
				errors++;
				return;
			}

			LabelAddr l_def = l->GetDefPoint();

			if (l && l_def != 0)
			{
				fprintf(stderr,
					"error -- "
					"call to label `%s' not supported.\n",
					label_ref);
				errors++;
				return;
			}
			
			fprintf(fp, "\tProgram();\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tCALL %s ;\n", label_ref); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"CALL %s\")\n", label_ref); }
				
	~ CallInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class ReturnInstr : public Instr
{
public:

	ReturnInstr () :
			Instr(RETURN)
		{;}

	String GetOperand (long)
		{ return NULL; }

	long & GetIxOperand (long)
		{ return error_token = IX_OPERAND_NOT_DEFINED; }

	long GetArity (void)
		{ return error_token = ARITY_NOT_DEFINED; }

	void Translate (FILE * fp)
		{
			if (gflag)
			{
				Print(fp);
			}

			fprintf(fp, "\treturn;\n");
		}

	long CountStringArgs (void)
		{ return 0; }

	void Gen (FILE * fp)
		{ 
			WriteLabel(fp);
			fprintf(fp, "\tRETURN ;\n"); 
		}
				
	void Print (FILE * fp)
		{ fprintf(fp, "INSTR(\"RETURN\")\n"); }
				
	~ ReturnInstr ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////
// this is the internal representation of a TRIM program the way the
// optimzer and the assembler see it.

typedef Instr * InstrPtr;

LLIST(AuxInstrList, InstrPtr);

class InstrList : public AuxInstrList
{
public:
	
	InstrList (long sz) :
			AuxInstrList(sz)
		{;}

	void Gen (FILE * fp)
		{
			for (int i = 0; i < Count(); i++)
			{
				Instr * instr = Get(i);
				instr->Gen(fp);
			}
		}
	
	~ InstrList ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////
// the instruction handler converts an instruction list into 
// an intermediate C++ representation.

class InstrHandler
{
	AsmSortOrder 	sort_order;
	AsmOpTable	op_table;
	InstrList *	instr_list;
	StringList *	string_table;

private:

	void WritePrologue (FILE * fp);

	void GenPrologue (FILE * fp);

	void WriteEpilogue (FILE * fp);

	void GenEpilogue (FILE * fp);

	void WriteStringTable (FILE * fp);

	void WriteInstrs (FILE * fp);

	void BuildStringTable (long size);

	void EnterOperand (Instr * instr, long op_ix, long str_ix);

	long CountStringArgs (void);

public:

	InstrHandler () :
			sort_order(SORT_ORDER_SIZE),
			op_table(OP_TABLE_SIZE)
		{ 
			instr_list = NULL; 
			string_table = NULL;
		}

	// *NOTE* using dynamic lists in the assembler should get
	// rid of this problem...

	InstrHandler (long list_size) :
			sort_order(SORT_ORDER_SIZE),
			op_table(OP_TABLE_SIZE)
		{ 
			instr_list = new InstrList(list_size); 
			mem_assert(instr_list);

			string_table = NULL;
		}

	InstrList *& GetInstrList (void)
		{ return instr_list; }

	AsmSortOrder * GetSortOrder (void)
		{ return & sort_order; }

	AsmOpTable * GetOpTable (void)
		{ return & op_table; }

	void Write (char * out_file);	// write C++ output.

	void Gen (char * out_file);	// generate TRIM code.

	~ InstrHandler ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

#endif /* ASMLISTS_HXX */

