/////////////////////////////////////////////////////////////////////////////
//  eqcomp.hxx
//
//  definitions for the Eq compiler.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef EQCOMP_HXX
#define EQCOMP_HXX

#include "const.hxx"
#include "config.hxx"
#include "basics.hxx"

/////////////////////////////////////////////////////////////////////////////
// Eq -- a simple equational language 
//
//	OSMOD	-- order sorted modules
//	MOD	-- equational modules
//	EQLIST	-- list of equations
//	UEQ	-- an unconditional equation
//	CEQ	-- a conditional equation
//	LOP	-- an operator on the lhs of an equation
//	LVAR	-- a variable on the lhs of an equation
//	ROP	-- an operator on the rhs of an equation
//	RVAR	-- a variable on the rhs of an equation
//
/////////////////////////////////////////////////////////////////////////////
// the actual class hierarchy of the abstract syntax tree nodes
// can be summarized as follows.

class EqTreeNode;
class EqOSMod;		// < EqTreeNode
class EqMod;		// < EqTreeNode
class EqList;		// < EqTreeNode
class Eq;		// < EqTreeNode
class UEq;		// < Eq
class CEq;		// < Eq
class EqTerm;		// < EqTreeNode
class EqLOp; 		// < EqTerm
class EqLVar;		// < EqTerm
class EqROp;		// < EqTerm
class EqRVar;		// < EqTerm

/////////////////////////////////////////////////////////////////////////////

class EqEnv;

/////////////////////////////////////////////////////////////////////////////
// compiler wide externals

extern StringHash *    hash;
extern char *          infilename;
extern char *          outfilename;
extern int             vflag;
extern int             gflag;
extern EqOSMod *       module;

/////////////////////////////////////////////////////////////////////////////
// functions from the scanner and parser.

int scanner_line (void);
char * scanner_filename (void);
extern int errors;
extern FILE *yyin;

int yyparse();
int yylex();

/////////////////////////////////////////////////////////////////////////////

LIST(EqTermList, EqTerm *);

/////////////////////////////////////////////////////////////////////////////

class EqTreeNode 
{
	EqTreeNodeType	tag;
	String		file_name;
	int		line;

protected:

	static int indent_level;

	void Indent (FILE * fp);
		

public:

	EqTreeNode (EqTreeNodeType ty)
		{ 
			tag = ty; 
			file_name = scanner_filename();
			line = scanner_line();
		}

	EqTreeNodeType GetTag (void)
		{ return tag; }

	String GetFileName (void)
		{ return file_name; }

	int GetLine (void)
		{ return line; }


	// abstract interface

	virtual void Print (FILE * fp) = 0;

	virtual void Translate (FILE * fp) = 0;

	virtual void FillEqEnv (EqEnv * env);

	virtual void LeftLinear (void);

	virtual ~ EqTreeNode ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

class EqOSMod : public EqTreeNode
{
	SortOrder * 	so;
	OpTable * 	ot;
	EqMod *		mod;

public:

	EqOSMod (SortOrder * so_init, OpTable * ot_init, EqMod * mod_init) :
			EqTreeNode(OSMOD)
		{ 
			so = so_init;
			ot = ot_init;
			mod = mod_init;
		}

	static void Parse (FILE * fp);

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	SortOrder * GetSortOrder (void)
		{ return so; }

	OpTable * GetOpTable (void)
		{ return ot; }

	EqMod * GetEqMod (void)
		{ return mod; }

	virtual ~EqOSMod ()
		{}
};

/////////////////////////////////////////////////////////////////////////////

class EqMod : public EqTreeNode
{
	EqList * eq_list;

public:

	EqMod (EqList * l) :
			EqTreeNode(MOD)
		{ assert(l); eq_list = l; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	virtual void LeftLinear (void);

	EqList * GetEqList (void)
		{ return eq_list; }

	void AddEq (Eq * eq);

	virtual ~ EqMod ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

LLIST(AuxEqList,Eq *);

class EqList : public AuxEqList, public EqTreeNode
{
private:

	void TranslateEquation (FILE * fp, int i);

public:

	EqList () :
			AuxEqList(),
			EqTreeNode(EQLIST)
		{;}

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	virtual void LeftLinear (void);

	void InsertSorted (Eq * e);

	EqList * GetEqList (void)
		{ return this; }

	virtual ~ EqList()
		{;}

};

/////////////////////////////////////////////////////////////////////////////
// this is an abstract class -- it is just here for typing purposes
// you are not actually able to instantiate this -- you have to
// instantiate this as something more concrete: either UEq or CEq.

class Eq : public EqTreeNode
{
protected:

	Eq (EqTreeNodeType t) :
			EqTreeNode(t)
		{;}

public:

	virtual EqTerm * GetLHS (void) = 0;

	virtual EqTerm * GetRHS (void) = 0;

	virtual EqEnv * GetEnv (void) = 0;

	virtual ~ Eq ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class UEq : public Eq
{
	EqTerm * 	lhs;
	EqTerm *	rhs;
	EqEnv *		env;

public:

	UEq (EqTerm * l, EqTerm * r) :	
			Eq(EQ)
		{ lhs = l; rhs = r; env = NULL; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	virtual void LeftLinear (void);

	EqTerm * GetLHS (void)
		{ return lhs; }

	EqTerm * GetRHS (void)
		{ return rhs; }

	EqEnv * GetEnv (void)
		{ return env; }

	virtual ~ UEq ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class CEq : public Eq
{
	EqTerm * 	lhs;
	EqTerm *	rhs;
	EqTerm *	cond;
	EqEnv *		env;

public:

	CEq (EqTerm * l, EqTerm * r, EqTerm * c) :
			Eq(CEQ)
		{ lhs = l; rhs = r; cond = c; env = NULL; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	virtual void LeftLinear (void);

	EqTerm * GetLHS (void)
		{ return lhs; }

	EqTerm * GetRHS (void)
		{ return rhs; }

	EqTerm * GetCOND (void)
		{ return cond; }

	EqEnv * GetEnv (void)
		{ return env; }

	virtual ~ CEq ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////
// this is an abstract class -- it is just here for typing purposes
// you are not actually able to instantiate this -- you have to
// instantiate this as something more concrete: either op or var.

class EqTerm : public EqTreeNode
{
protected:

	EqTerm (EqTreeNodeType t) :
			EqTreeNode(t)
		{;}

public:

	virtual String GetName (void) = 0;

	virtual String GetSort (void) = 0;

	virtual ~ EqTerm ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class EqLOp : public EqTerm
{
	String		op_name;
	String		sort_name;
	EqTermList *	children;		

public:

	EqLOp (String op, String s, EqTermList * ch) :
			EqTerm(LOP)
		{ op_name = op; sort_name = s; children = ch; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	String GetName (void)
		{ return this->GetOp(); }

	String GetOp (void)
		{ return op_name; }

	String GetSort (void)
		{ return sort_name; }

	long GetArity (void)
		{ return children->Count(); }

	EqTermList * GetChildren (void)
		{ return children; }

	virtual ~ EqLOp ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class EqLVar : public EqTerm
{
	String		var_name;
	SortList * 	sort_list;

public:

	EqLVar (String var, SortList * sl) :
			EqTerm(LVAR)
		{ var_name = var; sort_list = sl; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	String GetName (void)
		{ return this->GetVar(); }

	String GetVar (void)
		{ return var_name; }

	SortList * GetSortList (void)
		{ return sort_list; }

	String GetSort (void)
		{ 
			fprintf(stderr, "error -- no sort on variables.\n"); 
			abort(); 
			return NULL;
		}

	virtual ~ EqLVar ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class EqROp : public EqTerm
{
	String		op_name;
	String		sort_name;
	EqTermList *	children;		

public:

	EqROp (String op, String s, EqTermList * ch) :
			EqTerm(ROP)
		{ op_name = op; sort_name = s; children = ch; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	String GetName (void)
		{ return this->GetOp(); }

	String GetOp (void)
		{ return op_name; }

	String GetSort (void)
		{ return sort_name; }

	long GetArity (void)
		{ return children->Count(); }

	EqTermList * GetChildren (void)
		{ return children; }

	virtual ~ EqROp ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////

class EqRVar : public EqTerm
{
	String		var_name;
	SortList *	sort_list;

public:

	EqRVar (String var, SortList * sl) :
			EqTerm(RVAR)
		{ var_name = var; sort_list = sl; }

	virtual void Print (FILE * fp);
		
	virtual void Translate (FILE * fp);

	virtual void FillEqEnv (EqEnv * env);

	String GetName (void)
		{ return this->GetVar(); }

	String GetVar (void)
		{ return var_name; }

	SortList * GetSortList (void)
		{ return sort_list; }

	String GetSort (void)
		{ 
			fprintf(stderr, "error -- no sort on variables.\n"); 
			abort(); 
			return NULL;
		}

	virtual ~ EqRVar ()
		{;}

};

/////////////////////////////////////////////////////////////////////////////
// the following is used in the generation of context-free unique
// labels -- see the specification in `basics.obj'.

class Label
{
	String		label_name;
	int		label_id;

public:

	Label (String name, int id) 
		{ 
			label_name = name;
			label_id = id;
		}

	String & GetLabelName (void)
		{ return label_name; }

	int & GetLabelId (void)
		{ return label_id; }

	~ Label ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

LIST(AuxLabelList, Label *);

/////////////////////////////////////////////////////////////////////////////

class LabelList : public AuxLabelList
{
public:

	LabelList (long size) :
			AuxLabelList(size)
		{;}

	Label * FindLabel(String label);

	~ LabelList ()
		{;}
		
};

/////////////////////////////////////////////////////////////////////////////

STACK(AuxLabelStack, LabelList *);

/////////////////////////////////////////////////////////////////////////////

class LabelStack : public AuxLabelStack
{
	int	label_count;

public:

	LabelStack (int size) :
			AuxLabelStack(size)
		{ label_count = 0; }

        int IncLabelCount (void)
		{ return ++label_count; }

	int GetLabelCount (void)
		{ return label_count; }

	void ResetLabelCount (void)
		{ label_count = 0; }

	int GetLabelId (String name);

	int GetLabelIdPush (String name);

	int GetLabelIdPushReset (String name);

	int GetLabelIdPop (String name);

	~ LabelStack ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////
// define a variable record for keeping track of Eq variables

class EqVar
{
	String		var_name;
	EqTermList *	lhs_occur;
	EqTermList *	rhs_occur;

	

public:

	EqVar (String var)
		{
			assert(var);
			var_name = var;
			lhs_occur = mem(new EqTermList(MAX_VAR_OCCUR));
			rhs_occur = mem(new EqTermList(MAX_VAR_OCCUR));
		}

	String  GetVar (void)
		{ return var_name; }

	EqTermList * GetLHS (void)
		{ return lhs_occur; }

	EqTermList * GetRHS (void)
		{ return rhs_occur; }

	void Print (FILE * fp);

	~ EqVar ()
		{;}	
};

/////////////////////////////////////////////////////////////////////////////
// an environment for keeping track of Eq variables

LIST(EqVarList, EqVar *);

class EqEnv : public EqVarList
{

protected:

	EqVar * Enter (String x)
		{
			return Append(mem(new EqVar(x)));
		}

public:

	EqEnv (long max_size) :
			EqVarList(max_size)
		{;}

	// *NOTE* `Find' always returns a record -- if none is found
	// it will create a fresh one in the environment.

	EqVar * Find (String x);

	// *NOTE* `Fetch' only returns a record if one is found otherwise
	// it will return NULL.

	EqVar * Fetch (String x);

	// append two environments together respecting the semantics
	// of variable occurrances.

	EqEnv * CleanAppend (EqEnv * env);

	void Print (FILE * fp);

	~ EqEnv()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

String GenName (void);

/////////////////////////////////////////////////////////////////////////////

#endif /* EQCOMP_HXX */
