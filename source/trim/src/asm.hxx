/////////////////////////////////////////////////////////////////////////////
//  asm.hxx
//
//  definitions to handle TRIM assembler code.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef ASM_HXX
#define ASM_HXX

#include "const.hxx"
#include "config.hxx"
#include "asmlists.hxx"
#include "basics.hxx"

/////////////////////////////////////////////////////////////////////////////

extern InstrHandler *  instr_handler;
extern LabelTable *    label_tab;
extern StringHash *    hash;
extern char *          infilename;
extern char *          outfilename;
extern char *          prog_name;
extern int             vflag;
extern int             gflag;

/////////////////////////////////////////////////////////////////////////////

inline
Instr * append_instr(Instr * instr)
{
	return instr_handler->GetInstrList()->Append(instr);	
}

/////////////////////////////////////////////////////////////////////////////
// convert OBJ style identifiers into something that C++ can handle...

inline
String clean_up_iden (String str)
{
	assert(strlen(str)+1 <= MAX_NAME_LEN);

	char buf [MAX_NAME_LEN];

	strcpy(buf, str);

        // make the label printable

	for (int i = 0; i < strlen(buf); i++)
	{
		if (!isalnum(buf[i]))
		{
			buf[i] = '_';
		}
	}

	return STRING(buf); 
}

/////////////////////////////////////////////////////////////////////////////

#endif /* ASM_HXX */

