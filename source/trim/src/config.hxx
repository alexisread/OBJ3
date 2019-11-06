/////////////////////////////////////////////////////////////////////////////
//  config.hxx
//
//  configuration parameter file.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef CONFIG_HXX
#define CONFIG_HXX

/////////////////////////////////////////////////////////////////////////////

extern "C" 
{
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
}

/////////////////////////////////////////////////////////////////////////////
// tunable parameters.

#define	TRIM_VERSION		"1.4"
#define	TRIMASM_VERSION		"1.2"
#define	TRIMOPT_VERSION		"1.3"
#define	EQCOMP_VERSION		"1.2a"

#define	OP_TABLE_SIZE		128
#define	SORT_ORDER_SIZE		64
#define	SORT_LIST_SIZE		64
#define	SORT_TUPLE_LIST_SIZE	64
#define	EQ_INTERNAL_TAG		"$TRIM$_"
#define	EQ_VAR_TAG		EQ_INTERNAL_TAG "var"
#define	EQ_BOOL_TRUE		"true"
#define	EQ_BOOL_SORT		"Bool"
#define	MAX_VAR_OCCUR		32
#define	MAX_TDL_ARITY		32
#define	MAX_NAME_LEN		256
#define	TRIM_IMAGE_REV		2
#define	HASH_TAB_SIZE		997
#define	BUCKET_SIZE		64
#define	LABEL_ID_ERROR		-1
#define	LABEL_ID_LIST_LENGTH	1024
#define	LABEL_STACK_DEPTH	1024
#define	STATE_STACK_DEPTH	512
#define	EVAL_STACK_DEPTH	2*4096
#define	OPERAND_STACK_DEPTH	2*32
#define	WORK_STACK_DEPTH	2*1024
#define	ENV_SIZE		32
#define	MAX_TRIM_INSTR_ARITY	3
#define	MAX_LABEL_DEFS		1024
#define	MAX_LABEL_REFS		256
#define	LABEL_NOT_DEFINED	NULL
#define	MAX_NO_INSTRS		4096
#define	TRIM_FILE_EXT		".trm"
#define	OPT_TRIM_FILE_EXT	".opt"
#define	INSTR_FILE_EXT		".cxx"
#define	MEMORY_CACHE_SIZE	100*1024
#define	ERROR			-1
#define	IX_OPERAND_NOT_DEFINED	ERROR
#define	ARITY_NOT_DEFINED	ERROR
#define	INIT_ENTRY_POINT	0
#define	TRUE			1
#define	FALSE			0
#define	FAIL_TERM		NULL
#define BOOL			int

/////////////////////////////////////////////////////////////////////////////
// code generation macro (used in `eqcomp')

#define GEN(instr,label)	fprintf(fp,instr,label);

/////////////////////////////////////////////////////////////////////////////
// a macro that allows a nice display of instructions during debugging mode

#define INSTR(i)	fprintf(stderr, "***> %s\n", i);

/////////////////////////////////////////////////////////////////////////////
// SCANNER MACROS: for the iTerm scanner defined in `basics.cxx' 

#define MAX_TOKEN_LEN		256
#define IS_SEPARATOR(c)		( c == ' '  || c == '\t' || \
				  c == '\n' || c == '#'  || \
				  c == ','  || c == '('  || \
				  c == ')'  || c == '"' )
#define IS_COMMENT(c)		( c == '#' )

#define	FLAG_SYNTAX_ERROR(e)					\
	if (!e)							\
	{							\
		fprintf(stderr, 				\
			"error -- syntax error (line %d).\n",	\
			curr_line);				\
		exit(1);					\
	}				

/////////////////////////////////////////////////////////////////////////////
// FILE ACCESS MACROS:

#define	FLAG_READ_ERROR(e)					\
	if (!e)							\
	{							\
		fprintf(stderr, 				\
			"fatal -- read error (%s:%d).\n",	\
			__FILE__,__LINE__);			\
		abort();					\
	}

#define	FLAG_WRITE_ERROR(e)					\
	if (!e)							\
	{							\
		fprintf(stderr, 				\
			"fatal -- write error (%s:%d).\n",	\
			__FILE__,__LINE__);			\
		abort();					\
	}

/////////////////////////////////////////////////////////////////////////////
// MEMORY OPERATIONS:

#define	mem_assert(expr)					\
	if (!expr)						\
	{							\
		fprintf(stderr, 				\
			"fatal -- out of memory (%s:%d).\n",	\
			__FILE__, __LINE__);			\
		abort();					\
	}

// *NOTE* we just treat this as a flag where we should be checking
// 	  memory -- currently we use our own `new' operator and
//	  this one does the checking for us -- but if you ever 
//	  this then you will have to actually do something at
//	  the places flagged by `mem'.

#define	mem(expr)	expr

#ifndef __LPI__
void * operator new (size_t i);
void  operator delete (void *);
#endif

/////////////////////////////////////////////////////////////////////////////
// STRINGS:
//
// we deal with strings as pointers -- that allows us to just do a pointer
// comparison rather than a "strcmp" -- faster!!!

typedef	char *	String;
class StringHash;

extern StringHash * hash;

#define STRING(s)	hash->FindString(s)
#define	streq(s1,s2)	(s1 == s2)

/////////////////////////////////////////////////////////////////////////////
#endif /* CONFIG_HXX */
