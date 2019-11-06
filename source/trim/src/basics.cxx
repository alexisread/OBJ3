/////////////////////////////////////////////////////////////////////////////
//  basics.cxx
//
//  the "basics" from the TRIM specification -- function implementations
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "basics.hxx"

/////////////////////////////////////////////////////////////////////////////

int iTerm::indent_level = 0;
int iTerm::curr_line = 0;


/////////////////////////////////////////////////////////////////////////////
// this is a flag which allows us to generate pretty or almost
// only machine readable code -- the pretty printing can be very
// time consuming on large terms...

BOOL raw_output = FALSE; // set in `trimmain.cxx'

/////////////////////////////////////////////////////////////////////////////

void iTerm::Indent (FILE * fp)
{
	extern BOOL raw_output; // set in `trimmain.cxx'

	if (!raw_output)
	{
		for (int i = 0; i < indent_level; i++)
		{
			fprintf(fp, "  ");
		}
	}
}

/////////////////////////////////////////////////////////////////////////
// get the next token from the input, ignoring white space and
// carriage returns.

char * iTerm::ScanFile (FILE * infp)
{
	static char token_buf [MAX_TOKEN_LEN];

	char c;
	int ix = 0;
	int quote_mode = FALSE;

	while (c = getc (infp))
	{
		// handle some special characters.

		if (c == '\n')
		{
			curr_line++;
		}
		else if (c == EOF && ix == 0)
		{
			return NULL;
		}

		// figure out what we need to do.

		if (IS_SEPARATOR(c) && ix > 0)
		{
			if (c != '\n') 
			{
				ungetc(c, infp);
			}

			token_buf [ix] = '\0';
			ix = 0;
			return token_buf;
		}
		else if (IS_COMMENT(c))
		{
			while (c = getc(infp))
			{
				if (c == '\n' || c == EOF)
				{
					break;
				}
			}
			ungetc(c, infp);
		}
		else if (!IS_SEPARATOR(c))
		{
			token_buf [ix++] = c;
		}
		else if (c == '(' || c == ')')
		{
			token_buf [ix++] = c;
			token_buf [ix] = '\0';
			ix = 0;
			return token_buf;
		}
		else if (c == '"')
		{
			while (c = getc(infp))
			{
				if (c == '\n')
				{
					fprintf(stderr,
						"fatal -- "
						"illegal break in string (line %d).\n",
						curr_line);
					exit(1);
				}
				else if (c == EOF)
				{
					fprintf(stderr,
						"fatal -- "
						"unterminated string constant.\n");
					exit(1);
				}
				else if (c == '"')
				{
					token_buf[ix++] = c;
					token_buf [ix] = '\0';
					ix = 0;
					return token_buf;
				}			
				else
				{
					token_buf[ix++] = c;
				}
			}
		}
	}
        return 0;
}

/////////////////////////////////////////////////////////////////////////////

iTerm * iTerm::ParseNode (FILE * fp)
{
	char * open_paren = ScanFile(fp);
	FLAG_SYNTAX_ERROR(open_paren);
	FLAG_SYNTAX_ERROR(strcmp(open_paren, "(") == 0);

	char * op_string = ScanFile(fp);
	FLAG_SYNTAX_ERROR(open_paren);
	FLAG_SYNTAX_ERROR(strcmp(open_paren, "op") == 0);

	char * arity_str = ScanFile(fp);
	FLAG_SYNTAX_ERROR(arity_str);

	long arity = atoi(arity_str);

	char * op_name = ScanFile(fp);
	FLAG_SYNTAX_ERROR(op_name);

	op_name = hash->FindString(op_name);

	char * ty_name = ScanFile(fp);
	FLAG_SYNTAX_ERROR(ty_name);

	ty_name = hash->FindString(ty_name);

	iTerm * t = NULL;

	if (arity == 0)
	{
		t = new iTerm(op_name, ty_name);
		mem_assert(t);
	}
	else
	{
		t = new iTerm(op_name, ty_name, arity);
		mem_assert(t);

		for (int i = 0; i < arity; i++)
		{
			t->Append(ParseNode(fp));
		}

	}

	char * close_paren = ScanFile(fp);
	FLAG_SYNTAX_ERROR(close_paren);
	FLAG_SYNTAX_ERROR(strcmp(close_paren, ")") == 0);

	return t;
}

/////////////////////////////////////////////////////////////////////////////

void iTerm::PrintNode (FILE * fp)
{
#ifdef OBJ_OUTPUT

	if (strchr(GetOp(), '_'))
	{
		String op = GetOp();
		char * p = (char *) op;
		int child_count = 0;

		fprintf(fp, "( ");

		while (*p != '\0')
		{
			if (*p == '_')
			{
				Get(child_count++)->PrintNode(fp);
			}
			else
			{
				fprintf(fp, "%c ", *p);
			}

			p++;
		}

		assert(child_count == GetArity());

		fprintf(fp, ") ");
	}
	else
	{
		fprintf(fp, "%s ", GetOp());

		if (GetArity() > 0)
		{
			fprintf(fp, "( ");

			for (int i = 0; i < GetArity(); i++)
			{
				Get(i)->PrintNode(fp);
			}
	
			fprintf(fp, ") ");
		}
	}

#else /* not OBJ_OUTPUT */

	extern BOOL raw_output; // set in `trimmain.cxx'

	Indent(fp);

#ifdef DEBUG
	fprintf(fp, "(op %d %s %s [%s]", 
		GetArity(), 
		GetOp(), 
		GetSort(),
		ConvertTState(tstate));
#else
	fprintf(fp, "(op %d %s %s ", 
		GetArity(), 
		GetOp(), 
		GetSort());
#endif

	if (GetArity() == 0)
	{
		fprintf(fp, ")");
	}
	else
	{
		indent_level++;

		for (int i = 0; i < GetArity(); i++)
		{
			if (!raw_output)
			{
				fprintf(fp, "\n");
			}

			Get(i)->PrintNode(fp);
		}

		fprintf(fp, ")");
		indent_level--;
	}
#endif /* OBJ_OUTPUT */
}

/////////////////////////////////////////////////////////////////////////////

void TrimEnv::Print (void)
{
	for (int i = 0; i < Count(); i++)
	{
		fprintf(stderr, "binding `%s' to: \n",
			Get(i).GetVar());
		Get(i).GetTerm()->WriteTerm(stderr);
		fprintf(stderr, "\n");
	}
}

/////////////////////////////////////////////////////////////////////////////

void SortList::Print (FILE * fp)
{
        for (int i = 0; i < Count(); i++)
        {
		fprintf(fp, "%s ", Get(i));
        }
}

/////////////////////////////////////////////////////////////////////////////


