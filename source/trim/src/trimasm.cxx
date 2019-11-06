/////////////////////////////////////////////////////////////////////////////
//  trimasm.cxx
//
//  definition of TRIM assembler.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "asm.hxx"
#include "fileio.h"

/////////////////////////////////////////////////////////////////////////////

int 		errors = 0;
InstrHandler *  instr_handler = NULL;
LabelTable *    label_tab = NULL;
StringHash *	hash = NULL;
char *		infilename = NULL;
char *		outfilename = NULL;
char *		prog_name = NULL;
int		vflag = 0;
int		gflag = 0;

/////////////////////////////////////////////////////////////////////////////

main(int argc, char * argv[])
{
	int i = 1;
	argc--;

        while (argc && argv[i][0] == '-')
        {
                switch (argv[i][1])
                {
                        case 'g':
                                gflag++;
                                break;

                        case 'n':
                                if (argv[i][2])
                                        prog_name = &argv[i][2];
                                else
                                        prog_name = argv[(argc--,++i)];
                                break;

                        case 'o':
                                if (argv[i][2])
                                        outfilename = &argv[i][2];
                                else
                                        outfilename = argv[(argc--,++i)];
                                break;

                        case 'v':
                                vflag++;
                                break;

                        default:
                                fprintf(stderr,
					"warning -- unknown switch `%s'.\n", 
					argv[i]);
                                break;
                }
                i++;
		argc--;
        }

	if (vflag)
	{
		fprintf(stderr, 
		    	"TRIM Assembler Rev %s -- "
			"(c) Copyright 1995, Lutz H. Hamel\n",
			TRIMASM_VERSION);
	}

	if (!argc)
	{
		fprintf(stderr, 
			"usage: "
			"trimasm [-g|-v|-n <name>|-o <file>] <asm_file>\n");
		fprintf(stderr, 
			"  -g : generate debugging statements into code.\n");
		fprintf(stderr, 
			"  -n : specifiy alternate program name.\n");
		fprintf(stderr, 
			"  -o : specifiy alternate output file name.\n");
		fprintf(stderr, 
			"  -v : run assembler in verbose mode.\n");
		exit(1);
	}


        infilename = argv[i++];
	strcpy(scanner_filename(), infilename);

	// make our output file name
	if (!outfilename)
	{
		char * base = find_base(infilename);

	        outfilename = (char *)malloc(strlen(base) + 
				strlen(INSTR_FILE_EXT) + 1);
		mem_assert(outfilename);

		strcpy(outfilename, base);
		strcat(outfilename, INSTR_FILE_EXT);
	}

	if (vflag)
	{
		fprintf(stderr, "assembling `%s' => `%s'\n", 
			infilename, outfilename);
	}

	// build our global structures...

	instr_handler = new InstrHandler(MAX_NO_INSTRS);
	mem_assert(instr_handler);

	label_tab = new LabelTable(MAX_LABEL_DEFS);
	mem_assert(label_tab);

	hash = new StringHash(HASH_TAB_SIZE, BUCKET_SIZE);
	mem_assert(hash);


	// parse...

        yyin = text_read_open(infilename);

        if (!yyin)
        {
                fprintf(stderr,
			"fatal -- unable to open `%s' for input.\n", 
			infilename);
		exit(1);
        }

	yyparse();

	text_close(yyin);


	// do our thing...

	if (!errors)
	{
		label_tab->CheckInstrs(instr_handler->GetInstrList());
	}

#ifdef DEBUG
	fprintf(stderr, "*** Label Table:\n");
	label_tab->Print(stderr);
#endif
	
	if (!errors)
	{
		instr_handler->Write(outfilename);
	}


	// if there were errors get rid of the outputfile.

	if (errors)
	{
		unlink(outfilename);
	}


	if (vflag)
	{
		if (errors)
		{
			fprintf(stderr, 
				"assembly failed with %d error(s).\n", errors);
		}
		else
		{
			fprintf(stderr, "assembly successful.\n");
		}
	}

	exit(errors);
}

/////////////////////////////////////////////////////////////////////////////
