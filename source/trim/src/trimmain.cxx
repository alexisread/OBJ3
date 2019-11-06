/////////////////////////////////////////////////////////////////////////////
//  trimmain.cxx
//
//  the main driver for TRIM.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "trim.hxx"

/////////////////////////////////////////////////////////////////////////////
// global objects.

Trim * 		trim;
StringHash * 	hash;
BOOL 		vflag = FALSE;
extern BOOL 	raw_output; // defined in `basics.cxx'
extern char *	program_name; // defined by the user program

/////////////////////////////////////////////////////////////////////////////

void usage (void)
{
	fprintf(stderr, "usage: trim [-h|-r|-v] \n");
	fprintf(stderr, "  -h : produce this message.\n");
	fprintf(stderr, "  -r : output result term unformatted (raw).\n");
	fprintf(stderr, "  -v : run TRIM in verbose mode.\n");
	exit(1);
}

/////////////////////////////////////////////////////////////////////////////

main(int argc, char * argv[])
{
	long tc = 0;
	int i = 1;

        while (--argc && argv[i][0] == '-')
        {
                switch (argv[i][1])
                {
                        case 'h':
				usage();
				break;

                        case 'r':
                                raw_output = TRUE;
                                break;

                        case 'v':
                                vflag = TRUE;
                                break;

                        default:
                                fprintf(stderr,
					"error -- unknown switch `%s'.\n", 
					argv[i]);
				exit(1);
                                break;
                }
                i++;
        }

	if (vflag)
	{
		fprintf(stderr, 
			"TRIM Rev %s -- (c) Copyright 1995, Lutz H. Hamel\n",
			TRIM_VERSION);
		fprintf(stderr, "Program\t\t: `%s'\n", program_name);
	}

	hash = new StringHash(HASH_TAB_SIZE, BUCKET_SIZE);
	mem_assert(hash);

	trim = new Trim();
	mem_assert(trim);

	if (vflag)	
	{
		clock();
	}

	trim->Run();

	if (vflag)	
	{
		tc = clock();
	}

	if (vflag)
	{
		fprintf(stderr, "Reductions\t: %d\n", trim->GetNoReduct());
#ifdef __sun__
		fprintf(stderr, "CPU Time\t: %f sec.\n", tc/1000000.0);
		fprintf(stderr, "Reductions/sec.\t: %f\n", 
			(trim->GetNoReduct()*1000000.0)/tc);
#else
		fprintf(stderr, "CPU Time\t: %f sec.\n", tc*1.0/CLOCKS_PER_SEC);
		fprintf(stderr, "Reductions/sec.\t: %f\n", 
			(trim->GetNoReduct()*1.0*CLOCKS_PER_SEC)/tc);
#endif
	}

	exit(0);
}

/////////////////////////////////////////////////////////////////////////////
