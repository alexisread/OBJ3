
void record_temp_file(char *filename, int always_delete, int fail_delete);

#ifdef USG
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define vfork fork
#endif /* USG */

#ifdef __MSDOS__
#define R_OK 4
#define W_OK 2
#define X_OK 0
#define vfork fork
#endif 

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#ifdef alloca
#undef alloca
#endif
#define alloca __builtin_alloca
#endif

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

void *xmalloc(int size);
void *xrealloc(void *ptr, int size);
char *concat(char *s1, char *s2, char *s3);
char *save_string(char *s, int len);
void pfatal_with_name(char *name);
void perror_with_name (char *name);
void perror_exec(char *name);
void fancy_abort(void);

#ifdef HAVE_VPRINTF

void fatal(char *, ...);
void error(char *, ...);

#else /* not HAVE_VPRINTF */

void fatal (char *msg, char *arg1, char *arg2);
void error (char *msg, char *arg1, char *arg2);

#endif /* not HAVE_VPRINTF */

void validate_all_switches(void);
void validate_switches(char *start);

/* 
** If a stage of compilation returns an exit status >= 1,
** compilation of that file ceases.  
*/
#define MIN_FATAL_STATUS 1

#ifndef TMPDIR
#define TMPDIR "TMPDIR"
#endif

/* 
** config.h can define ASM_SPEC to provide extra args to the assembler
** or extra switch-translations.  
*/
#ifndef ASM_SPEC
#define ASM_SPEC ""
#endif

/* 
** config.h can define CPP_SPEC to provide extra args to the C preprocessor
** or extra switch-translations.  
*/
#ifndef CPP_SPEC
#define CPP_SPEC ""
#endif

/* 
** config.h can define CPP_PREDEFINES to provide extra args to the C preproc
** or extra switch-translations.  
*/
#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES ""
#endif

/* 
** config.h can define CC1_SPEC to provide extra args to cc1
** or extra switch-translations.  
*/
#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif

/* 
** config.h can define LINK_SPEC to provide extra args to the linker
** or extra switch-translations.  
*/
#ifndef LINK_SPEC
#define LINK_SPEC ""
#endif

/* 
** config.h can define LIB_SPEC to override the default libraries.  
*/
#ifndef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
#endif

/* 
** config.h can define STARTFILE_SPEC to override the default crt0 files.  
*/
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"
#endif

/* 
** This spec is used for telling cpp whether char is signed or not.  
*/
#define SIGNED_CHAR_SPEC  \
  (DEFAULT_SIGNED_CHAR ? "%{funsigned-char:-D__CHAR_UNSIGNED__}"	\
   : "%{!fsigned-char:-D__CHAR_UNSIGNED__}")

/* 
** This defines which switch letters take arguments.  
*/
#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)      \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'Y' || (CHAR) == 'm' \
   || (CHAR) == 'L' || (CHAR) == 'i' || (CHAR) == 'A')
#endif

/* 
** This defines which multi-letter switches take arguments.  
*/
#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) (!strcmp (STR, "Tdata"))
#endif

#ifndef USAGE
#define USAGE "no input files specified."
#endif

/* 
** This structure says how to run one compiler, and when to do so.  
*/
struct compiler
{
  	char *suffix;	/* 	Use this compiler for input files
		   		whose names end in this suffix.  */
  	char *spec;	/* 	To use this compiler, pass this spec
				to do_spec.  */
};

/* Default prefixes to attach to command names.  */

#ifndef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX "/usr/local/lib/gcc-"
#endif /* !defined STANDARD_EXEC_PREFIX */

#ifndef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/local/lib/"
#endif /* !defined STANDARD_STARTFILE_PREFIX */

/* 
** Clear out the vector of arguments (after a command is executed).  
*/
void clear_args(void);

/* Add one argument to the vector at the end.
** This is done when a space is seen or at the end of the line.
** If DELETE_ALWAYS is nonzero, the arg is a filename
**  and the file should be deleted eventually.
** If DELETE_FAILURE is nonzero, the arg is a filename
**  and the file should be deleted if this compilation fails.  
*/
void store_arg(char *arg, int delete_always, int delete_failure);

/* 
** Record the names of temporary files we tell compilers to write,
** and delete them at the end of the run.
**
** This is the common prefix we use to make temp file names.
** It is chosen once for each run of this program.
** It is substituted into a spec by %g.
** Thus, all temp file names contain this prefix.
** In practice, all temp file names start with this prefix.
**
** This prefix comes from the envvar TMPDIR if it is defined;
** otherwise, from the P_tmpdir macro if that is defined;
** otherwise, in /usr/tmp or /tmp.
*/

/* 
** Define the list of temporary files to delete.  
*/
struct temp_file
{
  char 		*name;
  struct temp_file 	*next;
};

/* 
** Record FILENAME as a file to be deleted automatically.
** ALWAYS_DELETE nonzero means delete it if all compilation succeeds;
** otherwise delete it in any case.
** FAIL_DELETE nonzero means delete it if a compilation step fails;
** otherwise delete it in any case.
*/
void record_temp_file(char *filename, int always_delete, int fail_delete);

/* 
** Delete all the temporary files whose names we previously recorded.  
*/
void delete_temp_files(void);

/* 
** Delete all the files to be deleted on error.  
*/
void delete_failure_queue(void);
void clear_failure_queue(void);

/* 
** Compute a string to use as the base of all temporary file names.
** It is substituted for %g.  
*/
void choose_temp_base(void);

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number.  */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

/* 
** Execute the command specified by the arguments on the current line of spec.
** When using pipes, this includes several piped-together commands
** with `|' between them.
**
** Return 0 if successful, -1 if failed.  
*/
int execute(void);

/* 
** Find all the switches given to us
** and make a vector describing them.
** The elements of the vector a strings, one per switch given.
** If a switch uses the following argument, then the `part1' field
** is the switch itself and the `part2' field is the following argument.
** The `valid' field is nonzero if any spec has looked at this switch;
** if it remains zero at the end of the run, it must be meaningless.  
*/
struct switchstr
{
  char 	*part1;
  char 	*part2;
  int 	valid;
};

/* 
** Create the vector `switches' and its contents.
** Store its length in `n_switches'.  
*/
void process_command(int argc, char **argv);

/* 
** Process a spec string, accumulating and running commands.  
*/

/* 
** Process the spec SPEC and run the commands specified therein.
** Returns 0 if the spec is successfully processed; -1 if failed.  
*/
int do_spec(char *spec);

/* 
** Process the sub-spec SPEC as a portion of a larger spec.
** This is like processing a whole spec except that we do
** not initialize at the beginning and we do not supply a
** newline by default at the end.
** INSWITCH nonzero means don't process %-sequences in SPEC;
** in this case, % is treated as an ordinary character.
** This is used while substituting switches.
** INSWITCH nonzero also causes SPC not to terminate an argument.
**
** Value is zero unless a line was finished
** and the command on that line reported an error.  
*/
int do_spec_1 (char *spec, int inswitch);

/* 
** Return 0 if we call do_spec_1 and that returns -1.  
*/
char *handle_braces (char *p);

/* 
** Pass a switch to the current accumulating command
** in the same form that we received it.
** SWITCHNUM identifies the switch; it is an index into
** the vector of switches gcc received, which is `switches'.
** This cannot fail since it never finishes a command line.  
*/
void give_switch(int switchnum);

/* 
** Search for a file named NAME trying various prefixes including the
** user's -B prefix and some standard ones.
** Return the absolute pathname found.  If nothing is found, return NAME.  
*/
char *find_file(char *name);

/* 
** On fatal signals, delete all the temporary files.  
*/
void fatal_error(int signum);

void *xmalloc(int size);
void *xrealloc(void *ptr, int size);

/* 	
** Return a newly-allocated string whose contents concatenate those of 
** s1, s2, s3.  
*/
char *concat(char *s1, char *s2, char *s3);
char *save_string(char *s, int len);
void pfatal_with_name(char *name);
void perror_with_name (char *name);
void perror_exec(char *name);

/* 
** More 'friendly' abort that prints the line and file.
** config.h can #define abort fancy_abort if you like that sort of thing.  
*/
void fancy_abort(void);

#ifdef HAVE_VPRINTF

/* Output an error message and exit */

void fatal (char *fmt, ...);
void error (char *fmt, ...);

#else /* not HAVE_VPRINTF */

void fatal (char *msg, char *arg1, char *arg2);
void error (char *msg, char *arg1, char *arg2);

#endif /* not HAVE_VPRINTF */

void validate_all_switches(void);

/* 
** Look at the switch-name that comes after START
** and mark as valid all supplied switches that match it.  
*/
void validate_switches(char *start);
