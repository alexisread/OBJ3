/* Compiler driver program that can handle many languages.
   Copyright (C) 1987,1989 Free Software Foundation, Inc. 

   Adapted for TRIM by Lutz Hamel, 1994.

   This program is the user interface to the C compiler and possibly to
   other compilers.  It is used because compilation is a complicated procedure
   which involves running several programs and passing temporary files between
   them, forwarding the users switches to those programs selectively,
   and deleting the temporary files at the end.

   CC recognizes how to compile each input file by suffixes in the file names.
   Once it knows which kind of compilation to perform, the procedure for
   compilation is specified by a string called a "spec".
*/

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>

#ifdef __MSDOS__
#include <io.h>
#include <process.h>
#endif

#ifdef USG
extern int execv(), execvp();
#include <sys/types.h>
#endif

#ifdef sun
#include <stdlib.h>
#include <unistd.h>
#endif

#ifdef linux
#include <stdlib.h>
#include <unistd.h>
#endif

#ifndef __MSDOS__
#include <sys/wait.h>
#include <sys/file.h>
#endif

#include <config.h>
#include <obstack.h>

#include <trimcc.h>

struct obstack obstack;

#ifndef bcopy
#define bcopy(src, dst, len)	memcpy(dst, src, len)
#endif
#ifndef bzero
#define bzero(ptr, len)			memset(ptr, 0, len)
#endif

#ifndef DIRDELIMITER
#error no directory delimiter given.
#endif

/* 
** Here are the specs for compiling files with various known suffixes.
** A file that does not end in any of these suffixes will be passed
** unchanged to the loader and nothing else will be done to it.  
*/

#ifndef COMPILER_DEFINITION
#error no compilers specified.
#endif

struct compiler compilers[] = { COMPILER_DEFINITION, {0, 0} };


/* Here is the spec for running the linker, after compiling all files.  */

#ifndef LINKER_DEFINITION
#error no linkers specified.
#endif

char *link_spec = LINKER_DEFINITION;


#ifdef __MSDOS__

/*
** Point to the environment PATH.
*/
char *env_path = 0;

#endif

/*
** Home directory of this process.
*/
char *home = 0;

/* 
** Accumulate a command (program name and args), and run it.
**
** Vector of pointers to arguments in the current line of specifications.  
*/
char **argbuf;

/* 
** Number of elements allocated in argbuf.  
*/
int argbuf_length;

/* 
** Number of elements in argbuf currently in use (containing args).  
*/
int argbuf_index;

/* Number of commands executed so far.  */

int execution_count;

/* Flag indicating whether we should print the command and arguments */

unsigned char vflag;

/* Name with which this program was invoked.  */

char *programname;

/* User-specified -B prefix to attach to command names,
   or 0 if none specified.  */

char *user_exec_prefix = 0;

/* Environment-specified prefix to attach to command names,
   or 0 if none specified.  */

char *env_exec_prefix = 0;

/* Suffix to attach to executables (e.g. `.exe') */

#ifdef MACHINE_SUFFIX
char *machine_suffix = MACHINE_SUFFIX;
#else
char *machine_suffix = 0;
#endif

char *standard_exec_prefix = STANDARD_EXEC_PREFIX;
char *standard_exec_prefix_1 = "/usr/lib/gcc-";
char *standard_startfile_prefix = STANDARD_STARTFILE_PREFIX;
char *standard_startfile_prefix_1 = "/lib/";
char *standard_startfile_prefix_2 = "/usr/lib/";

/* 
** Clear out the vector of arguments (after a command is executed).  
*/
void clear_args(void)
{
  argbuf_index = 0;
}

/* 
** Add one argument to the vector at the end.
** This is done when a space is seen or at the end of the line.
** If DELETE_ALWAYS is nonzero, the arg is a filename
**  and the file should be deleted eventually.
** If DELETE_FAILURE is nonzero, the arg is a filename
**  and the file should be deleted if this compilation fails.  
*/
void store_arg(char *arg, int delete_always, int delete_failure)
{
  if (argbuf_index + 1 == argbuf_length)
    {
      argbuf = (char **)realloc(argbuf, (argbuf_length *= 2)*sizeof (char *));
    }

  argbuf[argbuf_index++] = arg;
  argbuf[argbuf_index] = 0;

  if (delete_always || delete_failure)
    record_temp_file(arg, delete_always, delete_failure);
}

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
char *temp_filename;

/* Length of the prefix.  */

int temp_filename_length;

/* Define the list of temporary files to delete.  */

/* Queue of files to delete on success or failure of compilation.  */
struct temp_file *always_delete_queue;
/* Queue of files to delete on failure of compilation.  */
struct temp_file *failure_delete_queue;

/* 
** Record FILENAME as a file to be deleted automatically.
** ALWAYS_DELETE nonzero means delete it if all compilation succeeds;
** otherwise delete it in any case.
** FAIL_DELETE nonzero means delete it if a compilation step fails;
** otherwise delete it in any case.
*/
void record_temp_file(char *filename, int always_delete, int fail_delete)
{
  register char *name;

  name = (char *) xmalloc (strlen (filename) + 1);
  strcpy (name, filename);
  
  if (always_delete)
   {
     register struct temp_file *temp;

     for (temp = always_delete_queue; temp; temp = temp->next)
       if (! strcmp (name, temp->name))
	 goto already1;

     temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
     temp->next = always_delete_queue;
     temp->name = name;

     always_delete_queue = temp;
   already1:;
   }

  if (fail_delete)
    {
      register struct temp_file *temp;

      for (temp = failure_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already2;

      temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
      temp->next = failure_delete_queue;
      temp->name = name;

      failure_delete_queue = temp;

    already2:;
    }
}

/* 
** Delete all the temporary files whose names we previously recorded.  
*/
void delete_temp_files(void)
{
  register struct temp_file *temp;

  for (temp = always_delete_queue; temp; temp = temp->next)
    {
#ifdef DEBUG
      int i;

      printf ("Delete %s? (y or n) ", temp->name);
      fflush (stdout);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i == 'y' || i == 'Y')
#endif /* DEBUG */
	{
	  if (unlink (temp->name) < 0)
	    if (vflag)
	      perror_with_name (temp->name);
	}
    }
  always_delete_queue = 0;
}

/* 
** Delete all the files to be deleted on error.  
*/
void delete_failure_queue(void)
{
  register struct temp_file *temp;

  for (temp = failure_delete_queue; temp; temp = temp->next)
    {
#ifdef DEBUG
      int i;

      printf ("Delete %s? (y or n) ", temp->name);

      fflush (stdout);

      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;

      if (i == 'y' || i == 'Y')
#endif /* DEBUG */
	{
	  if (unlink (temp->name) < 0)
	    if (vflag)
	      perror_with_name (temp->name);
	}
    }
}

void clear_failure_queue(void)
{
  failure_delete_queue = 0;
}

/* 
** Compute a string to use as the base of all temporary file names.
** It is substituted for %g.  
*/
void choose_temp_base(void)
{
  char 	*base = getenv ("TMPDIR");
  int 	len;

  if (!base)
    {
#ifdef P_tmpdir
      if (access (P_tmpdir, R_OK | W_OK) == 0)
	base = P_tmpdir;
#endif
      if (base == (char *)0)
	{
	  if (access ("/usr/tmp", R_OK | W_OK) == 0)
	    base = "/usr/tmp/";
	  else
#ifdef __MSDOS__
	    base = "\\tmp";
#else
	    base = "/tmp/";
#endif
	}
    }

  len = strlen (base);

#ifdef __MSDOS__
  temp_filename = (char *) xmalloc (len + sizeof("\\ccXXXXXX"));
#else
  temp_filename = (char *) xmalloc (len + sizeof("/ccXXXXXX"));
#endif

  strcpy (temp_filename, base);
  	
  if (len > 0 && temp_filename[len-1] != DIRDELIMITER)
    temp_filename[len++] = DIRDELIMITER;

  strcpy (temp_filename + len, "ccXXXXXX");

#ifndef __MSDOS__
  mktemp (temp_filename);
#endif

  temp_filename_length = strlen (temp_filename);
}

/* 
** Search for an execute file through our search path.
** Return 0 if not found, otherwise return its name, allocated with malloc.  
*/
static char *find_exec_file(char *prog)
{
  int 	win = 0;
  char 	*temp;
  int 	size;

  size = standard_exec_prefix ? strlen (standard_exec_prefix) : 0;

  if (user_exec_prefix != 0 && strlen (user_exec_prefix) > size)
    size = strlen (user_exec_prefix);

  if (env_exec_prefix != 0 && strlen (env_exec_prefix) > size)
    size = strlen (env_exec_prefix);
  	
  if (home != 0 && strlen (home) > size)
    size = strlen (home);
  	
  if (standard_exec_prefix_1 != 0 && strlen (standard_exec_prefix_1) > size)
    size = strlen (standard_exec_prefix_1);

  size += strlen (prog) + 1;

  if (machine_suffix)
    size += strlen (machine_suffix) + 1;

  temp = (char *) xmalloc (size);

  
  /* Determine the filename to execute.  */

  if (user_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, prog);
	  strcat (temp, machine_suffix);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}

      if (!win)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, prog);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win && env_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, prog);
	  strcat (temp, machine_suffix);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, prog);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win && home)
    {
      if (machine_suffix)
	{
	  strcpy (temp, home);
	  strcat (temp, prog);
	  strcat (temp, machine_suffix);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, home);
	  strcat (temp, prog);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win && standard_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, prog);
	  strcat (temp, machine_suffix);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, prog);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
    }

  if (!win && standard_exec_prefix_1)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, prog);
	  strcat (temp, machine_suffix);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, prog);
#ifdef DEBUG
  fprintf(stderr, "trying `%s'.\n", temp);
#endif
	  win = (access (temp, X_OK) == 0);
	}
    }
#ifdef DEBUG
  if (win)
    fprintf(stderr, "found `%s'.\n", temp);
  else
    fprintf(stderr, "NOTHING...\n");
#endif


  return win ? temp : 0;
}

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number.  */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

/* 
** Pipe waiting from last process, to be used as input for the next one.
** Value is STDIN_FILE_NO if no pipe is waiting
** (i.e. the next command is the first of a group).  
*/
int last_pipe_input;

/* 
** Fork one piped subcommand.  FUNC is the system call to use
** (either execv or execvp).  ARGV is the arg vector to use.
** NOT_LAST is nonzero if this is not the last subcommand
** (i.e. its output should be piped to the next one.)  
*/
#ifdef HAVE_FORK 
typedef (*FORK)(char*, char*[]);
#endif


#ifdef HAVE_FORK
static int pexecute(
	FORK	func,
	char 	*program,
	char 	*argv[],
	int 	not_last)
#else  /* not HAVE_FORK */
static int pexecute(
	int	(*func)(int, char*, char*[]),
	char 	*program,
	char 	*argv[],
	int 	not_last)
#endif /* HAVE_FORK */
{
  int 	pid;
  int 	pdes[2];
  int 	input_desc 	= last_pipe_input;
  int 	output_desc = STDOUT_FILE_NO;

  /* 
   ** If this isn't the last process, make a pipe for its output,
   ** and record it as waiting to be the input to the next process.  
   */
#ifdef HAVE_PIPE
  if (not_last)
    {
      if (pipe (pdes) < 0)
	pfatal_with_name ("pipe");
      
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    last_pipe_input = STDIN_FILE_NO;
#endif

#ifdef HAVE_FORK
  pid = vfork ();

  switch (pid)
    {
    case -1:
      pfatal_with_name ("vfork");
      break;

    case 0: /* child */
      /* Move the input and output pipes into place, if nec.  */
      if (input_desc != STDIN_FILE_NO)
	{
	  close (STDIN_FILE_NO);
	  dup (input_desc);
	  close (input_desc);
			}
      if (output_desc != STDOUT_FILE_NO)
	{
	  close (STDOUT_FILE_NO);
	  dup (output_desc);
	  close (output_desc);
	}

      /* Close the parent's descs that aren't wanted here.  */
      if (last_pipe_input != STDIN_FILE_NO)
	close (last_pipe_input);
      
      /* Exec the program.  */

      (*func) (program, argv);

      perror_exec (program);

      exit (-1);

      /* NOTREACHED */

    default:
      /* 
       ** In the parent, after forking.
       ** Close the descriptors that we made for this child.  
       */

      if (input_desc != STDIN_FILE_NO)
	close (input_desc);

      if (output_desc != STDOUT_FILE_NO)
	close (output_desc);

      /* Return child's process number.  */

      return pid;
    }
#else	/* not HAVE_FORK */

  return (*func)(P_WAIT, program, argv); /* wait until completed. */

#endif
}

/* 
** Execute the command specified by the arguments on the current line of spec.
** When using pipes, this includes several piped-together commands
** with `|' between them.
**
** Return 0 if successful, -1 if failed.  
*/
int execute (void)
{
  int 	i, j;
  int 	n_commands;		/* # of command.  */
  char 	*string;
  struct command
    {
      char 	*prog;		/* program name.  */
      char 	**argv;		/* vector of args.  */
      int 	pid;		/* pid of process for this command.  */
    };

  struct command *commands;	/* each command buffer with above info.  */

  /* Count # of piped commands.  */

  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      n_commands++;

  /* Get storage for each command.  */
  commands = (struct command *) xmalloc (n_commands * sizeof (struct command));

  /* 
   ** Split argbuf into its separate piped processes,
   ** and record info about each one.
   ** Also search for the programs that are to be run.  
   */

  commands[0].prog = argbuf[0]; /* first command.  */
  commands[0].argv = &argbuf[0];

  string = find_exec_file (commands[0].prog);

  if (string)
    commands[0].argv[0] = string;

  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      {				/* each command.  */
	argbuf[i] = 0;	/* termination of command args.  */

	commands[n_commands].prog = argbuf[i + 1];
	commands[n_commands].argv = &argbuf[i + 1];

	string = find_exec_file (commands[n_commands].prog);

	if (string)
	  commands[n_commands].argv[0] = string;

	n_commands++;
      }

  argbuf[argbuf_index] = 0;

  /* If -v, print what we are about to do, and maybe query.  */

  if (vflag)
    {
      /* Print each piped command as a separate line.  */

      for (i = 0; i < n_commands ; i++)
	{
	  char **j;

	  for (j = commands[i].argv; *j; j++)
	    fprintf (stderr, " %s", *j);

	  /* Print a pipe symbol after all but the last command.  */
	  
	  if (i + 1 != n_commands)
	    fprintf (stderr, " |");
	  fprintf (stderr, "\n");
	}

      fflush (stderr);

#ifdef DEBUG
      fprintf (stderr, "\nGo ahead? (y or n) ");
      fflush (stderr);
      j = getchar ();
      if (j != '\n')
	while (getchar () != '\n') ;
      if (j != 'y' && j != 'Y')
	return 0;
#endif /* DEBUG */
    }

  /* Run each piped subprocess.  */

  last_pipe_input = STDIN_FILE_NO;

  for (i = 0; i < n_commands; i++)
    {
      char *string = commands[i].argv[0];

#ifdef DEBUG
      fprintf(stderr, "Program ... %s\n", commands[i].prog);
#endif

#ifdef HAVE_FORK

      commands[i].pid = pexecute ((string != commands[i].prog ? 
				   (FORK)execv : (FORK)execvp), 
				  string,   commands[i].argv, 
				  i + 1 < n_commands);
#else /* not HAVE_FORK */

	commands[i].pid = pexecute (string != commands[i].prog ? 
				  spawnv : spawnvp, string, 
				  commands[i].argv, i + 1 < n_commands);
#endif

      if (string != commands[i].prog)
	free (string);
    }

  execution_count++;

  /* 
   ** Wait for all the subprocesses to finish.
   ** We don't care what order they finish in;
   ** we know that N_COMMANDS waits will get them all.  
   */
  {
    int ret_code = 0;

    for (i = 0; i < n_commands; i++)
      {
	int 	status;
	int 	pid;

#ifdef HAVE_FORK

	char 	*prog;

	/* https://sourceware.org/ml/libc-alpha/2016-02/msg00342.html
	sys/wait.h union structure is deprecated and removed - use the USG alternative
	#ifdef USG */
	pid = wait (&status);
	/* #else
	union wait WaitUnion;

	pid = wait (&WaitUnion);
	status = WaitUnion.w_status;
	#endif */

	if (pid < 0)
	  abort ();

	if (status != 0)
	  {
	    int j;

	    for (j = 0; j < n_commands; j++)
	      if (commands[j].pid == pid)
		prog = commands[j].prog;

	    if ((status & 0x7F) != 0)
	      fatal ("program `%s' got fatal signal %d.", 
		     prog, (status & 0x7F));

	    if (((status & 0xFF00) >> 8) >= MIN_FATAL_STATUS)
	      ret_code = -1;
	  }

#else  /* not HAVE_FORK */

	status = commands[i].pid;

	if (status < 0)
	  abort ();

	if (status != 0)
	  {
	    if (vflag)
	      fatal ("program `%s' terminated with status %d.", 
		     commands[i].prog, status);
	    else
	      ret_code = -1;
	  }

#endif

      }
    return ret_code;
  }
}

/* 
** Find all the switches given to us
** and make a vector describing them.
** The elements of the vector a strings, one per switch given.
** If a switch uses the following argument, then the `part1' field
** is the switch itself and the `part2' field is the following argument.
** The `valid' field is nonzero if any spec has looked at this switch;
** if it remains zero at the end of the run, it must be meaningless.  
*/
struct switchstr 	*switches;
int 		n_switches;

/* 
** Also a vector of input files specified.  
*/
char 	**infiles;
int 	n_infiles;

/* 
** And a vector of corresponding output files is made up later.  
*/
char 	**outfiles;

/* 
** Create the vector `switches' and its contents.
** Store its length in `n_switches'.  
*/
void process_command(int argc, char **argv)
{
  register int i;

  n_switches = 0;
  n_infiles = 0;

  env_exec_prefix = getenv (ENV_EXEC_PREFIX);
  /* 
   ** Scan argv twice.  Here, the first time, just count how many switches
   ** there will be in their vector, and how many input files in theirs.
   ** Here we also parse the switches that cc itself uses (e.g. -v).  
   */
  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{
	  register char *p = &argv[i][1];
	  register int 	c = *p;

	  switch (c)
	    {
	    case 'b':
	      machine_suffix = p + 1;
	      break;

	    case 'B':
	      user_exec_prefix = p + 1;
	      break;

	    case 'V':	/* Print our subcommands and print versions.  */
	      vflag++;
/* lhh	      n_switches++; */
	      break;

	    case 'v':	/* Print our subcommands and print versions.  */
#ifndef __MSDOS__
	      fprintf(stderr, "%s\n", VERSION);
#endif
	      n_switches++;
	      break;

	    default:
	      n_switches++;

	      if (SWITCH_TAKES_ARG (c) && p[1] == 0)
		i++;
	      else if (WORD_SWITCH_TAKES_ARG (p))
		i++;
	    }
	}
      else
	n_infiles++;
    }

  /* Then create the space for the vectors and scan again.  */

  switches = ((struct switchstr *)
	      xmalloc ((n_switches + 1) * sizeof (struct switchstr)));
  infiles = (char **) xmalloc ((n_infiles + 1) * sizeof (char *));
  n_switches = 0;
  n_infiles = 0;

  /* 
   ** This, time, copy the text of each switch and store a pointer
   ** to the copy in the vector of switches.
   ** Store all the infiles in their vector.  
   */
  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{ 	
	  register char *p = &argv[i][1];
	  register int c = *p;

	  if (c == 'B' || c == 'b')
	    continue;
	  switches[n_switches].part1 = p;
	  if ((SWITCH_TAKES_ARG (c) && !p[1]) || WORD_SWITCH_TAKES_ARG (p))
	    switches[n_switches].part2 = argv[++i];
	  else
	    switches[n_switches].part2 = 0;
	  switches[n_switches].valid = 0;
	  n_switches++;
	}
      else
	infiles[n_infiles++] = argv[i];
    }

  switches[n_switches].part1 = 0;
  infiles[n_infiles] = 0;
}

/* 
** Process a spec string, accumulating and running commands.  
*/

/* 
** These variables describe the input file name.
** input_file_number is the index on outfiles of this file,
** so that the output file name can be stored for later use by %o.
** input_basename is the start of the part of the input file
** sans all directory names, and basename_length is the number
** of characters starting there excluding the suffix .c or whatever.  
*/

char 	*input_filename;
int 	input_file_number;
int 	input_filename_length;
int 	basename_length;
char 	*input_basename;

/* These are variables used within do_spec and do_spec_1.  */

/* 
** Nonzero if an arg has been started and not yet terminated
** (with space, tab or newline).  
*/
int arg_going;

/* 
** Nonzero means %d or %g has been seen; the next arg to be terminated
** is a temporary file name.  
*/
int delete_this_arg;

/* 
** Nonzero means %w has been seen; the next arg to be terminated
** is the output file name of this compilation.  
*/
int this_is_output_file;

/* 
** Nonzero means %s has been seen; the next arg to be terminated
** is the name of a library file and we should try the standard
** search dirs for it.  
*/
int this_is_library_file;

/* 
** Process the spec SPEC and run the commands specified therein.
** Returns 0 if the spec is successfully processed; -1 if failed.  
*/
int do_spec (char *spec)
{
  int value;

#ifdef DEBUG 
  fprintf(stderr, "do_spec : spec ... %s\n", spec);
#endif

  clear_args();
  arg_going 			= 0;
  delete_this_arg 	= 0;
  this_is_output_file = 0;
  this_is_library_file= 0;

  value = do_spec_1 (spec, 0);

  /* 
   ** Force out any unfinished command.
   ** If -pipe, this forces out the last command if it ended in `|'.  
   */

#ifdef DEBUG 
  fprintf(stderr, "do_spec : value ... %d\n", value);
#endif

  if (value == 0)
    {

#ifdef DEBUG
      fprintf(stderr, "do_spec : argbuf_index ... %d\n", argbuf_index);
#endif
      
      if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	argbuf_index--;

#ifdef DEBUG
      fprintf(stderr, "do_spec : argbuf_index ... %d\n", argbuf_index);
#endif
      if (argbuf_index > 0)
	value = execute ();
    }
  return value;
}

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
int do_spec_1 (char *spec, int inswitch)
{
  register char 	*p = spec;
  register int 	c;
  char 			*string;

  while ((c = *p++) != 0)
    /* 
     ** If substituting a switch, treat all chars like letters.
     ** Otherwise, NL, SPC, TAB and % are special.  
     */

    switch (inswitch ? 'a' : c)
      {
      case '\n':
	/* 
	 ** End of line: finish any pending argument,
	 ** then run the pending command if one has been started.  
	 */

	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);

#ifdef DEBUG
	    fprintf(stderr, "do_spec_1 : string ... %s\n", string);
#endif

	    if (this_is_library_file)
	      string = find_file (string);

	    store_arg (string, delete_this_arg, this_is_output_file);

	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }

	arg_going = 0;

	if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	  {
	    int i;

	    for (i = 0; i < n_switches; i++)
	      if (!strcmp (switches[i].part1, "pipe"))
		break;

	    /* 
	     ** A `|' before the newline means use a pipe here,
	     ** but only if -pipe was specified.
	     ** Otherwise, execute now and don't pass the `|' as an arg.
	     */

	    if (i < n_switches)
	      {
		switches[i].valid = 1;
		break;
	      }
	    else
	      argbuf_index--;
	  }

	if (argbuf_index > 0)
	  { 	
	    int value = execute ();
		
	    if (value)
	      return value;
	  }

	/* 
	 ** Reinitialize for a new command, and for a new argument.  
	 */
	
	clear_args ();

	arg_going = 0;
	delete_this_arg	= 0;
	this_is_output_file = 0;
	this_is_library_file= 0;

	break;

      case '|':
	/* End any pending argument.  */

	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);

	    if (this_is_library_file)
	      string = find_file (string);

	    store_arg (string, delete_this_arg, this_is_output_file);

	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }

	/* Use pipe */
	obstack_1grow (&obstack, c);
	arg_going = 1;

	break;

      case '\t':
      case ' ':
	/* Space or tab ends an argument if one is pending.  */

	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);

	    if (this_is_library_file)
	      string = find_file (string);

	    store_arg (string, delete_this_arg, this_is_output_file);
	    
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }

	/* Reinitialize for a new argument.  */

	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file= 0;

	break;

      case '%':
	switch (c = *p++)
	  {
	  case 0:
	    fatal ("bug: invalid specification.");

	  case 'b':
	    obstack_grow (&obstack, input_basename,basename_length);
	    arg_going = 1;
	    break;

	  case 'd':
	    delete_this_arg = 2;
	    break;

	  case 'e':
	    /* {...:%efoo} means report an error with `foo' as 
	     ** error message and don't execute any more commands 
	     ** for this file.  
	     */
	    {
	      char *q = p;
	      char *buf;
	      
	      while (*p != 0 && *p != '\n')
		p++;

	      buf = (char *) xmalloc (p - q + 1);
	      strncpy (buf, q, p - q);
	      *(buf + (p - q)) = 0;
	      error ("%s", buf);
	      return -1;
	    }
	    break;

	  case 'g':
	    obstack_grow (&obstack, temp_filename,temp_filename_length);
	    delete_this_arg = 1;
	    arg_going = 1;
	    break;

	  case 'i':
	    obstack_grow (&obstack, input_filename,
			  input_filename_length);
	    arg_going = 1;
	    break;

	  case 'o':
	    { 
	      register int f;

	      for (f = 0; f < n_infiles; f++)
		store_arg (outfiles[f], 0, 0);
	    }
	    break;

	  case 's':
	    this_is_library_file = 1;
	    break;

	  case 'w':
	    this_is_output_file = 1;
	    break;

	  case '{':
	    p = handle_braces (p);

	    if (p == 0)
	      return -1;
	    break;

	  case '%':
	    obstack_1grow (&obstack, '%');
	    break;

/*** The rest just process a certain constant string as a spec.  */
	    
	  case '1':
	    do_spec_1 (CC1_SPEC, 0);
	    break;

	  case 'a':
	    do_spec_1 (ASM_SPEC, 0);
	    break;

	  case 'c':
	    do_spec_1 (SIGNED_CHAR_SPEC, 0);
	    break;

	  case 'C':
	    do_spec_1 (CPP_SPEC, 0);
	    break;

	  case 'l':
	    do_spec_1 (LINK_SPEC, 0);
	    break;

	  case 'L':
	    do_spec_1 (LIB_SPEC, 0);
	    break;

	  case 'p':
	    do_spec_1 (CPP_PREDEFINES, 0);
	    break;

	  case 'P':
	    {
	      char *x = (char *) xmalloc (strlen(CPP_PREDEFINES)*2+1);
	      char *buf = x;
	      char *y = CPP_PREDEFINES;

	      /* 
	       ** Copy all of CPP_PREDEFINES into BUF,
	       ** but put __ after every -D and at the end of 
	       ** each arg,  
	       */

	      while (1)
		{
		  if (! strncmp (y, "-D", 2))
		    {
		      *x++ = '-';
		      *x++ = 'D';
		      *x++ = '_';
		      *x++ = '_';
		      y += 2;
		    }
		  else if (*y == ' ' || *y == 0)
		    {
		      *x++ = '_';
		      *x++ = '_';

		      if (*y == 0)
			break;
		      else
			*x++ = *y++;
		    }
		  else
		    *x++ = *y++;
		}
	      *x = 0;

	      do_spec_1 (buf, 0);
	    }
	    break;

	  case 'S':
	    do_spec_1 (STARTFILE_SPEC, 0);
	    break;
	    
	  default:
	    abort ();
	  }
	break;

      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }
  return 0;		/* End of string */
}

/* 
** Return 0 if we call do_spec_1 and that returns -1.  
*/
char *handle_braces (char *p)
{
  register char 	*q;
  char 			*filter;
  int 			pipe 	= 0;
  int 			negate 	= 0;

  if (*p == '|')
    /* 
     ** A `|' after the open-brace means,
     ** if the test fails, output a single minus sign rather than nothing.
     ** This is used in %{|!pipe:...}.  
     */

    pipe = 1, ++p;

  if (*p == '!')
    /* 
     ** A `!' after the open-brace negates the condition:
     ** succeed if the specified switch is not present.  
     */
    negate = 1, ++p;

  filter = p;
  while (*p != ':' && *p != '}') 
    p++;

  if (*p != '}')
    {
      register int count = 1;

      q = p + 1;

      while (count > 0)
	{
	  if (*q == '{')
	    count++;
	  else if (*q == '}')
	    count--;
	  else if (*q == 0)
	    abort ();

	  q++;
	}
    }
  else
    q = p + 1;

  if (p[-1] == '*' && p[0] == '}')
    {
      /* Substitute all matching switches as separate args.  */

      register int i;

      --p;

      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter))
	  give_switch (i);
    }
  else
    {
      /* Test for presence of the specified switch.  */

      register int i;
      int present = 0;

      /* 
       ** If name specified ends in *, as in {x*:...},
       ** check for presence of any switch name starting with x.  
       */

      if (p[-1] == '*')
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      if (!strncmp (switches[i].part1, filter, p - filter - 1))
		{
		  switches[i].valid = 1;
		  present = 1;
		}
	    }
	}
      /* Otherwise, check for presence of exact name specified.  */
      else
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      if (!strncmp (switches[i].part1, filter, p - filter)
		  && switches[i].part1[p - filter] == 0)
		{
		  switches[i].valid = 1;
		  present = 1;
		  break;
		}
	    }
	}
      
      /* 
       ** If it is as desired (present for %{s...}, absent for %{-s...})
       ** then substitute either the switch or the specified
       ** conditional text.  
       */

      if (present != negate)
	{
	  if (*p == '}')
	    {
	      give_switch (i);
	    }
	  else
	    {
	      if (do_spec_1 (save_string (p + 1, q - p - 2), 0) < 0)
		return 0;
	    }
	}
      else if (pipe)
	{
	  /* 
	   ** Here if a %{|...} conditional fails: output a minus sign,
	   ** which means "standard output" or "standard input".  
	   */
	  do_spec_1 ("-", 0);
	}
    }

  return q;
}

/* 
** Pass a switch to the current accumulating command
** in the same form that we received it.
** SWITCHNUM identifies the switch; it is an index into
** the vector of switches gcc received, which is `switches'.
** This cannot fail since it never finishes a command line.  
*/
void give_switch (int switchnum)
{
  do_spec_1 ("-", 0);
  do_spec_1 (switches[switchnum].part1, 1);
  do_spec_1 (" ", 0);
  if (switches[switchnum].part2 != 0)
    {
      do_spec_1 (switches[switchnum].part2, 1);
      do_spec_1 (" ", 0);
    }
  switches[switchnum].valid = 1;
}

/* 
** Search for a file named NAME trying various prefixes including the
** user's -B prefix and some standard ones.
** Return the absolute pathname found.  If nothing is found, return NAME.  
*/
char *find_file (char *name)
{
  int 	size;
  char	*temp;
  int 	win = 0;

  /* Compute maximum size of NAME plus any prefix we will try.  */

  size = standard_exec_prefix ?  strlen (standard_exec_prefix) : 0;

  if (user_exec_prefix != 0 && strlen (user_exec_prefix) > size)
    size = strlen (user_exec_prefix);

  if (env_exec_prefix != 0 && strlen (env_exec_prefix) > size)
    size = strlen (env_exec_prefix);

  if (standard_exec_prefix != 0 && strlen (standard_exec_prefix) > size)
    size = strlen (standard_exec_prefix);

  if (standard_exec_prefix_1 != 0 && strlen (standard_exec_prefix_1) > size)
    size = strlen (standard_exec_prefix_1);

  if (strlen (standard_startfile_prefix) > size)
    size = strlen (standard_startfile_prefix);

  if (strlen (standard_startfile_prefix_1) > size)
    size = strlen (standard_startfile_prefix_1);
  
  if (strlen (standard_startfile_prefix_2) > size)
    size = strlen (standard_startfile_prefix_2);

  if (machine_suffix)
    size += strlen (machine_suffix) + 1;

  size += strlen (name) + 1;

  temp = (char *) xmalloc (size);

  if (user_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, user_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win && env_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, env_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win && standard_exec_prefix)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win && standard_exec_prefix_1)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_exec_prefix_1);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }
  
  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix_1);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix_1);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }

  if (!win)
    {
      if (machine_suffix)
	{
	  strcpy (temp, standard_startfile_prefix_2);
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
	{
	  strcpy (temp, standard_startfile_prefix_2);
	  strcat (temp, name);
	  win = (access (temp, R_OK) == 0);
	}
    }
  
  if (!win)
    {
      if (machine_suffix)
	{
#ifdef __MSDOS__
	  strcpy (temp, "");
#else
	  strcpy (temp, "./");
#endif
	  strcat (temp, name);
	  strcat (temp, machine_suffix);
	  win = (access (temp, R_OK) == 0);
	}
      if (!win)
		{
#ifdef __MSDOS__
		  strcpy (temp, "");
#else
		  strcpy (temp, "./");
#endif
		  strcat (temp, name);
		  win = (access (temp, R_OK) == 0);
		}
    }

  if (win)
    return save_string (temp, strlen (temp));

  return name;
}

/* 
** On fatal signals, delete all the temporary files.  
*/
void fatal_error (int signum)
{
  signal (signum, SIG_DFL);
  delete_failure_queue ();
  delete_temp_files ();

  /* 
   ** Get the same signal again, this time not handled,
   ** so its normal effect occurs.  
   */

#ifdef HAVE_FORK
  kill (getpid (), signum);
#endif
}

int main(int argc, char **argv)
{
  register int 	i;
  int value;
  int error_count 	= 0;
  int linker_was_run 	= 0;
  char *pt;    
  char *explicit_link_files;

  programname = argv[0];

#ifdef DEBUG
  fprintf(stderr, "me: %s\n", programname);
#endif

  /* find the products home directory. */

  home = (char *)xmalloc(strlen(argv[0]));
  strcpy (home, programname);

  for (pt = home + strlen(home); pt != home && *pt != DIRDELIMITER; pt--);

  if (pt != home)
    *(pt+1) = 0;
  else
    *pt = 0;

#ifdef __MSDOS__

  /* get the process' PATH variable. */

  env_path = getenv("PATH");

#endif

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_error);

#ifndef __TURBOC__
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_error);
#endif

  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_error);

  argbuf_length = 10;
  argbuf = (char **) xmalloc (argbuf_length * sizeof (char *));

  obstack_init (&obstack);

  choose_temp_base ();

  /* 	
   ** Make a table of what switches there are (switches, n_switches).
   ** Make a table of specified input files (infiles, n_infiles).  
   */

  process_command (argc, argv);

#ifdef __MSDOS__
  fprintf (stderr, "%s\n", VERSION);
#endif

  if (vflag)
    {
#ifndef __MSDOS__
      fprintf (stderr, "%s\n", VERSION);
#endif

      if (n_infiles == 0)
	exit (0);
    }

  if (n_infiles == 0)
    fatal (USAGE);

  /* 	
   ** Make a place to record the compiler output file names
   ** that correspond to the input files.  
   */

  outfiles = (char **) xmalloc (n_infiles * sizeof (char *));
  bzero (outfiles, n_infiles * sizeof (char *));

  /* Record which files were specified explicitly as link input.  */
  
  explicit_link_files = (char *) xmalloc (n_infiles);
  bzero (explicit_link_files, n_infiles);

  for (i = 0; i < n_infiles; i++)
    {
      register struct compiler *cp;
      int this_file_error = 0;

      /* Tell do_spec what to substitute for %i.  */

      input_filename = infiles[i];
      input_filename_length = strlen (input_filename);
      input_file_number = i;

      /* Use the same thing in %o, unless cp->spec says otherwise. */

      outfiles[i] = input_filename;

      /* Figure out which compiler from the file's suffix.  */

      for (cp = compilers; cp->spec; cp++)
	{
	  if (strlen (cp->suffix) < input_filename_length &&
	      !strcmp (cp->suffix, 
		       infiles[i] + input_filename_length
		       -strlen (cp->suffix)))
	    {
	      /* 	
	       ** Ok, we found an applicable compiler.  Run its spec.
	       ** First say how much of input_filename to substitute 
	       ** for %b  
	       */
	      register char *p;

	      input_basename = input_filename;

	      for (p = input_filename; *p; p++)
		if (*p == DIRDELIMITER)
		  input_basename = p + 1;

	      basename_length = 
		(input_filename_length - strlen (cp->suffix)
		 - (input_basename - input_filename));

	      value = do_spec (cp->spec);
	      
	      if (value < 0)
		this_file_error = 1;

	      break;
	    }
	}

      /* 
       ** If this file's name does not contain a recognized suffix,
       ** record it as explicit linker input.  
       */

      if (! cp->spec)
	explicit_link_files[i] = 1;

      /* 	
       ** Clear the delete-on-failure queue, deleting the files in it
       ** if this compilation failed.  
       */

      if (this_file_error)
	{
	  delete_failure_queue ();
	  error_count++;
	}

      /* If this compilation succeeded, don't delete those  files later.  */

      clear_failure_queue ();
    }

  /* Run ld to link all the compiler output files.  */

  if (error_count == 0)
    {
      int tmp = execution_count;
      value = do_spec (link_spec);

      if (value < 0)
	error_count = 1;

      linker_was_run = (tmp != execution_count);
    }

  /* If options said don't run linker,
     complain about input files to be given to the linker.  */

  if (! linker_was_run && error_count == 0)
    for (i = 0; i < n_infiles; i++)
      if (explicit_link_files[i])
	error ("file `%s' not processed.", outfiles[i]);

  /* Set the `valid' bits for switches that match anything in any spec.  */

  validate_all_switches ();

  /* Warn about any switches that no pass was interested in.  */
  
  for (i = 0; i < n_switches; i++)
    if (! switches[i].valid)
      error ("unrecognized switch `-%s'.", switches[i].part1);

  /* Delete some or all of the temporary files we made.  */

  if (error_count)
    delete_failure_queue ();

  delete_temp_files ();

  exit (error_count);
}

void *xmalloc(int size)
{
  void *value = malloc(size);

  if (value == 0)
    fatal ("out of memory.");

  return value;
}

void *xrealloc(void *ptr, int size)
{
  void *value = realloc (ptr, size);

  if (value == 0)
    fatal ("out of memory.");

  return value;
}

/* 	
** Return a newly-allocated string whose contents concatenate those of 
** s1, s2, s3.  
*/
char *concat(char *s1, char *s2, char *s3)
{
  int 	len1 	= strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char 	*result = (char*)xmalloc(len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

char *save_string(char *s, int len)
{
  char *result = (char *) xmalloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;

  return result;
}

void pfatal_with_name(char *name)
{
  extern int 	errno, sys_nerr;
  //lhh extern char *sys_errlist[];
  char 		*s;

  if (errno < sys_nerr)
    s = concat ("%s: ", sys_errlist[errno], "");
  else
    s = "unable to open file `%s'.";

  fatal (s, name);
}

void perror_with_name (char *name)
{
  extern int 	errno, sys_nerr;
  //lhh extern char *sys_errlist[];
  char 		*s;

  if (errno < sys_nerr)
    s = concat ("%s: ", sys_errlist[errno], "");
  else
    s = "unable to open `%s'.";

  error (s, name);
}

void perror_exec(char *name)
{
  extern int 	errno, sys_nerr;
  //lhh extern char *sys_errlist[];
  char 		*s;

  if (errno < sys_nerr)
    s = concat ("unable to exec %s: ",
		sys_errlist[errno], "");
  else
    s = "unable to exec %s";

  error (s, name);
}

/* 
** More 'friendly' abort that prints the line and file.
** config.h can #define abort fancy_abort if you like that sort of thing.  
*/
void fancy_abort(void)
{
  fatal ("internal compiler error.");
}

#ifdef HAVE_VPRINTF

/* Output an error message and exit */

void fatal (char *fmt, ...)
{
  va_list ap;
  
  fprintf(stderr, "fatal: ");

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  fprintf(stderr, "\n");

  delete_temp_files();

  exit (1);
}  

void error (char *fmt, ...) 
{ 
  va_list ap;

  if (vflag)
    fprintf (stderr, "%s: ", programname);
  else
    fprintf (stderr, "");

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  fprintf(stderr, "\n");
}

#else /* not HAVE_VPRINTF */

void fatal (char *msg, char *arg1, char *arg2)
{
  fprintf(stderr, "fatal: ");
  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");

  delete_temp_files (0);

  exit (1);
}

void error (char *msg, char *arg1, char *arg2)
{
  if (vflag)
    fprintf (stderr, "%s: ", programname);
  else
    fprintf (stderr, "");

  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");
}

#endif /* not HAVE_VPRINTF */

void validate_all_switches(void)
{
  struct compiler *comp;
  register char *p;
  register char c;

  for (comp = compilers; comp->spec; comp++)
    {
      p = comp->spec;
      while ((c = *p++) != 0)
	if (c == '%' && *p == '{')
	  /* We have a switch spec.  */
	  validate_switches (p + 1);
    }

  p = link_spec;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  /* Now notice switches mentioned in the machine-specific specs.  */

#ifdef ASM_SPEC
  p = ASM_SPEC;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef CPP_SPEC
  p = CPP_SPEC;
  
  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef SIGNED_CHAR_SPEC
  p = SIGNED_CHAR_SPEC;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef CC1_SPEC
  p = CC1_SPEC;
  
  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef LINK_SPEC
  p = LINK_SPEC;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef LIB_SPEC
  p = LIB_SPEC;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif

#ifdef STARTFILE_SPEC
  p = STARTFILE_SPEC;

  while ((c = *p++) != 0)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
#endif
}

/* 
** Look at the switch-name that comes after START
** and mark as valid all supplied switches that match it.  
*/
void validate_switches(char *start)
{
  register char 	*p = start;
  char 			*filter;
  register int 	i;

  if (*p == '|')
    ++p;

  if (*p == '!')
    ++p;

  filter = p;

  while (*p != ':' && *p != '}') 
    p++;

  if (p[-1] == '*')
    {
      /* Mark all matching switches as valid.  */
      --p;

      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter))
	  switches[i].valid = 1;
    }
  else
    {
      /* Mark an exact matching switch as valid.  */
      for (i = 0; i < n_switches; i++)
	{
	  if (!strncmp (switches[i].part1, filter, p - filter)
	      && switches[i].part1[p - filter] == 0)
	    switches[i].valid = 1;
	}
    }
}


