/*
/////////////////////////////////////////////////////////////////////////////
//  fileio.h
//
//  low-level file I/O functions.
//
//  *NOTE* this file is used in C and C++ files.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////
*/


#include "fileio.h"
#include "config.hxx"

EXTERN_C_BEGIN
#include <stdio.h>
#include <string.h>
EXTERN_C_END

#ifdef __IBMCPP__
#include <fcntl.h>
#define R_OK O_RDWR
#endif
 
#if defined(__MAC__) || defined(__THINK__)
#include <MacIO.h>
#endif

/* //////////////////////////////////////////////////////////////////////// */
#if defined( __NeXT__ )
EXTERN_C_BEGIN
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <libc.h>
EXTERN_C_END
/* //////////////////////////////////////////////////////////////////////// */

#elif defined( __LPI__ )
#define _SYSV3

EXTERN_C_BEGIN
int access (char *, int);
int write (int, char *, int);
int read (int, char *, int);
int close (int);

#include <sys/unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
EXTERN_C_END
/* //////////////////////////////////////////////////////////////////////// */
// #elif defined ( sun ) || defined ( _AIX ) || defined ( __sgi )
#elif defined ( unix )

EXTERN_C_BEGIN
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
EXTERN_C_END
#ifndef O_NDELAY
#define O_NDELAY 00100000
#endif

/* //////////////////////////////////////////////////////////////////////// */

#elif defined(__MSDOS__) || defined(__OS2__)
EXTERN_C_BEGIN
#include <fcntl.h>
#include <sys/stat.h>
#include <io.h>
EXTERN_C_END
#endif

#ifdef _WINDOWS
#include <windows.h>
#ifdef USE_OWL
#include <bwcc.h>
#endif
extern HWND hWnd;
#endif

#if defined(__TCPLUSPLUS__) || defined(__BORLANDC__)
#define R_OK 4
#define W_OK 2
#define X_OK 0
#endif

#if defined(__THINK__) || defined(__MAC__)
#ifndef R_OK
#define R_OK 4
#endif

#ifndef W_OK
#define W_OK 2
#endif

#ifndef X_OK
#define X_OK 0
#endif
#endif

#ifdef _WIN32
#include <sys/stat.h>
#include <fcntl.h>
#define R_OK 4
#endif

#ifndef MASK
#define MASK 0666
#endif

/* ///////////////////////////////////////////////////////////////////////// */

#if defined(__THINK__) || defined(__MAC__)
/*{*/
int access(char *path, int mode)
{
	return McRAccessOrStatFile(path);
}

/* ///////////////////////////////////////////////////////////////////////// */

int is_directory (char * filename)
{
	return McRIsDirectory(filename);
}
/*}*/
#else
/*{*/

/* ///////////////////////////////////////////////////////////////////////// */

char * find_base (char * filename)
{
	int len;
	int i;
	char * buf;

	assert(filename);
	len = strlen(filename);

        buf = (char *)malloc(len + 1);
        mem_assert(buf);

	strcpy(buf, filename);

	for (i = len-1; i >= 0; i--)
	{
		if (buf[i] == '.')
		{
			buf[i] = '\0';
			return buf;
		}
	}

	/* couldn't figure out the file extension -- abort. */

	fprintf(stderr, 
		"fatal -- illegal file extension of file `%s'.\n",
		filename);
	exit(1);
	return NULL;
}

/* ///////////////////////////////////////////////////////////////////////// */

int is_directory (char * filename)
{
#ifndef __cplusplus
	struct stat buf;
#endif

#if 0
	int i = strlen(filename);

	if (filename[i] == '\\' || filename[i] == '/')
	{
		filename[i] = '\0';
	}
#endif

#ifdef __cplusplus
	struct stat buf;
#endif

	if (stat(filename, &buf) == -1)
	{
		return 0;
	}

	if (buf.st_mode & S_IFDIR)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/*}*/
#endif

/* ///////////////////////////////////////////////////////////////////////// */

void kill_slash (char * filename)
{
	int i = strlen(filename) - 1;

#if defined(__MAC__) || defined(__THINK__)
	if (filename[i] == '\\' || filename[i] == '/' || filename[i] == ':')
#else
	if (filename[i] == '\\' || filename[i] == '/')
#endif
	{
		filename[i] = '\0';
	}
}

/* ///////////////////////////////////////////////////////////////////////// */

FILE * text_read_open (char * filename)
{
#ifndef _WINDOWS
	FILE * file_handle = (FILE *)0;
    if (filename) file_handle = fopen(filename,"r");
#else
	OFSTRUCT of;

	//FILE * file_handle = fdopen(OpenFile(filename, &of, OF_READ),"r");
	FILE * file_handle = fdopen(_lopen(filename, OF_READ),"r");

	if (file_handle)
		ftell(file_handle);
#endif

	return file_handle;
}

/* ///////////////////////////////////////////////////////////////////////// */
/* lhh: open the file in binary mode and then then create a file pointer --
   we need this for DOS/WINDOWS so that things like Cntrl-Z will be
   ignored in the input. */

FILE * text_read_raw_open (char * filename)
{
#ifndef _WINDOWS
	FILE * file_handle = (FILE *)0;

	if (filename) 
	  file_handle =  fdopen(open(filename, 
				     O_RDONLY, S_IWRITE|S_IREAD),
				"r");

#else
	OFSTRUCT of;

	//FILE * file_handle = fdopen(OpenFile(filename, &of, OF_READ),"r");
	//FILE * file_handle = fdopen(_lopen(filename, OF_READ),"r");
	FILE * file_handle = fdopen(open(filename, 
					 O_RDONLY|O_TEXT,S_IWRITE|S_IREAD),
				    "r");

	if (file_handle)
		ftell(file_handle);
#endif

	return file_handle;
}

/* ///////////////////////////////////////////////////////////////////////// */

FILE * text_write_open (char * filename, const char* mode)
{
#ifndef _WINDOWS
	FILE * file_handle = fopen(filename, mode);
#else
	OFSTRUCT of;

	//FILE * file_handle = fdopen(OpenFile(filename, &of, OF_CREATE), mode);
	//FILE * file_handle = fdopen(_lcreat(filename, 0), mode);
	FILE * file_handle = fopen(filename, mode);

	if (file_handle)
		ftell(file_handle);
#endif

	return file_handle;
}

/* ///////////////////////////////////////////////////////////////////////// */

void text_close (FILE * file_handle)
{
	fclose(file_handle);
}

/* ///////////////////////////////////////////////////////////////////////// */

int bin_write_open (char * filename)
{
#if defined(unix) || defined(__MAC__) || defined(__THINK__)
#if !defined(__THINK__) && !defined(__MAC__)
	int mymask = umask ( 0 );
	umask ( mymask );
#endif

#if defined(__MAC__) || defined(__THINK__)
        int fp = open (filename, O_RDWR | O_CREAT | O_TRUNC | O_NDELAY);
#else
        int fp = open (filename, O_RDWR | O_CREAT | O_TRUNC | O_NDELAY,
/*                                S_IWRITE | S_IREAD);*/
                                MASK - mymask);
#endif

#endif

#if defined(_WINDOWS) || defined(__MSDOS__) || defined(__OS2__) || defined(_WIN32)
	int fp = open (filename, O_TRUNC | O_CREAT | O_RDWR | O_BINARY, 
						S_IWRITE | S_IREAD);
#endif

	if (fp < 0)
		fp = 0;

	return fp;
}

/* ///////////////////////////////////////////////////////////////////////// */

int bin_read_open (char * filename)
{
#if defined(unix)
	int fp = open (filename, O_RDONLY | O_NDELAY, 0);
#elif defined(__MAC__) || defined(__THINK__)
	int fp = open (filename, O_RDONLY | O_NDELAY);
#endif

#if defined(_WINDOWS) || defined(__MSDOS__) || defined(__OS2__) || defined(_WIN32)
	int fp = open (filename, O_RDONLY | O_BINARY, 0);
#endif

	if (fp < 0) fp = 0;

	return fp;
}

/* ///////////////////////////////////////////////////////////////////////// */

void bin_close (int fp)
{
#if defined(_WINDOWS)
	_lclose(fp);
#else
	close(fp);
#endif
}

/* ///////////////////////////////////////////////////////////////////////// */

int bin_write(int fp, char *str, unsigned len)
{
	int i = write(fp, str, len);

	return i == len;
}

/* ///////////////////////////////////////////////////////////////////////// */

int bin_read (int fp, char *str, unsigned len)
{
	int i = read(fp, str, len);

	return i == len;
}

/* ///////////////////////////////////////////////////////////////////////// */

char * construct_filename (char * base, char * name)
{
	/*
	 * constructs a filename from `base' and `name' and
	 * then checks if this file is accessible.
	 * if the file is not READ accessible the function
	 * returns a NULL pointer.
	 */

	char * buf;
	int i;

	if (!base || !name)
	{
		fprintf(stderr, "fatal -- cannot construct a filename.");
		abort();
	}

	buf = (char *)malloc(strlen(base) + strlen(name) + 1);
	mem_assert(buf);

	/* make the directory name and check that it is indeed a directory */

	strcpy(buf, base);

	if (!is_directory(buf))
	{
		for (i = strlen(buf); i > 0; i--)
		{
#if defined(__THINK__) || defined(__MAC__)
			if (buf[i] == ':')
#else
			if (buf[i] == '/' || buf[i] == '\\')
#endif
			{
				break;
			}
		}

		buf[i] = '\0';
	}

	kill_slash(buf);

	/* make the whole name and check for accessibility */

	strcat(buf, name);

#ifdef DEBUG
	fprintf(stderr, "testing for file `%s'...", buf);
#endif

	if (access(buf, R_OK) == 0)
	{
#ifdef DEBUG
		fprintf(stderr, "readable.\n");
#endif
		return buf;
	}
	else
	{
#ifdef DEBUG
		fprintf(stderr, "access denied!\n");
#endif
		free(buf);

		return NULL;
	}
}


/* ///////////////////////////////////////////////////////////////////////// */

char * find_file_in_path (char * name)
{
	/*
	 * returns a NULL pointer if the file cannot be found in the 
	 * path. 
	 */

	char * path_sym = getenv("PATH");
	char * found_file = NULL;
	char * buf;
	char * pstart;
	char * pstop;
	int searching = TRUE;

	
	if (!path_sym)
	{
		fprintf(stderr, "fatal -- cannot find file `%s'.\n", name);
		abort();
	}

	buf = (char *)malloc(strlen(path_sym) + 1);
	mem_assert(buf);

	strcpy(buf, path_sym);

	pstart = buf;
	pstop = buf;

	while (searching)
	{
		while (*pstop && 
#if defined(__MSDOS__) || defined(__OS2__)
		       *pstop != ';'
#else
		       *pstop != ':' 
#endif
		       )
		{
			pstop++;
		}

		if (*pstop)
		{
			*pstop = '\0';
		}
		else
		{
			searching = FALSE;
		}

		if (found_file = construct_filename(pstart, name))
		{
			break;
		}

		pstop++;
		pstart = pstop;
	}

	return found_file;
}

/* ///////////////////////////////////////////////////////////////////////// */
