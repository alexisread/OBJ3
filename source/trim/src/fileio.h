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

#ifndef FILEIO_H
#define FILEIO_H

/* //////////////////////////////////////////////////////////////////////// */

#ifdef __cplusplus
#define EXTERN_C_BEGIN 	extern "C" {	
#else
#define EXTERN_C_BEGIN 
#endif

#ifdef __cplusplus	
#define EXTERN_C_END 	}
#else
#define EXTERN_C_END
#endif

/* //////////////////////////////////////////////////////////////////////// */

EXTERN_C_BEGIN 

#include <stdio.h>

EXTERN_C_END 

/* //////////////////////////////////////////////////////////////////////// */

char * find_base (char * filename);

int is_directory (char * filename);

void kill_slash (char * filename);

char * find_file_in_path (char * name);

char * construct_filename (char * base, char * name);

char * make_filename (char* fname, char* extension);

FILE * text_read_open (char * filename);

FILE * text_read_raw_open (char * filename); 

FILE * text_write_open (char * filename, const char* mode);

void text_close (FILE * file_handle);

int bin_write_open (char * filename);

int bin_read_open (char * filename);

void bin_close (int fp);

int bin_write (int fp, char *str, unsigned len);

int bin_read (int fp, char *str, unsigned len);


#endif /* FILEIO_H */
