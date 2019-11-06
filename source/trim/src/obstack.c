/* obstack.c - subroutines used implicitly by object stack macros
   Copyright (C) 1988 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 1, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */

#include <stdlib.h>
#include <obstack.h>

/* Determine default alignment.  */
struct fooalign {char x; double d;};
#define DEFAULT_ALIGNMENT ((char *)&((struct fooalign *) 0)->d - (char *)0)
/* If malloc were really smart, it would round addresses to DEFAULT_ALIGNMENT.
   But in fact it might be less smart and round addresses to as much as
   DEFAULT_ROUNDING.  So we prepare for it to do that.  */
union fooround {long x; double d;};
#define DEFAULT_ROUNDING (sizeof (union fooround))

/* When we copy a long block of data, this is the unit to do it with.
   On some machines, copying successive ints does not work;
   in such a case, redefine COPYING_UNIT to `long' (if that works)
   or `char' as a last resort.  */
#ifndef COPYING_UNIT
#define COPYING_UNIT int
#endif

/* The non-GNU-C macros copy the obstack into this global variable
   to avoid multiple evaluation.  */

struct obstack *_obstack;

/* 
** Initialize an obstack H for use.  Specify chunk size SIZE (0 means default).
** Objects start on multiples of ALIGNMENT (0 means use default).
** CHUNKFUN is the function to use to allocate chunks,
** and FREEFUN the function to free them.  
*/
void _obstack_begin(
	struct obstack *h,
	int 	size,
	int 	alignment,
	POINTER (*chunkfun)(int),
	void 	(*freefun)(POINTER))
{
  register struct _obstack_chunk* chunk; /* points to new chunk */

  if (alignment == 0)
    alignment = DEFAULT_ALIGNMENT;

  if (size == 0)
    /* 
     ** Default size is what GNU malloc can fit in a 4096-byte block.
     ** Pick a number small enough that when rounded up to DEFAULT_ROUNDING
     ** it is still smaller than 4096 - 4.  
     */
    {
      int extra = 4;

      if (extra < DEFAULT_ROUNDING)
	extra = DEFAULT_ROUNDING;
      size = 4096 - extra;
    }

  h->chunkfun 	= chunkfun;
  h->freefun 		= freefun;
  h->chunk_size 	= size;
  h->alignment_mask = alignment - 1;

  chunk			= h->chunk 			= 
    (struct _obstack_chunk*)(*h->chunkfun) (h->chunk_size);
  	h->next_free 	= h->object_base 	= chunk->contents;
  	h->chunk_limit 	= chunk->limit 		= (char *) chunk + h->chunk_size;
  	chunk->prev = 0;
}

/* 
** Allocate a new current chunk for the obstack *H
** on the assumption that LENGTH bytes need to be added
** to the current object, or a new object of length LENGTH allocated.
** Copies any partial object from the end of the old chunk
** to the beginning of the new one.  
*/
void _obstack_newchunk (struct obstack *h, int length)
{
 	register struct _obstack_chunk*	old_chunk = h->chunk;
  	register struct _obstack_chunk*	new_chunk;
  	register long	new_size;
  	register int obj_size = h->next_free - h->object_base;
  	register int i;
  	int already;

  	/* Compute size for new chunk.  */
  	new_size = (obj_size + length) + (obj_size >> 3) + 100;
  	if (new_size < h->chunk_size)
    	new_size = h->chunk_size;

  	/* Allocate and initialize the new chunk.  */
  	new_chunk = h->chunk = (struct _obstack_chunk*)(*h->chunkfun) (new_size);
  	new_chunk->prev = old_chunk;
  	new_chunk->limit= h->chunk_limit = (char *) new_chunk + new_size;

  	/* Move the existing object to the new chunk.
    ** Word at a time is fast and is safe if the object
    ** is sufficiently aligned.  
	*/
  	if (h->alignment_mask + 1 >= DEFAULT_ALIGNMENT)
    {
      	for (i = obj_size / sizeof (COPYING_UNIT) - 1; i >= 0; i--)
			((COPYING_UNIT *)new_chunk->contents)[i]
	  			= ((COPYING_UNIT *)h->object_base)[i];
      	/* 
		** We used to copy the odd few remaining bytes as one extra 
		** COPYING_UNIT,
	 	** but that can cross a page boundary on a machine
	 	** which does not do strict alignment for COPYING_UNITS.  
		*/
      	already = obj_size / sizeof (COPYING_UNIT) * sizeof (COPYING_UNIT);
    }
  	else
    	already = 0;
  	/* Copy remaining bytes one by one.  */
  	for (i = already; i < obj_size; i++)
    	new_chunk->contents[i] = h->object_base[i];

  	h->object_base 	= new_chunk->contents;
  	h->next_free 	= h->object_base + obj_size;
}

/* 
** Return nonzero if object OBJ has been allocated from obstack H.
** This is here for debugging.
** If you use it in a program, you are probably losing.  
*/
int _obstack_allocated_p(struct obstack *h, POINTER obj)
{
  struct _obstack_chunk *lp;  /* below addr of any objects in this chunk */
  struct _obstack_chunk	*plp;  /* point to previous chunk if any */

  	lp = (h)->chunk;
  	while (lp != 0 && ((POINTER)lp > obj || (POINTER)(lp)->limit < obj))
    {
      	plp = lp -> prev;
      	lp 	= plp;
    }
  	return lp != 0;
}

/* 
** Free objects in obstack H, including OBJ and everything allocate
** more recently than OBJ.  If OBJ is zero, free everything in H.  
*/
void
#ifdef __STDC__
#undef obstack_free
 obstack_free(struct obstack *h, POINTER obj)
#else
_obstack_free(struct obstack *h, POINTER obj)
#endif
{
  struct _obstack_chunk	*lp;  /* below addr of any objects in this chunk */
  struct _obstack_chunk	*plp; /* point to previous chunk if any */

  	lp = (h)->chunk;
  	while (lp != 0 && ((POINTER)lp > obj || (POINTER)(lp)->limit < obj))
    {
      	plp = lp -> prev;
      	(*h->freefun) (lp);
      	lp = plp;
    }
  	if (lp)
    {
      	(h)->object_base = (h)->next_free = (char *)(obj);
      	(h)->chunk_limit = lp->limit;
      	(h)->chunk = lp;
    }
  	else if (obj != 0)
    	/* obj is not in any of the chunks! */
    	abort ();
}

/* Let same .o link with output of gcc and other compilers.  */

#ifdef __STDC__
void _obstack_free(struct obstack *h, POINTER obj)
{
  	obstack_free (h, obj);
}
#endif

/* These are now turned off because the applications do not use it
   and it uses bcopy via obstack_grow, which causes trouble on sysV.  */

/* Now define the functional versions of the obstack macros.
   Define them to simply use the corresponding macros to do the job.  */

#if  __STDC__ 
/* These function definitions do not work with non-ANSI preprocessors;
   they won't pass through the macro names in parentheses.  */

/* The function names appear in parentheses in order to prevent
   the macro-definitions of the names from being expanded there.  */

POINTER (obstack_base)(struct obstack *os)
{
  	return obstack_base(os);
}

POINTER (obstack_next_free)(struct obstack *os)
{
  	return obstack_next_free(os);
}

int (obstack_object_size)(struct obstack *os)
{
  	return obstack_object_size(os);
}

int (obstack_room)(struct obstack *os)
{
  	return obstack_room(os);
}

void (obstack_grow)(struct obstack *os, POINTER pointer, int length)
{
  	obstack_grow(os, pointer, length);
}

void (obstack_grow0)(struct obstack *os, POINTER pointer, int length)
{
  	obstack_grow0(os, pointer, length);
}

void (obstack_1grow)(struct obstack *os, int character)
{
  	obstack_1grow(os, character);
}

void (obstack_blank)(struct obstack *os, int length)
{
  	obstack_blank(os, length);
}

void (obstack_1grow_fast)(struct obstack *os, int character)
{
  	obstack_1grow_fast(os, character);
}

void (obstack_blank_fast)(struct obstack *os, int length)
{
  	obstack_blank_fast(os, length);
}

POINTER (obstack_finish)(struct obstack *os)
{
  	return obstack_finish(os);
}

POINTER (obstack_alloc)(struct obstack *os, int length)
{
  	return obstack_alloc(os, length);
}

POINTER (obstack_copy)(struct obstack *os, POINTER pointer, int length)
{
  	return obstack_copy(os, pointer, length);
}

POINTER (obstack_copy0)(struct obstack *os, POINTER pointer, int length)
{
  	return obstack_copy0(os, pointer, length);
}

#endif /* __STDC__ */
