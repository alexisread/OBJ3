/////////////////////////////////////////////////////////////////////////////
//  mem.cxx
//
//  memory management
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#include "config.hxx"

/////////////////////////////////////////////////////////////////////////////
#ifndef __LPI__
void * operator new (size_t i)
{
//	fprintf(stderr, "::new asking for %d bytes\n", i);

	// adjust the value so that it divisible by four

	i = i >> 2;
	i = (i << 2) + 4;

	assert(i < MEMORY_CACHE_SIZE);

	static char * cache = (char*) malloc(MEMORY_CACHE_SIZE);
	mem_assert(cache);

	static long cache_ix = 0;

	if ((cache_ix + i) >= MEMORY_CACHE_SIZE)
	{
		cache = (char*) malloc(MEMORY_CACHE_SIZE);
		mem_assert(cache);

		cache_ix = 0;
	}

	char * addr = &cache[cache_ix];
	cache_ix += i;

	return addr;
}

/////////////////////////////////////////////////////////////////////////////

void  operator delete (void *)
{
// we do nothing to save time.
}
#endif
/////////////////////////////////////////////////////////////////////////////


