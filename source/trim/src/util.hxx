/////////////////////////////////////////////////////////////////////////////
//  util.hxx
//
//  the basic utility classes for TRIM.
//
//  Written by Lutz H. Hamel.
//  (c) copyright 1994.
/////////////////////////////////////////////////////////////////////////////

#ifndef UTIL_HXX
#define UTIL_HXX

#include "config.hxx"

/////////////////////////////////////////////////////////////////////////////
// a generic parameterized list -- array based.

#define LIST(name,elem_type)	 				\
								\
class name							\
{								\
	elem_type * 		list;				\
	long			list_ptr;			\
	long			size;				\
								\
public:								\
								\
	name (long sz)	 					\
		{ 						\
			list = new elem_type[size = sz];	\
			mem_assert(list);			\
			list_ptr = 0; 				\
		}						\
								\
	long Count(void)					\
		{ return list_ptr; }				\
								\
	elem_type & Get(int pos)				\
		{ 						\
			assert(pos >= 0 && pos < list_ptr); 	\
			return list[pos]; 			\
		}						\
								\
	elem_type Append(elem_type e)				\
		{						\
			assert(list_ptr != size);		\
			list[list_ptr++] = e;			\
			return list[list_ptr-1];		\
		}						\
								\
	void ListAppend(name * l)				\
		{						\
			assert(l);				\
			for (int i = 0; i < l->Count(); i++)	\
			{					\
				this->Append(l->Get(i));	\
			}					\
		}						\
								\
	elem_type Delete(void)					\
		{						\
			assert(list_ptr != 0);			\
			return list[--list_ptr];		\
		}						\
								\
	void Clear(void)					\
		{ list_ptr = 0; }				\
								\
	long GetSize(void)					\
		{ return size; }				\
								\
	~ name ()						\
		{;}						\
}

/////////////////////////////////////////////////////////////////////////////
// a generic parameterized linked list.

#define LLIST(name,elem_type) 						\
									\
class name##Rec								\
{									\
	elem_type 	elem;						\
	name##Rec *	next;						\
	name##Rec *	prev;						\
									\
public:									\
									\
	name##Rec (elem_type e)						\
		{ elem = e; next = NULL; prev = NULL; }			\
									\
	elem_type & GetElem (void)					\
		{ return elem; }					\
									\
	name##Rec *& GetNext (void)					\
		{ return next; }					\
									\
	name##Rec *& GetPrev (void)					\
		{ return prev; }					\
									\
	~ name##Rec ()							\
		{;}							\
};									\
									\
class name								\
{									\
	name##Rec *		root;					\
	name##Rec *		last;					\
	long			size;					\
									\
public:									\
									\
	name ()		 						\
		{ 							\
			root = last = NULL;				\
			size = 0;					\
		}							\
									\
	name (long) /* ignore size! */ 					\
		{ 							\
			root = last = NULL;				\
			size = 0;					\
		}							\
									\
	long Count(void)						\
		{ return size; }					\
									\
	name##Rec * Find (long pos)					\
		{ 							\
			name##Rec * p;					\
			long i;						\
			for (i = 0, p = root; 				\
			     i <= pos && p; 				\
			     i++, p = p->GetNext())			\
			{						\
				if (i == pos && p) return p;		\
			}						\
			abort();					\
		}							\
									\
	long FindElem (elem_type e)					\
		{ 							\
			name##Rec * p;					\
			int i;						\
			for (i = 0, p = root; 				\
			     i < size && p; 				\
			     i++, p = p->GetNext())			\
			{						\
				if (p->GetElem() == e)			\
				{					\
					return i;			\
				}					\
			}						\
			abort();					\
		}							\
									\
	elem_type & Get(int pos)					\
		{ 							\
			name##Rec * p = Find(pos);			\
			return p->GetElem();				\
		}							\
									\
	elem_type Append(elem_type e)					\
		{							\
			name##Rec * r = mem(new name##Rec (e));		\
			if (size == 0)					\
			{						\
				root = last = r;			\
			}						\
			else						\
			{						\
				last->GetNext() = r;			\
				r->GetPrev() = last;			\
				last = r;				\
			}						\
			size++;						\
			return r->GetElem();				\
		}							\
									\
	elem_type AppendPos (long pos, elem_type e)			\
		{							\
			if (size == 0 || pos == size -1)		\
			{						\
				return Append(e);			\
			}						\
			name##Rec * p = Find(pos);			\
			name##Rec * n = mem(new name##Rec (e));		\
			p->GetNext()->GetPrev() = n;			\
			n->GetNext() = p->GetNext();			\
			n->GetPrev() = p;				\
			p->GetNext() = n;				\
			size++;						\
			return n->GetElem();				\
		}							\
									\
	elem_type AppendElem (elem_type e_pos, elem_type e_ins)		\
		{							\
			long pos = FindElem(e_pos);			\
			return AppendPos(pos, e_ins);			\
		}							\
									\
	elem_type Delete (void)						\
		{							\
			if (size == 0)					\
			{						\
				abort();				\
				return NULL;				\
			}						\
			else if (size == 1)				\
			{						\
				elem_type e = root->GetElem();		\
				delete root;				\
				root = last = NULL;			\
				size = 0;				\
				return e;				\
			}						\
			else						\
			{						\
				elem_type e = last->GetElem();		\
				name##Rec * d = last;			\
				last = last->GetPrev();			\
				delete d;				\
				size--;					\
				return e;				\
			}						\
		}							\
									\
	elem_type DeletePos (long pos)					\
		{							\
			if (size == 0 || 				\
			    size == 1 || 				\
			    pos == size -1) 				\
			{						\
				return Delete();			\
			}						\
			else if (pos == 0)				\
			{						\
				name##Rec * p = root;			\
				root = root->GetNext();			\
				elem_type e = p->GetElem();		\
				delete p;				\
				size--;					\
				return e;				\
			}						\
			else						\
			{						\
				name##Rec * p = Find(pos);		\
				p->GetPrev()->GetNext() = p->GetNext();	\
				p->GetNext()->GetPrev() = p->GetPrev();	\
				elem_type e = p->GetElem();		\
				delete p;				\
				size--;					\
				return e;				\
			}						\
		}							\
									\
	elem_type DeleteElem (elem_type e)				\
		{							\
			long pos = FindElem(e);				\
			return DeletePos(pos);				\
		}							\
									\
	void Clear(void)						\
		{ 							\
			name##Rec * p = root;				\
			while(p)					\
			{						\
				name##Rec * tmp = p;			\
				p = p->GetNext();			\
				delete tmp;				\
			}						\
			size = 0;					\
		}							\
									\
	long GetSize(void)						\
		{ return size; }					\
									\
	~ name ()							\
		{;}							\
};

/////////////////////////////////////////////////////////////////////////////
// a generic parameterized stack.

#define	STACK(name,elem_type)					\
								\
LIST(__StackList##name,elem_type);				\
								\
class name : public __StackList##name				\
{								\
public:								\
	name (long max_size) :					\
			__StackList##name (max_size)		\
		{;}						\
								\
	void Push(elem_type e)					\
		{ Append(e); }					\
								\
	elem_type Pop(void)					\
		{ return Delete(); }				\
								\
	elem_type Top(void)					\
		{ return Get(Count() - 1); }			\
								\
	~ name ()						\
		{;}						\
}


/////////////////////////////////////////////////////////////////////////////
// define a string hash table.

LIST(StringList,String);

typedef StringList * StringListPtr;

LIST(StringListPtrList, StringListPtr);



class StringHash : public StringListPtrList
{
public:

	StringHash (long table_size, long bucket_size);

	long HashValue(String s);

	String PutString(String s);

	String GetString(String s);

	String FindString(String s);

	~ StringHash ()
		{;}
};

/////////////////////////////////////////////////////////////////////////////

inline
StringHash::StringHash (long table_size, long bucket_size) :
	StringListPtrList(table_size)
{
	for (int i = 0; i < table_size; i++)	
	{
		StringList * l = new StringList(bucket_size);
		mem_assert(l);

		Append(l);
	}
}

/////////////////////////////////////////////////////////////////////////////

inline
long StringHash::HashValue(String s)
{
	long l = strlen(s);
	long sum = 0;

	for (long i = 0; i < l; i++)
	{
		sum += s[i];
	}

	return sum % GetSize();	
}

/////////////////////////////////////////////////////////////////////////////

inline
String StringHash::PutString(String s)
{
	String new_str = new char[strlen(s) + 1];
	mem_assert(new_str);
	strcpy(new_str, s);

	return Get(HashValue(new_str))->Append(new_str);
}

/////////////////////////////////////////////////////////////////////////////

inline
String StringHash::GetString(String s)
{
	StringList * bucket = Get(HashValue(s));
	long n = bucket->Count();

	for (long i = 0; i < n; i++)
	{
		if (strcmp(s, bucket->Get(i)) == 0)
		{
			return bucket->Get(i);
		}
	}

	return NULL;
}

/////////////////////////////////////////////////////////////////////////////

inline
String StringHash::FindString(String s)
{
	String str = GetString(s);

	if (str)
	{	
		return str;
	}
	else
	{
		return PutString(s);
	}
}

/////////////////////////////////////////////////////////////////////////////
#endif /* UTIL_HXX */
