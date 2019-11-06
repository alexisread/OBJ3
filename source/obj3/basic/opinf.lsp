;; OBJ3 version 2
;; Copyright (c) 2000, Joseph Kiniry, Joseph Goguen, Sula Ma
;; Copyright (c) 1988,1991,1993 SRI International
;; All Rights Reserved
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;   o Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;; 
;;   o Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;; 
;;   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
;;   International, nor the names of its contributors may be used to
;;   endorse or promote products derived from this software without
;;   specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
;; INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;; $Id: opinf.lsp,v 206.1 2003/09/23 13:40:11 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specially optimized optable routines.
;;
;; DANGER -- this is VERY implementation dependent.
;;
;; This file defines a replacement definition for optable$operator_info
;; which is defined in optables and can be omitted.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Clines
"static object getopfinfofn;"

"struct htent {                  /*  hash table entry  */"
"        object  hte_key;        /*  key  */"
"        object  hte_value;      /*  value  */"
"};"

"struct hashtable {              /*  hash table header  */"
"        short   t, m;"
"        struct htent"
"                *ht_self;       /*  pointer to the hash table  */"
"        object  ht_rhsize;      /*  rehash size  */"
"        object  ht_rhthresh;    /*  rehash threshold  */"
"        int     ht_nent;        /*  number of entries  */"
"        int     ht_size;        /*  hash table size  */"
"        short   ht_test;        /*  key test function  */"
"                                /*  of enum httest  */"
"};"

"union lispunion1 {"
"	struct hashtable ht;"
"       object ob;  /* don't really need this */ "
"};"
)

(defCfun 
"object getopinfo(optable,op)
register union lispunion1 *optable; object op;"
0
"    int hsize;"
"    register struct htent *e;"
"    register int i, j, k;"
"    register object hkey;"
"    int found;"
"    object val;"
""
"    found = 0;"
"    j = -1;"
"    hsize = optable->ht.ht_size;"
"    i = ((int)op / 4) & 0x7fffffff;"
"    for (i %= hsize, k = 0; k < hsize;  k++) {"
"	e = &optable->ht.ht_self[i];"
"	hkey = e->hte_key;"
"	if (hkey == OBJNULL) {"
"	    if (e->hte_value == OBJNULL) {"
"		found = 1;"
"		if (0 <= j) e = &optable->ht.ht_self[j];"
"		break;"
"	    }"
"	    else"
"		if (j < 0) j = i;"
"	}"
"	else"
"	if (op == hkey) {"
"	    found = 1;"
"	    break;"
"	}"
"	i++;"
"	if (i == hsize) i = 0;"
"    }"
"    if (! found) e = &optable->ht.ht_self[j];"
""
"    if (e->hte_key != OBJNULL)"
"	val = e->hte_value;"
"    else"
"	val = Cnil;"
""
"    if(val==Cnil) {"
"	val = op->str.str_self[9];"
"	if(val==Cnil){"
"	    return simple_symlispcall_no_event(getopfinfofn,vs_base,0);"
"	}"
"	else return val;"
"    }"
"    else return val;"
)

(defentry optable$operator_info (object object) (object getopinfo))

(defCfun "initgetopi(x) object x;" 0
" getopfinfofn = x;"
" Creturn(Cnil);"
)

(defentry init-getopinfo (object) (object initgetopi))

(eval-when (load) (init-getopinfo 'operator_info$make))
