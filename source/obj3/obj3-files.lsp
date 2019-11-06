;; OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
;; OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
;; OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
;; TRIM Copyright (c) 1994,2001 Lutz Hamel
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

;; $Id: obj3-files.lsp,v 206.1 2003/09/26 13:06:11 kiniry Exp $

;;
;; OBJ3 source files
;;

(in-package :user)

(defconstant +version+ "OBJ3 (2.09) + TRIM")
(setq *version* '(:obj3)) 
(defun print_char (n ch)           ;; files "obj3-compile" and 
  (dotimes (i n) (princ ch)))      ;; "obj3-load" call this function

(defvar *obj3_files* '(
"lib/addr"                 
"lib/alt"                  
"lib/bt"                   
"lib/filecol"              
"lib/function"  
"lib/gctrace"   
"lib/hash" 
"lib/macros"  
"lib/obless" 
"lib/sigs" 
"lib/topo_sort" 
"lib/type-of-safe" 
                       
"basic/operator"             
"basic/optables"             
"basic/sort"                
"basic/sort_constraint" 
"basic/sort_order"  
"basic/term" 
"basic/term_print" 
"basic/utilities" 
"basic/variable" 

"modexp/modexp" 
"modexp/modexp_parse" 
"modexp/mapping" 
"modexp/modexp_eval"
"modexp/modexp_eval2"

"match/a"                   
"match/ac"                  
"match/acz"                 
"match/az"                
"match/c"                   
"match/cz"                  
"match/empty"  
"match/environment" 
"match/global_state" 
"match/match" 
"match/match_equation"    
"match/match_methods"  
"match/match_system" 
"match/state" 
"match/system" 
"match/theory" 
"match/theory_name" 
"match/theory_state" 
"match/z" 

"parser/parse_aux"    
"parser/parse_dict"   
"parser/parser" 

"prelude/obj3sp"       
"prelude/prelude" 

"rew/memo_rew"             
"rew/rew"                  
"rew/rule" 
"rew/rule_ring"
"rew/substitution" 

"rule_gen/ext_rule_gen"         

"tools/fns"          
"tools/obj_print"    
"tools/obj_trace"    
"tools/tools" 

"top/top" 
"top/apply"  
"top/ci"  
"top/misc" 
"top/print" 
"top/reader" 
"top/module"  
"top/module_eval" 
"top/module_basic" 
"top/module_other" 
"top/module_parse"  

))
