;; OBJ3 version 2
;; Modified 5/30/2001 by Lutz Hamel for TRIM support
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

;; $Id: module_parse.lsp,v 206.1 2003/09/26 13:04:26 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; schematic parser/reader
; uses reader.lsp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; created: June 16, 86 ;;;;

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op module_parse$parse : {*standard_input*} ->
;                          {*standard_input*} "pre-module"

; op general_read$reader : *standard_input* name schema ->
;                          {*standard_input*} parse_tree

; var *module_parse$module_schemas* : schemas describing [constant]
;   OBJ3 module structure; for use with general_read$reader

(proclaim '(special
    *module_parse$module_schemas*
    *general_read$$schema_env*))

(defvar *general_read$$void* '(void))
(defvar *general_read$$input* nil)

(defvar *general_read$$current_schema* nil)
(defvar *general_read$$current_context* nil)
(defvar *general_read$$starting-position* nil)

;;; top-level parser invokes general_read$reader with schemas for modules
; op module_parse$parse : {*standard_input*} ->
;                          {*standard_input*} "pre-module"
(defun module_parse$parse ()
  ;; *standard_input* -> list structure of structure read
  (general_read$reader 'Top_form *module_parse$module_schemas*))

;;;; GENERAL_READ -- schema based generalized reader

;;; SCHEMA -- definition of (see also general_read$$read)
;;; schemas match patterns
;;; choices must be determined by first symbol of context
;;;   e.g. if an optional element doesn't occur what occurs first
;;;     must be an explicit symbol in the schema
;;; (&optional PAT)  -- optional occurrence of pattern
;;; (&if_present PAT)  -- optional occurrence of pattern
;;;   &optional -- omit if next token matches following context
;;;   &if_present -- assume if first matches
;;; (&one_of PAT1 PAT2 ...) -- of of patterns determined by first of patterns
;;; (&one_of_default PAT1 PAT2 ...) ... with a default PAT1
;;; (&seq_of PAT) -- some number of repetitions of PAT
;;; (&many_of PAT1 PAT2 ...) -- roughly (&seq_of (&one_of PAT1 ...))
;;; &symbol -- a symbol
;;; &symbols -- (&seq_of &symbol)
;;; &int -- an integer
;;; &term -- sequence of tokens up to terminator which follows
;;; &sort -- a sort name (possibly qualified)
;;; &sorts -- (&seq_of &sort)
;;; &comment -- string of characters to end of current line
;;; &commentlong -- variation of above with provision for long comments
;;; (&+ a b c) -- any of several symbols
;;; (&& SCHEMA-NAME) -- match the named schema
;;; (&call LISP-EXPR) -- escape to Lisp
;;; (&rdr ("char1" "char2"...) PAT) -- make characters be single-char and
;;;   then match pattern, returning status of characters to previous when done
;;; &modexp -- read a term with balanced (/) [/] view/endv
;;;   particular tricky case: view A to B[view C to D is ... endv] is
;;; &obj_item -- read a (possibly parenthesized) item
;;; otherwise: symbol -- matches symbol
;;; (&pred PREDICATE) -- matches a token satisfying predicate
;;; &append PAT -- analogous to ,@; incorporate structure from PAT
;;;   into result "removing one set of parentheses"
;;; &upto -- specify ending context for repeated form

;;; read input from *standard_input, parse according to schema
; op general_read$reader : *standard_input* name schema ->
;                          {*standard_input*} parse_tree
(defun general_read$reader (name schms)
  (let ((*general_read$$schema_env* schms))
    (general_read$$!init)
    (general_read$$named name *general_read$$void*)))

;;; make a list of characters be single character symbols in the reader
;;;   return list of original status
(defun general_read$$!set_single_reader (l)
  (mapcar #'(lambda (x)
	    (let ((chr (if (stringp x) (char x 0) x)))
	    (prog1
	      (cons chr (reader$get_syntax chr))
	      (reader$!set_syntax chr (intern (string x))))))
    l))

;;; modify a sequence of characters for syntax as given by associated values
(defun general_read$$!set_reader (l)
  (mapc #'(lambda (x)
            (let ((s (car x)))
	    (reader$!set_syntax
	     (if (stringp s) (char s 0) s) (cdr x))))
    l))

;;;; Input Buffering for general reader
;;;;   the following routines create a single token buffer, which
;;;;   allows look-ahead

(defun general_read$$!in ()
  (when (eq *general_read$$input* *general_read$$void*)
    (setq *general_read$$input* (reader$read_sym))))

(defun general_read$$!discard ()
  (setq *general_read$$input* *general_read$$void*))

(defun general_read$$!sym ()
  (cond
    ((eq *general_read$$input* *general_read$$void*) (reader$read_sym))
    (t (prog1 *general_read$$input*
	 (general_read$$!discard)))))

(defun general_read$$at_eof (&optional (val *general_read$$input*))
  (eq *reader$eof_value* val))

(defun general_read$$eof_error ()
  (princ "Unexpected end of input") (terpri)
  (when *obj$input_source*
    (princ "filename: ") (princ *obj$input_source*)
    (terpri))
  (when (and *general_read$$current_schema*
	     (general_read$$is_simple_schema *general_read$$current_schema*))
    (princ "processing schema: ")
    (general_read$$display_schema *general_read$$current_schema*)
    (terpri))
  (if *general_read$$starting-position*
    (progn
      (princ "starting character position was: ")
      (prin1 *general_read$$starting-position*)
      (terpri)
      (obj3-return-to-top))
    (obj3-break)))

;;; basic string matching function
(defun string-match (x y)
  (and (stringp x) ; defensive for a reason
       (string= x (if (stringp y) y (string-downcase (string y)))))
  )

;;; used to match tokens against "patterns" which should be
;;;   either a symbol, string, or of one of the forms
;;;   (&+ a b c ...) or (&pred PRED)
; op general_read$$string_matches : token pattern -> Bool
(defun general_read$$string_matches (x y)
   (and (atom x)
	(if (atom y) (string-match x y)
	  (or
	   (and (eq '&pred (car y)) (funcall (cadr y) x))
	   (if (eq '&+ (car y)) ;@@@@
	     (member x (cdr y) :test #'string-match)
	     (member x y :test #'string-match))))))

;;; is a token an integer
; op general_read$$numberp : token -> Bool
(defun general_read$$numberp (str)
  (prog (p len)
    (setq p 0)
    (setq len (length str))
    (when (member (aref str p) '(#\+ #\-))
      (incf p)
      (when (= 1 len) (return nil)))
  loop_digits
    (when (= len p) (return t))
    (when (not (digit-char-p (aref str p))) (return nil))
    (incf p)
    (go loop_digits)
  ))

;;; the workhorse general read routine
;;;   it dispatches to routines to handle the various cases
; op general_read$$read : {*standard_input*} schema context ->
;                         {*standard_input*} parse_tree
(defun general_read$$read (schema context)
  (let ((*general_read$$current_schema* schema)
	(*general_read$$current_context* context)
	(*general_read$$starting-position*
         (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))))
  (cond ((null schema) nil)
  (t (let ((elt (car schema)) (rest (cdr schema)))
    (let ((restcontext (if rest rest context)))
    (cond
     ((symbolp elt)
     (case elt
      (&optional (general_read$$optional rest context))
      (&if_present (general_read$$if_present rest context))
      (&one_of (general_read$$one_of rest context))
      (&one_of_default (general_read$$one_of_default rest context))
      (&many_of ;like one_of but with repetitions
        (general_read$$many_of rest context))
      (&seq_of (general_read$$seq_of rest context))
      (&symbol (general_read$$continue (general_read$$!sym) rest context))
      (&symbols (general_read$$continue (general_read$$seq_of '(&symbol)
							    restcontext)
				       rest context))
      (&int (let ((val (general_read$$!sym)))
	(cond
	 ((general_read$$numberp val)
	  (general_read$$continue val rest context))
	 (t (format t "was expecting an integer not ~s" val)
	    (general_read$$show_context)
	    (obj3-return-to-top)))))
      (&term (general_read$$continue (general_read$$term restcontext)
				    rest context))
      (&sort (general_read$$continue (general_read$$sort restcontext)
				    rest context))
      (&sorts (general_read$$continue (general_read$$sorts restcontext)
				    rest context))
      (&comment (general_read$$continue (read-line) rest context))
      (&commentlong
       (general_read$$continue (general_read$$commentlong) rest context))
      (&+ (general_read$$any_one rest))
      (&& ; use named description
       (general_read$$named (car rest) context))
      (&call (eval (car rest)))
      (&append
       (let* ((rr (cdr rest))
	      (rc (if rr rr context)))
       (general_read$$continue_append
	(general_read$$read (car rest) rc) rr context)))
      (&rdr
       (let ((cur (general_read$$!set_single_reader (car rest))))
         (prog1 (general_read$$read (cdr rest) context)
           (general_read$$!set_reader cur))))
      (&modexp
       (general_read$$continue
	(general_read$$module_exp (car restcontext)) rest context))
      (&obj_item 
       (general_read$$!discard)
       (let ((val (reader$read)))
	 (let ((a (if (null (cdr val)) (car val) val)))
	   (general_read$$continue a rest context))))
      (otherwise
         (general_read$$!in)
	 (cond
	  ((string-match *general_read$$input* elt)
	   (let ((inp *general_read$$input*))
	     (general_read$$!discard)
	     (general_read$$continue inp rest context)))
	  (t (format t "Was expecting the symbol '~a' not '~a'"
		     elt *general_read$$input*)
	     (general_read$$show_context)
	     (obj3-return-to-top)))
        )
     ))
    ((member (car elt) '(&& &rdr))
     (let ((val (general_read$$read elt restcontext)))
       (cond ((eq *general_read$$void* val) (general_read$$read rest context))
	     (t (append val
			(general_read$$read rest context))))))
    ((eq '&upto (car elt))
     (append (general_read$$read (cddr elt) (list (cadr elt)))
	     (general_read$$read rest context)))
    (t
     (general_read$$continue (general_read$$read elt restcontext) rest context)
    ))))))))

;;; PATTERN HANDLERS
;;; ops : {*standard_input*} args context -> {*standard_input*} parse_tree

(defun general_read$$named (name context)
  (let ((val (assoc name *general_read$$schema_env*)))
    (cond
     (val (general_read$$read (cadr val) context))
     (t (format t "Undefined name in general reader ~a" name)
	(obj3-break)))))

(defun general_read$$optional (s c)
  (general_read$$!in)
  (cond
   ((general_read$$string_matches *general_read$$input* (car c))
    *general_read$$void*)
   (t (general_read$$read s c)))
 )

(defun general_read$$if_present (s c)
  (general_read$$!in)
  (cond
   ((general_read$$string_matches *general_read$$input* (car s))
    (general_read$$read s c))
   (t *general_read$$void*))
  )

(defun general_read$$one_of (s c)
  (let ((inp (general_read$$!sym)))
  (let ((val (assoc inp s :test #'general_read$$string_matches)))
    (cond
     (val (cons inp (general_read$$read (cdr val) c)))
     ((and (general_read$$at_eof inp) (assoc 'eof s))
      (cons 'eof (general_read$$read (cdr (assoc 'eof s)) c)))
     ((general_read$$at_eof inp) (general_read$$eof_error))
     (t
      (if *obj$print_errors*
	  (let ((top-level (assoc 'eof s))) ;should only be top-level schema
	    (princ "Expecting one of") (terpri)
	    (if (and top-level (not *mod_eval$open_module*))
	      (module$print_top_level_choices)
	      (general_read$$print_schema (mapcar #'car s)))
	    (terpri)
	    (princ "not ") (princ inp)
	    (general_read$$show_context)
	    (when top-level
	      (setq *obj$print_errors* nil))
	    (obj3-return-to-top))
	(obj3-return-to-top) ; don't always print message
	)
      )))))

;;; first alternative is a default
(defun general_read$$one_of_default (s c)
  (general_read$$!in)
  (let ((val (assoc *general_read$$input* (cdr s)
	       :test #'general_read$$string_matches)))
    (cond
     (val
      (let ((inp *general_read$$input*))
      (general_read$$!discard)
      (cons inp (general_read$$read (cdr val) c))))
     ((and (general_read$$at_eof) (assoc 'eof s))
      (cons 'eof (general_read$$read (cdr (assoc 'eof s)) c)))
     ((general_read$$at_eof) (general_read$$eof_error))
     (t (general_read$$read (car s) c)))))

(defun general_read$$many_of (s c)
  (let ((res nil) (close (car c)))
  (loop
   (general_read$$!in)
   (when (general_read$$at_eof) (general_read$$eof_error))
   (when (general_read$$string_matches *general_read$$input* close)
     (return (if (null res) *general_read$$void* (nreverse res))))
   (setq res (cons (general_read$$one_of s c) res))
  )))

(defun general_read$$seq_of (s c)
  (cond ((equal '(&term) s) (general_read$$seq_of_term c))
  (t
    (let ((res nil) (close (car c)))
    (loop
     (general_read$$!in)
     (when (general_read$$at_eof) (general_read$$eof_error))
     (when (general_read$$string_matches *general_read$$input* close)
       (return (if (null res) *general_read$$void* res)))
     (setq res (append res (general_read$$read s c)))
    )))))

(defun general_read$$seq_of_term (c)
  (let ((cur (general_read$$!set_single_reader '("[" "]" "_"))))
  (let ((res nil) (close (car c)))
  (if (eq *general_read$$input* *general_read$$void*)
	(setq *general_read$$input* (reader$read))
    (if (equal "(" *general_read$$input*)
	(setq *general_read$$input* (reader$$read_rest_of_list))
      ))
  (when (atom *general_read$$input*)
    (setq *general_read$$input* (list *general_read$$input*)))
  (loop
    (when (general_read$$at_eof) (general_read$$eof_error))
    (when (and (null (cdr *general_read$$input*))
	       (stringp (car *general_read$$input*))
	       (general_read$$string_matches
	           (car *general_read$$input*) close))
	  (setq *general_read$$input* (car *general_read$$input*))
	  (return))
    (setq res (append res *general_read$$input*))
    (setq *general_read$$input* (reader$read))
  )
  (general_read$$!set_reader cur)
  res
  )))

(defun general_read$$term (c)
  (let ((cur (general_read$$!set_single_reader '("[" "]" "_"))))
  (let ((res nil) (close (car c)) inp inv)
  (loop
   (setq inp (reader$read))
   (when (general_read$$at_eof inp) (general_read$$eof_error))
   (cond ((= 1 (length inp)) (setq inv (car inp)))
	 (t (setq inv *general_read$$void*)))
   (when (general_read$$string_matches inv close)
     (setq *general_read$$input* inv)
     (return))
   (setq res (append res inp))
  )
  (general_read$$!set_reader cur)
  res
  )))

(defun general_read$$sort (c) ;@@
  (declare (ignore c))
  (let ((inp (general_read$$!sym)))
    (cond
     ((and (stringp inp)
	   (eql #\. (char inp (1- (length inp)))))
      (loop (unless (eq 'space reader$$ch) (return)) ;a bit ugly
	    (setq reader$$ch (reader$$get_char *standard-input*)))
      (if (equal '|(| reader$$ch)
	  (list* inp (reader$read))
	inp))
     (t inp)
  )))

(defun general_read$$sorts (c)
  (let ((res nil))
  (loop
    (general_read$$!in)
    (when (general_read$$at_eof) (general_read$$eof_error))
    (when (general_read$$string_matches *general_read$$input* (car c))
	  (return (nreverse res)))
    (push (general_read$$sort c) res)
  )))

(defun general_read$$any_one (s)
  (general_read$$!in)
  (cond
   ((member *general_read$$input* s :test #'string-match)
    (general_read$$!sym))
   (t (princ "Expectinag one of") (terpri)
      (general_read$$print_schema s) (terpri)
      (princ "not ") (princ *general_read$$input*)
      (general_read$$show_context)
      (obj3-return-to-top))))

(defun general_read$$module_exp (c)
  (let ((cur (general_read$$!set_single_reader '(#\[ #\]))))
    (prog1 (general_read$$modexp c)
      (general_read$$!set_reader cur))))

;;; read term with balanced: view/endv (/) [/] terminated by c context
(defun general_read$$modexp (c)
  (let ((cur (general_read$$!set_single_reader '("_"))))
  (let ((res nil))
    (loop
     (general_read$$!in)
     (when (general_read$$at_eof) (general_read$$eof_error))
     (when (general_read$$string_matches *general_read$$input* c)
       (return res))
     (setq res (nconc res (general_read$$modexp_delimited)))
     )
  (general_read$$!set_reader cur)
  res
  )))

(defun general_read$$modexp_delimited ()
  (general_read$$!in)
  (let ((pr (assoc *general_read$$input*
		   '(("view" "endv" "weiv") ("[" "]") ("(" ")"))
		   :test #'general_read$$string_matches)))
    (cond
     ((null pr)
      (prog1 (cons *general_read$$input* nil)
	     (general_read$$!discard)))
     (t 
      (let ((sym *general_read$$input*))
	(general_read$$!discard)
	(let ((lst (general_read$$modexp (cdr pr))))
	  (prog1 (cons sym (append lst (cons *general_read$$input* nil)))
	    (general_read$$!discard)))))
       )))

;;; continues the matching process
; op general_read$$continue : {*standard_input*} value schema context ->
;   {*standard_input*} parse_tree
;;; the value is returned as the first component of the resulting tree
(defun general_read$$continue (v s c)
  (cond
    ((eq *general_read$$void* v) (general_read$$read s c))
    (t (cons v (general_read$$read s c)))))

;;; continues the matching process; appending the value given
; op general_read$$continue_append : {*standard_input*} value schema context ->
;   {*standard_input*} parse_tree
;;; the value is returned as the first component of the resulting tree
(defun general_read$$continue_append (v s c)
  (cond
    ((eq *general_read$$void* v) (general_read$$read s c))
    (t (append v (general_read$$read s c)))))

;;; schema for top-level OBJ3
;&&&& whole as defvar
(defvar *module_parse$module_schemas*)
(setq *module_parse$module_schemas* '(
(Top_form
  (&one_of
   ((&+ object obj) &symbol (&optional (&& Params)) is
     (&many_of
      ((&+ dfn define) &symbol is &modexp |.|)
      ((&+ ex extending) &modexp |.|)
      ((&+ pr protecting) &modexp |.|)
      ((&+ us using) &modexp |.|)
      ((&+ inc including) &modexp |.|) ;@
      ((&+ sort sorts) &sorts |.|)
      (bsort &symbol (&call (read)) |.|)
      ((&+ psort principal-sort) &sort |.|)
      ((&+ subsort subsorts) (&upto (< |.|) &sorts)
           &append (&seq_of < (&upto (< |.|) &sorts)) |.|)
      ((&+ op ops) (&& Op_pattern) |:| &sorts -> &sort
           (&optional (&& Attr)) |.|)
      (let &symbol (&optional |:| &sort) = &term |.|)
      ((&+ var vars) &symbols |:| &sort |.|)
      (vars-of (&optional &modexp) |.|) ;@
      (as &sort |:| &term if &term |.|)
      (op-as (&& Op_pattern) |:| &sorts -> &sort
	     for &term if (&upto ("[" ".") &term)
	     (&optional (&& Attr)) |.|)
      (eq &term = &term |.|)
      ((&+ ceq cq) &term = &term if &term |.|)
      ((&+ beq bq) &term = (&call (read)) |.|)
      ((&+ cbeq cbq) &term = (&call (read))
           if &term |.|)
      ((&+ ---> ***>) &comment)
      ((&+ --- ***) &commentlong)
      (parse &term |.|)
      (ev (&call (read)))
      ([ (&seq_of &term) ])
     )
     (&+ endo jbo)
    )
   ((&+ theory th) &symbol (&optional (&& Params)) is
     (&many_of
      ((&+ dfn define) &symbol is &modexp |.|)
      ((&+ ex extending) &modexp |.|)
      ((&+ pr protecting) &modexp |.|)
      ((&+ us using) &modexp |.|)
      ((&+ inc including) &modexp |.|)
      ((&+ sort sorts) &sorts |.|)
      ((&+ psort principal-sort) &sort |.|)
      ((&+ subsort subsorts) (&upto (< |.|) &sorts)
		&append (&seq_of < (&upto (< |.|) &sorts)) |.|)
      ((&+ op ops) (&& Op_pattern) |:| &sorts -> &sort
	  (&optional (&& Attr)) |.|)
      (let &symbol (&optional |:| &sort) = &term |.|)
      ((&+ var vars) &symbols |:| &sort |.|)
      (vars-of (&optional &modexp) |.|) ;@
      (as &sort |:| &term if &term |.|)
      (op-as (&& Op_pattern) |:| &sorts -> &sort
	     for &term if (&upto ("[" ".") &term)
	     (&optional (&& Attr)) |.|)
      (eq &term = &term |.|)
      ((&+ ceq cq) &term = &term if &term |.|)
      ((&+ ---> ***>) &comment)
      ((&+ --- ***) &commentlong)
      (parse &term |.|)
      (ev (&call (read)))
      ([ (&seq_of &term) ])
     )
     (&+ endth ht)
    )
   (view &symbol
	 &modexp
;	 (&one_of
;	  (from &modexp to &modexp)
;	  (of &modexp as &modexp))
;	 is
;     (&many_of
;      (sort (&upto (to) (&seq_of &term)) to (&upto (|.|) (&seq_of &term)) |.|)
;      (var &symbols |:| &symbol |.|)
;      (op (&optional &symbol) |:| (&upto (to) (&seq_of &term))
;	  to (&optional &symbol) |:| (&upto (|.|) (&seq_of &term))
;	  |.|)
;      ((&+ --- *** ---> ***>) &comment)
;     )
     (&+ endv weiv endview)
    )
   ((&+ reduce red) (&if_present  in &modexp |:|) (&seq_of &term) |.|)
   (make &symbol (&optional (&& Params)) is &modexp endm)
   (test reduction (&if_present in &modexp |:|)
     (&seq_of &term)
     |expect:| (&seq_of &term) |.|)
   (call-that &symbol |.|)
   ((&+ red-loop rl) &symbol) ;@ change?
   ((&+ input in) (&call (read-line)))
   ((&+ ---> ***>) &comment)
   ((&+ --- ***) &commentlong)
   (parse &term |.|)
   ((&+ ev eval evq eval-quiet) (&call (read))) ; once was lisp
; lhh -- add the compile command to the list of available top level stuff
   ((&+ show sh set do select compile) (&seq_of &term) |.|)
   (open &modexp |.|) ;@
   (openr &modexp |.|) ;@
   (close)
   (eof)
   ((&+ quit q))
;@@@@@@@@@@@@ top level actions
      ((&+ dfn define) &symbol is &modexp |.|)
      ((&+ ex extending) &modexp |.|)
      ((&+ pr protecting) &modexp |.|)
      ((&+ us using) &modexp |.|)
      ((&+ inc including) &modexp |.|) ;@
      ((&+ sort sorts) &sorts |.|)
      (bsort &symbol (&call (read)) |.|)
      ((&+ psort principal-sort) &sort |.|)
      ((&+ subsort subsorts) (&upto (< |.|) &sorts)
           &append (&seq_of < (&upto (< |.|) &sorts)) |.|)
      ((&+ op ops) (&& Op_pattern) |:| &sorts -> &sort
           (&optional (&& Attr)) |.|)
      (let &symbol (&optional |:| &sort) = &term |.|)
      ((&+ var vars) &symbols |:| &sort |.|)
      (vars-of (&optional &modexp) |.|) ;@
      (as &sort |:| &term if &term |.|)
      (op-as (&& Op_pattern) |:| &sorts -> &sort
	     for &term if (&upto ("[" ".") &term)
	     (&optional (&& Attr)) |.|)
      (eq &term = &term |.|)
      ((&+ ceq cq) &term = &term if &term |.|)
      ((&+ beq bq) &term = (&call (read)) |.|)
      ((&+ cbeq cbq) &term = (&call (read))
           if &term |.|)
      ;already global:
      ;((&+ ---> ***>) &comment)
      ;((&+ --- ***) &commentlong)
      ;((&+ reduce red) (&if_present  in &modexp |:|) (&seq_of &term) |.|)
      ;(parse &term |.|)
      ;(ev (&call (read)))
   (start &term |.|)
; lhh - insert the run command
   (run (&seq_of &term) |.|)
   (apply (&one_of_default
     (&symbol (&upto (within at)
     (&optional with &symbol = (&upto (|,| within at) &term) &append
		(&seq_of |,| &symbol = (&upto (|,| within at) &term))))
     (&+ within at)
     (&upto (|.|)
     ((&& Selector))
     (&seq_of of ((&& Selector)))))
     (-retr with sort &sort
       (&+ within at) (&upto (|.|) ((&& Selector))
			     (&seq_of of ((&& Selector)))))
     (?))
     |.|)
   (cd &symbol)
   (ls)
   (pwd)
   ([ (&seq_of &term) ])
   ;&term --> (&seq_of of &specifier) -- omit first "of"

   ; specifier -- [#..#] (# ...) {#,...} ---opname?
   (?)
  ))
(Selector
 (&one_of
  (term) (top)
  ({ &int &append (&seq_of |,| &int) })
  (|(| (&seq_of &int) |)|)
  ([ &int (&optional |..| &int) ])))
(Params ([ (&& Param) &append (&seq_of |,| (&& Param)) ]))
(Param 
 (&one_of_default
  (&symbols |::| (&upto (|,| ]) &modexp))
  ((&+ ex extending us using pr protecting inc including) ;@
   &symbols |::| (&upto (|,| ]) &modexp))))
(Op_pattern ((&seq_of &term)))
(Attr
 ([ (&many_of
     ((&+ assoc comm idem associative commutative idempotent))
     (selectors |(| &symbols |)|) ;not implemented
     (|id:| &obj_item)
     (|identity:| &obj_item)
     (|idr:| &obj_item)
     (|identity-rules:| &obj_item)
     ((&pred general_read$$numberp))
     ((&+ prec precedence) &int) ;7/9/86
     (|(| (&seq_of &int) |)|)
     ((&+ strat strategy) |(| (&seq_of &int) |)|)
     ((&+ gather gathering) |(| &symbols |)|)
     ((&+ poly polymorphic) (&call (read)))
     (memo)
     (intrinsic))
  ]))
))

(defvar *obj$input_source*)

(defun general_read$$show_context ()
  (terpri)
  (when (and obj$verbose *general_read$$current_context*
	     (not (eq *general_read$$void* *general_read$$current_context*)))
	(princ "Following context: ")
	(print$simple_princ_open *general_read$$current_context*) (terpri))
  (when *obj$input_source* ; nil means may be from terminal
  (princ "filename: ") (princ *obj$input_source*)
  (when (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))
    (princ " at character position: ")
    (prin1 (file-position *standard-input*)))
  (terpri)
  (when (and *general_read$$current_schema*
	     (general_read$$is_simple_schema *general_read$$current_schema*))
    (princ "processing schema: ")
    (general_read$$display_schema *general_read$$current_schema*)
    (terpri))
  (when (and *general_read$$starting-position*
	     (not (equal *general_read$$starting-position*
			 (file-position *standard-input*))))
    (princ "starting character position was: ")
    (prin1 *general_read$$starting-position*)
    (terpri))
  (princ "Context:")
  (unless (eq *general_read$$void* *general_read$$input*)
    (princ *general_read$$input*))
  (if (reader$is_at_eof)
    (princ " ... at end of file")
    (dotimes (i 20)
      (print$check)
      (princ " ")
      (let ((val (reader$read_sym)))
	(when (reader$is_at_eof)
	  (princ " [end of file]")
	  (return))
	(princ val)
	(when (equal "eof" val) (return)))))
  (terpri)))

(defun general_read$$is_simple_schema (sch)
  (or (atom sch)
      (and (consp sch)
	   (every #'atom sch)))
  )

; modify print to certain depth and length transliterating notations
(defun general_read$$display_schema (sch)
  (dolist (i sch)
    (print$check)
    (princ " ")
    (princ i))
  )

(defun general_read$$!init ()
  ; initialize the single token buffer
  (setq *general_read$$input* *general_read$$void*))

(defun general_read$$print_schema (s)
  (if (atom s) (princ s)
  (let ((flag nil))
  (dolist (i s)
    (if (< *print$line_limit* (filecol *standard-output*))
	(progn
	  (terpri)
	  (when *print$indent_contin*
	    (princ "    ")
	    (setq flag t)))
      (if flag (princ " ") (setq flag t)))
    (if (atom i) (princ i)
    (if (eq '&+ (car i))
      (dolist (e (cdr i))
	(if (< *print$line_limit* (filecol *standard-output*))
	    (progn
	      (terpri)
	      (when *print$indent_contin*
		(princ "    ")
		(setq flag t)))
	  (if flag (princ " ") (setq flag t)))
	(princ e))
      (princ i)
      ))
  ))))

; an ignored comment (value is "")
; has provision for long case: *** ( )
(defun general_read$$commentlong ()
  (let (ch)
  (unless (eql '|(| reader$$ch)
  (loop
   (locally
       #+MIPS (declare (optimize (safety 2)))
       (setq ch (read-char *standard-input* nil *reader$eof_value*)))
   (unless (or (eql #\Space ch)
	       (eql #\Tab ch))
     (return)))
  (setq reader$$ch
	(if (eq ch *reader$eof_value*) *reader$eof_value*
	  (let ((val (reader$get_syntax ch)))
	    (if val val ch)))))
  (if (eq '|(| reader$$ch)
      (reader$read)
    (unless (or (eql #\Newline ch) (eql #\Return ch)) (read-line))
  ))
  (setq reader$$ch 'space)
  ""
  )

; lhh -- list the compile & run commands as available commands
(defun module$print_top_level_choices ()
  (let ((flag nil))
  (dolist (i '(
      "object" "obj"  "theory" "th" "view"  "reduce" "red"
      "make" "test" "red-loop" "rl" "input" "in" "--->"
      "***>" "---" "***" "parse" "ev" "eval" "evq" "eval-quiet"
      "show" "sh" "set" "do" "select" "compile" "run" 
      "open" "openr" "close" "eof"
      "quit" "q" "start" "apply" "cd" "ls" "pwd" "["))
    (if (< *print$line_limit* (filecol *standard-output*))
	(progn
	  (terpri)
	  (when *print$indent_contin*
	    (princ "    ")
	    (setq flag t)))
      (if flag (princ " ") (setq flag t)))
    (princ i))))
