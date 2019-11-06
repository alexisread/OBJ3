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

;; $Id: modexp_parse.lsp,v 205.2.1.1 2003/09/23 14:06:14 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    module expression parser
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: May 12, 1986

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;

; op modexp_parse$parse : list[Token] -> ModuleExpression

; op token$is_name : Token -> bool (location?)

;;;; HIERARCHY OF OPERATORS
;;;; precedence: _[] over _*_ over _+_
;;;; all are left-associative

;;; checks whether a token is a valid name
(defun token$is_name (token)
  (dotimes (i (length token) t)
    (let ((ch (aref token i)))
    (when
     ; allow more things in names
;     (not (or (alphanumericp ch)
;	      (equal ch #\-)))
      (or (eql ch #\[) (eql ch #\]))
	  (return nil)))))

;;;; MODULE EXPRESSION PARSER
;;;; argument is a list of strings
;;;; result is a module expression

; op modexp_parse$parse : list[Token] ->
;                         ModuleExpression [global: *modexp-parse$$inp*]
(defun modexp_parse$parse (inp)
  (if (null inp)
      (progn
	(princ "Error: module expression is empty") (terpri)
      	(obj3-to-top))
  (let ((*modexp_parse$$inp* inp))
  (let ((res (modexp_parse$$parser)))
    res)))
  )

; op modexp_parse$parse_view : list[Token] ->
;                              ModuleExpression [global: *modexp-parse$$inp*]
; used in module_eval for parsing top-level views
(defun modexp_parse$parse_view (inp)
  (let ((*modexp_parse$$inp* inp))
    (modexp_parse$$view)))

; var *modexp_parse$$inp* -- global var for module expression input
(defvar *modexp_parse$$inp* 'undef)

;;;; skip input
; op modexp_parse$$skip : {*modexp_parse$$inp*} -> {*modexp_parse$$inp*}
(defun modexp_parse$$skip ()
  (setq *modexp_parse$$inp* (cdr *modexp_parse$$inp*)))

;;;; Module Expression parser: input is global in *modexp_parse$$inp*

; op modexp_parse$$parser : {*modexp_parse$$inp*} ->
;                           {*modexp_parse$$inp*} ModuleExpression
(defun modexp_parse$$parser ()
  (let ((e1 (modexp_parse$$rename_or_inst)))
    (cond
     ((null *modexp_parse$$inp*) e1)
     ((equal "+" (car *modexp_parse$$inp*))
      (loop
        (modexp_parse$$skip)
        (let ((e2 (modexp_parse$$rename_or_inst)))
	  (setq e1 (modexp$make_plus e1 e2)))
	(when (or (null *modexp_parse$$inp*)
		  (not (equal "+" (car *modexp_parse$$inp*))))
	  (return e1))))
     ((equal "is" (car *modexp_parse$$inp*)) ;&&&& should add layer?
      (modexp_parse$$skip)
      (let ((e2 (modexp_parse$$parser)))
	(list 'is e1 e2)
	))
     ((equal "with" (car *modexp_parse$$inp*)) ;same
      (let ((lst nil))
	(loop
	 (modexp_parse$$skip)
	 (setq lst (cons (modexp_parse$$parser) lst))
	 (unless (equal "and" (car *modexp_parse$$inp*)) (return))
	)
	(list 'with e1 (nreverse lst))
      ))
     ((member (car *modexp_parse$$inp*)
	      '("]" "," ")" "is" "to" "::" "and" ".")
	      :test #'equal)
      e1)
     (t (format t "Error: module expression parsing: ~a followed by ~a~%"
		e1 (car *modexp_parse$$inp*))
	(obj3-to-top)))))

; op modexp_parse$$general : {*modexp_parse$$inp*} ->
;                            {*modexp_parse$$inp*} ModuleExpression
; used only in context: view from X to <<Y>> is
; doesn't allow "is" in this context as modexp_parse$$parser would
(defun modexp_parse$$general ()
  (let ((e1 (modexp_parse$$rename_or_inst)))
    (cond
     ((null *modexp_parse$$inp*) e1)
     ((equal "+" (car *modexp_parse$$inp*))
      (loop
        (modexp_parse$$skip)
        (let ((e2 (modexp_parse$$rename_or_inst)))
	  (setq e1 (modexp$make_plus e1 e2)))
	(when (or (null *modexp_parse$$inp*)
		  (not (equal "+" (car *modexp_parse$$inp*))))
	  (return e1))))
     ((equal "with" (car *modexp_parse$$inp*))
      (let ((lst nil))
	(loop
	 (modexp_parse$$skip)
	 (setq lst (cons (modexp_parse$$general) lst))
	 (unless (equal "and" (car *modexp_parse$$inp*)) (return))
	)
	(list 'with e1 (nreverse lst))
      ))
     ((member (car *modexp_parse$$inp*)
	      '("]" "," ")" "is" "to" "::" "and" ".")
	      :test #'equal)
      e1)
     (t (format t "Error: module expression parsing: ~a followed by ~a~%"
		e1 (car *modexp_parse$$inp*))
	(obj3-to-top)))))

;;;; SYNTACTIC CLASS ROUTINES

(defun modexp_parse$$rename_or_inst ()
  (let ((e1 (modexp_parse$$instantiation)))
    (cond
     ((null *modexp_parse$$inp*) e1)
     ((equal "*" (car *modexp_parse$$inp*))
      (loop
       (modexp_parse$$skip)
       (let ((e2 (modexp_parse$$rename)))
	 (setq e1 (modexp$make_rename e1 e2)))
       (when (or (null *modexp_parse$$inp*)
		 (not (equal "*" (car *modexp_parse$$inp*))))
	 (return e1))))
     ((member (car *modexp_parse$$inp*)
	      '("]" "," ")" "is" "to" "::" "+" "with" "and" ".")
	      :test #'equal)
      e1)
     (t (format t "Error: module expression parsing: ~a followed by ~a~%"
		e1 (car *modexp_parse$$inp*))
	(obj3-to-top)))))

(defun modexp_parse$$rename ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: premature end of module expression in a rename~%")
    (obj3-to-top))
   ((not (equal "(" (car *modexp_parse$$inp*)))
    (format t "Error: body of renaming should be preceded by \"(\"~%")
    (obj3-to-top))
   (t
    (modexp_parse$$skip)
    (let ((res nil))
     (loop
      (when (null *modexp_parse$$inp*)
	(format t "Error: ill-formed rename body~%")
	(obj3-to-top))
      (setq res (cons (modexp_parse$$rename_elt) res))
      (when (equal ")" (car *modexp_parse$$inp*))
	(modexp_parse$$skip)
	(return (nreverse res)))
      (when (not (equal "," (car *modexp_parse$$inp*)))
	(format t "Error: renaming elements should be separated by \",\"~%")
	(obj3-to-top))
      (modexp_parse$$skip)
      )))))

(defun modexp_parse$$rename_elt ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: premature end of renaming~%")
    (obj3-to-top))
   ((equal "sort" (car *modexp_parse$$inp*))
    (let (a b)
      (modexp_parse$$skip)
      (setq a (modexp_parse$$sort_specn '("to")))
      (when (not (equal "to" (car *modexp_parse$$inp*)))
	(format t "Error: in renaming, for sort ~a didn't find \"to\"~%" a)
	(obj3-to-top))
      (modexp_parse$$skip)
      (setq b (modexp_parse$$sort_specn '("," ")")))
      `(sort ,a ,b)))
   ((equal "op" (car *modexp_parse$$inp*))
    (let (a b)
      (modexp_parse$$skip)
      (setq a (modexp_parse$$op_name_specn '("to")))
      (when (not (equal "to" (car *modexp_parse$$inp*)))
	    (format t "Error: in renaming, for sort ~a didn't find \"to\"~%" a)
	    (obj3-to-top))
      (modexp_parse$$skip)
      (setq b (modexp_parse$$op_name_specn '("," ")")))
      `(op ,a ,b)))
   (t (format t "Error: found ~a when expected \"sort\" or \"op\"~%"
	      (car *modexp_parse$$inp*))
      (obj3-to-top))))

; always called with current input = "("
(defun modexp_parse$$balanced_parens ()
  (modexp_parse$$skip)
  (let ((res (list "(")) (d 1))
    (loop
     (cond
      ((null *modexp_parse$$inp*)
       (princ "Error: premature end of input after:") (terpri)
       (print$simple_princ_open (nreverse res))
       (terpri)
       (obj3-to-top))
      (t (let ((cur (car *modexp_parse$$inp*)))
	(setq res (cons cur res))
	(modexp_parse$$skip)
	(cond
	 ((equal ")" cur)
	  (decf d)
	  (when (= 0 d) (return (nreverse res))))
	 ((equal "(" cur) (incf d)))))
    ))))

(defun modexp_parse$$instantiation ()
  (let ((m (modexp_parse$$basic)))
    (cond
     ((null *modexp_parse$$inp*) m)
     ((equal "[" (car *modexp_parse$$inp*))
      (modexp_parse$$skip)
      (let ((args (modexp_parse$$args)))
	(when (not (equal "]" (car *modexp_parse$$inp*)))
         (format t "Error: \"[\" without \"]\" in module expression~%")
	 (obj3-to-top))
	(modexp_parse$$skip)
	(modexp$make_instantiation m args)
      ))
     (t m))
  ))

(defun modexp_parse$$args ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input in argument list~%")
    (obj3-to-top))
   ((equal "]" (car *modexp_parse$$inp*)) nil)
   (t (let ((res nil))
    (loop
     (setq res (cons (modexp_parse$$arg) res))
     (when (equal "]" (car *modexp_parse$$inp*))
       (return (nreverse res)))
     (when (not (equal "," (car *modexp_parse$$inp*)))
       (format t "Error: in instantation, need \",\" between arguments~%")
       (obj3-to-top))
     (modexp_parse$$skip)
   )))))

(defun modexp_parse$$arg ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input in argument to parameterized module~%")
    (obj3-to-top))
   ((equal "view" (car *modexp_parse$$inp*))
    (modexp_parse$$view))
   ((equal "op" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    `(op ,(modexp_parse$$op_name_specn '("," "]"))))
   ((equal "sort" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    `(sort ,(modexp_parse$$sort_specn '("," "]"))))
   ((modexp_parse$$have_specn *modexp_parse$$inp*)
    `(specn ,(modexp_parse$$op_specn '("," "]"))))
   (t (let ((m (modexp_parse$$parser)))
     (cond
      ((equal "::" (car *modexp_parse$$inp*))
       (modexp_parse$$skip)
       (let ((vw (modexp_parse$$view)))
	 (modexp$make_view_under m vw)))
      ((stringp m) (let ((pos (position #\. m)))
        (if pos
	    `(qual ,(list (subseq m 0 pos))
		   ,(modexp_parse$$check_qual (subseq m (1+ pos))))
	  m)))
      (t m)
      )))
   ))

; Heuristic function: can always use explicit views
; This should only give true when the input definitely is
; a operator or sort specification
; looks for _ . :
(defun modexp_parse$$have_specn (in)
  (and
   in
   (let ((i1 (car in)))
   (or (equal "_" i1)
       (if (equal "(" i1)
	   (let ((rst (cdr in)))
	     (and rst
		  (or (equal "_" (car rst))
		      (let ((rst2 (cdr rst)))
		      (and rst2
			   (let ((i3 (car rst2)))
			     (or (equal "_" i3)
				 (equal ":" i3)
				 (and (equal ")" i3)
				      (cdr rst2)
				      (let ((i4 (cadr rst2)))
				      (or (equal "." i4)
					  (eql #\. (char i4 0))))))))))))
	 (and (cdr in)
	      (let ((i2 (cadr in)))
	      (or (equal "." i2)
		  (equal "_" i2)))))))))

(defun modexp_parse$$view ()
  (cond
   ((equal "view" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    (let (th m mppg)
      (when (equal "from" (car *modexp_parse$$inp*)) (modexp_parse$$skip))
      (if (equal "to" (car *modexp_parse$$inp*))
	(setq th 'none)
        (setq th (modexp_parse$$parser)))
      (when (not (equal "to" (car *modexp_parse$$inp*)))
	(format t "Error: expecting \"to\" in view~%")
	(obj3-to-top))
      (modexp_parse$$skip)
      (setq m (modexp_parse$$general))
      (when (not (equal "is" (car *modexp_parse$$inp*)))
        (format t "Error: expecting \"is\" in view~%")
        (obj3-to-top))
      (modexp_parse$$skip)
      (if (equal "endv" (car *modexp_parse$$inp*))
	  (setq mppg nil)
	(progn
	  (setq mppg (modexp_parse$$view_mapping))
	  (when (not (equal "endv" (car *modexp_parse$$inp*)))
	    (format t "Error: expecting \"endv\" in view~%")
	    (obj3-to-top))
	  (modexp_parse$$skip)))
      (modexp$make_view th m mppg)
    ))
   ((token$is_name (car *modexp_parse$$inp*))
    (prog1 (car *modexp_parse$$inp*) (modexp_parse$$skip)))
   (t
    (format t "Error: in view not expecting ~a~%" (car *modexp_parse$$inp*))
    (obj3-to-top))))

(defun modexp_parse$$view_mapping ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: premature end of module expression in a view~%")
    (obj3-to-top))
   (t (let ((res nil))
     (loop
      (when (null *modexp_parse$$inp*)
	(format t "Error: ill-formed view body~%")
	(obj3-to-top))
      (setq res (cons (modexp_parse$$view_elt) res))
      (when (not (equal "." (car *modexp_parse$$inp*)))
	(format t "Error: view elements should be terminated by \".\"~%")
	(obj3-to-top))
      (modexp_parse$$skip)
      (when (equal "endv" (car *modexp_parse$$inp*))
	(return (nreverse res)))
      )))))

(defun modexp_parse$$view_elt ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: premature end of view body~%")
    (obj3-to-top))
   ((equal "sort" (car *modexp_parse$$inp*))
    (let (a b)
      (modexp_parse$$skip)
      (setq a (modexp_parse$$sort_specn '("to")))
      (when (not (equal "to" (car *modexp_parse$$inp*)))
	(format t "Error: in view body, for sort ~a didn't find \"to\"~%" a)
	(obj3-to-top))
      (modexp_parse$$skip)
      (setq b (modexp_parse$$sort_specn '(".")))
      `(sort ,a , b)))
   ((or (equal "var" (car *modexp_parse$$inp*))
	(equal "vars" (car *modexp_parse$$inp*)))
    (let (v s)
      (modexp_parse$$skip)
      (setq v nil)
      (loop
        (let ((inp (car *modexp_parse$$inp*)))
        (when (equal ":" inp) (return))
	(push inp v)
	(modexp_parse$$skip)))
      (modexp_parse$$skip)
      (setq s (modexp_parse$$sort_specn '(".")))
      `(var ,v ,s)))
   ((equal "op" (car *modexp_parse$$inp*))
    (let ((sa 'universe) a (sb 'universe) b)
      (modexp_parse$$skip)
      (when *obj$obj2_mode*
	(when (not (equal ":" (car *modexp_parse$$inp*)))
	  (setq sa (car *modexp_parse$$inp*))
	  (modexp_parse$$skip))
	(when (not (equal ":" (car *modexp_parse$$inp*)))
	  (format t "Error: expecting a \":\"~%") (obj3-to-top))
	(modexp_parse$$skip))
      (setq a (modexp_parse$$op_name '("to")))
      (when (not (equal "to" (car *modexp_parse$$inp*)))
	    (format t "Error: in view body, for op ~a didn't find \"to\"~%" a)
	    (obj3-to-top))
      (modexp_parse$$skip)
      (when *obj$obj2_mode*
	(when (not (equal ":" (car *modexp_parse$$inp*)))
	  (setq sb (car *modexp_parse$$inp*))
	  (modexp_parse$$skip))
	(when (not (equal ":" (car *modexp_parse$$inp*)))
	  (format t "Error: expecting a \":\"~%") (obj3-to-top))
	(modexp_parse$$skip))
      (setq b (modexp_parse$$op_name '(".")))
      `(op (,sa ,a) (,sb ,b))))
   (t (format t "Error: found ~a when expected \"sort\" or \"op\"~%"
	      (car *modexp_parse$$inp*))
      (obj3-to-top))))

(defun modexp_parse$$basic ()
  (cond
   ((equal "(" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    (let ((m (modexp_parse$$parser)))
      (cond
       ((equal ")" (car *modexp_parse$$inp*))
	(modexp_parse$$skip)
	m)
       (t (format t "Error: unmatched \"(\" in module expression after ~a~%"
		  m)
	  (obj3-to-top))
       )))
   ((token$is_name (car *modexp_parse$$inp*))
    (prog1 (car *modexp_parse$$inp*) (modexp_parse$$skip)))
   (t (format t "Error: Can't make sense of ~a in module expression~%"
	      (car *modexp_parse$$inp*))
      (obj3-to-top))
  ))

(defun modexp_parse$$op_spec ()
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input at operation spec. in rename~%")
    (obj3-to-top))
   ((equal "(" (car *modexp_parse$$inp*))
    (modexp_parse$$balanced_parens))
   ((token$is_name (car *modexp_parse$$inp*))
    (prog1 (car *modexp_parse$$inp*) (modexp_parse$$skip)))
   (t (format t "Error: incorrect operator specification in a rename~%")
      (obj3-to-top))
  ))

;;; num indicates right context: 1 - "to"; 2 - "." or "endv"
;;; note: we are treating expression as un-parsed token sequences
(defun modexp_parse$$op_expr (num)
  (let ((context (if (= num 1) '("to") '("." "endv"))) (res nil))
    (loop
     (when (member (car *modexp_parse$$inp*) context :test #'equalp)
       (return (nreverse res)))
     (setq res (cons (car *modexp_parse$$inp*) res))
     (modexp_parse$$skip))
  ))

; used in views
(defun modexp_parse$$op_specn (cntxt)
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input at operation specification~%")
    (obj3-to-top))
   ((equal "(" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    (let ((val (modexp_parse$$op_name '(")"))) (flag t))
      (when (equal ")" (car *modexp_parse$$inp*))
	(modexp_parse$$skip)
	(setq flag nil))
      (let ((res
	   (cond ((and (equal "." (car *modexp_parse$$inp*))
		      (not (member "." cntxt :test #'equal)))
		 (modexp_parse$$skip)
		 `(qual ,val
			,(modexp_parse$$check_qual (modexp_parse$$parser))))
		((and *modexp_parse$$inp*
		      (<= 2 (length (car *modexp_parse$$inp*)))
		      (eql #\. (char (car *modexp_parse$$inp*) 0)))
		 (let ((name (car *modexp_parse$$inp*)))
		   (modexp_parse$$skip)
		   `(qual ,val ,(modexp_parse$$check_qual (subseq name 1))))
		 )
		(t val)
		)))
       (when flag
	 (unless (equal ")" (car *modexp_parse$$inp*))
	   (princ "Error: unbalanced parentheses in operator specification")
	   (terpri)
	   (princ "context: ") (print$simple_princ_open val) (terpri)
	   (obj3-to-top))
	 (modexp_parse$$skip)
	 )
       res
       )
      )
    )
   (t (modexp_parse$$op_name cntxt))
  ))

(defun modexp_parse$$op_name (cntxt)
  (let ((res nil))
  (loop
   (when (null *modexp_parse$$inp*)
     (format t "Error: end of input in operator pattern~%")
     (princ "beginning of pattern: ")
     (print$simple_princ_open (nreverse res))
     (terpri)
     (obj3-to-top))
   (when (member (car *modexp_parse$$inp*) cntxt :test #'equal)
     (return))
   (setq res (nconc res (modexp_parse$$balanced_context cntxt)))
   )
  res
  ))

; used in renames and after op in parameter position
(defun modexp_parse$$op_name_specn (cntxt)
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input at operation specification~%")
    (obj3-to-top))
   ((equal "(" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    (let ((val (modexp_parse$$op_simple_name '(")"))) (flag t))
      (when (equal ")" (car *modexp_parse$$inp*))
	(modexp_parse$$skip)
	(setq flag nil))
      (let ((res
	   (cond ((and (equal "." (car *modexp_parse$$inp*))
		      (not (member "." cntxt :test #'equal)))
		 (modexp_parse$$skip)
		 `(qual ,val
			,(modexp_parse$$check_qual (modexp_parse$$parser))))
		((and *modexp_parse$$inp*
		      (<= 2 (length (car *modexp_parse$$inp*)))
		      (eql #\. (char (car *modexp_parse$$inp*) 0)))
		 (let ((name (car *modexp_parse$$inp*)))
		   (modexp_parse$$skip)
		   `(qual ,val ,(modexp_parse$$check_qual (subseq name 1))))
		 )
		(t val)
		)))
       (when flag
	 (unless (equal ")" (car *modexp_parse$$inp*))
	   (princ "Error: unbalanced parentheses in operator specification")
	   (terpri)
	   (princ "context: ") (print$simple_princ_open val) (terpri)
	   (obj3-to-top))
	 (modexp_parse$$skip)
	 )
       res
       )
      )
    )
   (t (let ((val (modexp_parse$$op_simple_name cntxt)))
	(if (and (consp val) (null (cdr val)) (stringp (car val)))
	    (let ((name (car val)))
	    (let ((pos (position #\. name)))
	      (if (and pos (< 0 pos) (< (1+ pos) (length name)))
		  `(qual ,(list (subseq name 0 pos))
			 ,(modexp_parse$$check_qual (subseq name (1+ pos))))
		val)))
	  val)
	))
  ))

(defun modexp_parse$$op_simple_name (cntxt)
  (if (equal "(" (car *modexp_parse$$inp*))
      (progn
	(modexp_parse$$skip)
	(prog1
	  (modexp_parse$$balanced_context '(")"))
	  (modexp_parse$$skip)))
  (let ((res nil))
  (loop
   (when (null *modexp_parse$$inp*)
     (if (null cntxt)
	 (return)
       (progn
	 (format t "Error: end of input in operator pattern~%")
	 (princ "beginning of pattern: ")
	 (print$simple_princ_open (nreverse res))
	 (terpri)
	 (princ "Expecting one of ") (princ cntxt) (terpri)
	 (obj3-to-top))))
   (when (member (car *modexp_parse$$inp*) cntxt :test #'equal)
     (return))
   (setq res (nconc res (modexp_parse$$balanced_context cntxt)))
   )
  res
  )))

(defun modexp_parse$$sort_specn (cntxt)
  (cond
   ((null *modexp_parse$$inp*)
    (format t "Error: end of input at sort specification~%")
    (obj3-to-top))
   ((equal "(" (car *modexp_parse$$inp*))
    (modexp_parse$$skip)
    (let ((val (modexp_parse$$balanced_context_one '(")"))) (flag t))
      (when (equal ")" (car *modexp_parse$$inp*))
	(modexp_parse$$skip)
	(setq flag nil))
      (let ((res
	  (cond ((and (equal "." (car *modexp_parse$$inp*))
		      (not (member "." cntxt :test #'equal)))
		 (modexp_parse$$skip)
		 `(qual ,val ,(modexp_parse$$parser)))
		((and (null (cdr val))
		      (eql #\. (char (car val) (1- (length (car val))))))
		 `(qual ,(list (subseq (car val) 0 (- (length (car val)) 1)))
		   ,(modexp_parse$$parser)))
		((and *modexp_parse$$inp*
		      (<= 2 (length (car *modexp_parse$$inp*)))
		      (eql #\. (char (car *modexp_parse$$inp*) 0)))
		 (let ((name (car *modexp_parse$$inp*)))
		   (modexp_parse$$skip)
		   `(qual ,val ,(modexp_parse$$check_qual (subseq name 1))))
		 )
		(t val)
		)))
	(when flag
	  (unless (equal ")" (car *modexp_parse$$inp*))
	    (princ "Error: unbalanced parentheses in sort specification")
	    (terpri)
	    (princ "context: ") (print$simple_princ_open val) (terpri)
	    (obj3-to-top))
	  (modexp_parse$$skip))
	res
	)
      ))
   (t (let ((val (car *modexp_parse$$inp*)))
     (modexp_parse$$skip)
     (if (eql #\. (char val (1- (length val))))
	`(qual ,(list (subseq val 0 (- (length val) 1)))
	       ,(modexp_parse$$parser))
     (let ((pos (position #\. val)))
       (if pos
	   `(qual ,(list (subseq val 0 pos))
		  ,(subseq val (1+ pos)))
	 (list val))))
     ))
  ))

(defun modexp_parse$$balanced_context (cntxt)
  (let ((res nil) (d 0))
    (loop
     (cond
      ((null *modexp_parse$$inp*)
       (if (null cntxt)
	   (return (nreverse res))
	 (progn
	   (princ "Error: premature end of input after:") (terpri)
	   (print$simple_princ_open (nreverse res))
	   (terpri)
	   (obj3-to-top))))
      (t (let ((cur (car *modexp_parse$$inp*)))
	(when (and (and (= 0 d)
			(member cur cntxt :test #'equal)))
	  (return (nreverse res)))
	(setq res (cons cur res))
	(modexp_parse$$skip)
	(cond
	 ((equal ")" cur)
	  (decf d)
	  (when (= -1 d)
	    (princ "Error: too many ')'s") (terpri)
	    (return (nreverse res))))
	 ((equal "(" cur) (incf d)))))
    ))))

(defun modexp_parse$$balanced_context_one (cntxt)
  (declare (ignore cntxt))
  (if (equal "(" (car *modexp_parse$$inp*))
      (progn
	(modexp_parse$$skip)
	(prog1
	  (modexp_parse$$balanced_context '(")"))
	  (modexp_parse$$skip)))
    (prog1 (list (car *modexp_parse$$inp*)) (modexp_parse$$skip))
  ))

(defun modexp_parse$$check_qual (x)
  (if (stringp x)
    (let ((pos (position #\. x)))
      (if (and pos (< 0 pos) (< (1+ pos) (length x)))
	  `(qual ,(list (subseq x 0 pos))
		 ,(subseq x (1+ pos)))
	x))
    x)
  )

; used for views to parse op_name_specn "later on"
(defun modexp_parse$parse_op_name_specn (tokens)
  (if (null tokens) nil
  (let ((*modexp_parse$$inp* tokens))
  (let ((val (modexp_parse$$op_name_specn nil)))
    (if (null *modexp_parse$$inp*)
	val
      nil)
  ))))

(defun modexp_parse$parse_sort_specn (tokens)
  (if (null tokens) nil
  (let ((*modexp_parse$$inp* tokens))
  (let ((val (modexp_parse$$sort_specn nil)))
    (if (null *modexp_parse$$inp*)
	val
      nil)
  ))))
