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

;; $Id: substitution.lsp,v 206.1 2003/09/29 12:46:22 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  All the operations related to substitutions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; June 3 86 ;;;;

;;;;;;;;;;;;;;;; Representation

;; A substitution is supposed to be an association list.

;; op substitution$new: -> Substitution.
(defmacro substitution$new ()
  '()
  )

;; op substitution$create: Variable Term -> Substitution
; not used
(defmacro substitution$create (var term)
  `(list (cons ,var ,term))
  ;18 Feb 88 added list
  )

;; op substitution$add: Substitution var term -> Substitution
(defmacro substitution$add (subst var term)
;  `(acons ,var ,term ,subst)
  `(cons (cons ,var ,term) ,subst)
  )

;; Returns teta(t) and "true" iff the sort of "t" and "teta(t)" are the same.
;; A COPY of the term "t" is done and the sort information is updated.

;; op substitution$image: Substitution Term -> Term Bool .
(defun substitution$image (teta term)
  (macrolet ((assoc% (x y)
	       `(let ((lst ,y))
		  (loop
		   (when (null lst) (return nil))
		   (when (eq ,x (caar lst)) (return (car lst)))
		   (setq lst (cdr lst))))))
  (cond ((term$is_var term)
	 (let ((im (cdr (assoc% term teta))))
	 (if im  ;; i.e. im = teta(term)
	     (values im nil)
	   (values term t)
	   ))) ; clause
	((term$is_constant term)
	 (if (eq obj_BUILT-IN$sort_Built-in (term$sort term))
	     (multiple-value-bind
	      (new_term success) (funcall (term$built_in_value term) teta)
	      (if success new_term
		(throw 'rule_failure nil))
	        ; should only be used in context of rule$!apply_one_rule
	      )
	     (values
	      (cons (let ((a (car term)))
		      (cons (car a) (cdr a)))
		    (cdr term))
	      t)))
	(t 
	 (let ((l_result nil)
	       (modif_sort nil))
	   (dolist (s_t (term$subterms term))
		   (multiple-value-bind (image_s_t same_sort)
					(substitution$image teta s_t)
			(unless same_sort (setq modif_sort t))
			(push image_s_t l_result)
			) ;; bind
		   ) ;; do
	   (setq l_result (nreverse l_result))
	   (if modif_sort
	       (let ((term_image (term$make_term_with_sort_check 
					 (term$head term)
					 l_result)))
	       (values term_image
		       (eq (term$sort term)
			   (term$sort term_image))))
	     (values (term$make_term (term$head term)
				     l_result)
		     t)
	     ) ;; if
	   ));; let
	);; cond
  ) ; macrolet
  )

(defun substitution$make_built_in_subterm (lsp)
  (term$make_built_in_constant obj_BUILT-IN$sort_Built-in lsp))

(defun substitution$check_built_in (trm)
  (if (or #-CMU (typep trm 'compiled-function)
	  #+(or CMU CLISP) (typep trm 'function)
	  (and (consp trm) (eq 'lambda (car trm))))
      (term$make_built_in_constant obj_BUILT-IN$sort_Built-in trm)
    trm))

;; NO COPY of Term is done.

;; op substitution$!image_wo_copy: Substitution Term -> !Term
(defun substitution$!image_wo_copy (teta term)
  (let ((im nil))
    (cond ((term$is_var term)
	   (when (setq im (cdr (assoc term teta))) ;; i.e. im = teta(term)
	       (variable$!replace_var_by_term term im)
	       )) ; cond clause
	  ((term$is_constant term) ) ;; do nothing
	  (t
	   (dolist (s_t (term$subterms term))
	     (substitution$!image_wo_copy teta s_t)
	     ) ;; do
	   )
	  ) ;; cond
    )
  )

;; op substitution$lookup : Substitution Term -> Term or Nil
(defun substitution$lookup (teta term)
  (let ((val (assoc term teta)))
    (if val (cdr val) nil)))

;; op substitution$partial_image: Substitution Term -> Term Bool .
; from substitution$image

(defun substitution$partial_image (teta term)
  (macrolet ((assoc% (x y)
	       `(let ((lst ,y))
		  (loop
		   (when (null lst) (return nil))
		   (when (eq ,x (caar lst)) (return (car lst)))
		   (setq lst (cdr lst))))))
  (cond ((or #-CMU (typep term 'compiled-function) ;same test above
	     #+(or CMU CLISP) (typep term 'function)
	     (and (consp term) (eq 'lambda (car term))))
	 (substitution$make_built_in_subterm
	  (substitution$compose teta term)))
        ((term$is_var term)
	 (let ((im (cdr (assoc% term teta))))
	 (if im  ;; i.e. im = teta(term)
	     (values im nil)
	   (values term t)))) ; clause
	((term$is_constant term)
	 (if (eq obj_BUILT-IN$sort_Built-in (term$sort term))
	     (substitution$make_built_in_subterm
	      (substitution$compose teta (term$built_in_value term)))
	     (values
	      (cons (let ((a (car term)))
		      (cons (car a) (cdr a)))
		    (cdr term))
	      t)))
	(t
	 (let ((l_result nil) (modif_sort nil))
	   (dolist (s_t (term$subterms term))
		   (multiple-value-bind
		    (image_s_t same_sort)
		       (substitution$partial_image teta s_t) ;1/6/92
		    (unless same_sort (setq modif_sort t))
		    (push image_s_t l_result)
		    )) ;; do bind
	   (setq l_result (nreverse l_result))
	   (if modif_sort
	       (let ((term_image
		      (term$make_term_with_sort_check 
		       (term$head term)
		       l_result)))
	       (values term_image
		       (eq (term$sort term) ;22 Dec 87 was equal
			   (term$sort term_image))))
	     (values (term$make_term (term$head term) l_result)
		     t)
	     ) ;; if
	   ));; let
	);; cond
  ) ; macrolet
  )

(defun substitution$compose (teta fcn)
  (if (or #-CMU (typep fcn 'compiled-function)
	  #+(or CMU CLISP) (typep fcn 'function)
	  (not (and (consp fcn) (eq 'lambda (car fcn))
		    (equal '(compn) (cadr fcn)))))
      (make_function `(lambda (compn)
	 (funcall ',fcn (append ',teta compn))))
    (let ((oldteta (cadr (nth 1 (nth 2 (nth 2 fcn)))))
	  (realfcn (cadr (nth 1 (nth 2 fcn)))))
      (make_function `(lambda (compn)
	 (funcall ',realfcn (append ',(append teta oldteta) compn)))))
  ))

;; op substitution$image_simplifying : Substitution Term -> Term Bool .
(defun substitution$image_simplifying (teta term)
  (macrolet ((assoc% (x y)
	       `(let ((lst ,y))
		  (loop
		   (when (null lst) (return nil))
		   (when (eq ,x (caar lst)) (return (car lst)))
		   (setq lst (cdr lst))))))
  (cond ((term$is_var term)
	 (let ((im (cdr (assoc% term teta))))
	 (if im  ;; i.e. im = teta(term)
	     (values im nil)
	   (values term t)
	   ))) ; clause
	((term$is_constant term)
	 (if (eq obj_BUILT-IN$sort_Built-in (term$sort term))
	     (multiple-value-bind
	      (new_term success) (funcall (term$built_in_value term) teta)
	      (if success new_term
		(throw 'rule_failure nil))
	        ; should only be used in context of rule$!apply_one_rule
	      )
	     (values
	      (cons (let ((a (car term)))
		      (cons (car a) (cdr a)))
		    (cdr term))
	      t)))
	(t 
	 (let ((l_result nil)
	       (modif_sort nil))
	   (dolist (s_t (term$subterms term))
		   (multiple-value-bind (image_s_t same_sort)
					(substitution$image_simplifying
					 teta s_t)
			(unless same_sort (setq modif_sort t))
			(push image_s_t l_result)
			) ;; bind
		   ) ;; do
	   (setq l_result (nreverse l_result))
	   (let ((op (term$head term)))
	   (if (and (cdr l_result) (null (cddr l_result))
		    (operator$is_identity op))
	     (if (term$is_zero_for_op (car l_result) op)
	       (values (cadr l_result)
		       (eq (term$sort term) (term$sort (cadr l_result))))
	     (if (term$is_zero_for_op (cadr l_result) op)
	       (values (car l_result)
		       (eq (term$sort term) (term$sort (car l_result))))
	     ; This is a duplication of the following code
	     (if modif_sort
	       (let ((term_image (term$make_term_with_sort_check 
				  op l_result)))
		 (values term_image
			 (eq (term$sort term) (term$sort term_image))))
	       (values (term$make_term op l_result) t)
	       ))) ; done with zero cases
	     ; This is the same as the previous bit of code
	     (if modif_sort
	       (let ((term_image (term$make_term_with_sort_check 
				  op l_result)))
		 (values term_image
			 (eq (term$sort term) (term$sort term_image))))
	       (values (term$make_term op l_result) t)
	       )) ;; if
	     )));; let
	 );; cond
	) ; macrolet
  )

; canonicalize substitution
(defun substitution$can (s)
  (sort (copy-list s)
      #'(lambda (x y) ;two substitution items (var . term)
	  (string< (variable$name (car x)) (variable$name (car y)))
	  ))
  )

; assumed canonicalized
(defun substitution$equal (s1 s2)
  (every2len #'(lambda (x y)
		 (and
		  (variable$is_equal (car x) (car y))
		  (term$similar (cdr x) (cdr y))))
    s1 s2))

(defun substitution$restrict (vars sub)
  (let ((res nil))
    (dolist (s sub)
      (when (member (car s) vars)
	(push s res))
    )
    res
  ))

; subset when viewed as a set of (mapping) pairs
; assumed canonicalized
(defun substitution$subset (s1 s2)
  (let ((s1x s1) (s2x s2) (res t))
  (loop
   (when (null s1x) (return))
   (let ((v1 (caar s1x)) (t1 (cdar s1x)))
   (loop
    (when (null s2x) (setq res nil) (return))
    (when (variable$is_equal v1 (caar s2x))
      (if (term$similar t1 (cdar s2x))
	(progn (setq s2x (cdr s2x)) (return))
	(progn (setq res nil) (return))))
    (setq s2x (cdr s2x))
   )
   (when (null res) (return))
   (setq s1x (cdr s1x))
  ))
  res
  ))

;; op substitution$simple_image: Substitution Term -> Term .
(defun substitution$simple_image (teta term)
  (macrolet ((assoc% (x y)
	       `(let ((lst ,y))
		  (loop
		   (when (null lst) (return nil))
		   (when (eq ,x (caar lst)) (return (car lst)))
		   (setq lst (cdr lst))))))
  (cond ((term$is_var term)
	 (let ((im (cdr (assoc% term teta))))
	   (if im im term)))
	((term$is_constant term) ;@@
	 (cons (let ((a (car term)))
		 (cons (car a) (cdr a)))
	       (cdr term)))
	(t 
	 (term$make_term (term$head term)
	     (mapcar #'(lambda (stm) (substitution$simple_image teta stm))
	       (term$subterms term)))))))
