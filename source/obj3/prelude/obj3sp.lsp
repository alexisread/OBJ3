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

;; $Id: obj3sp.lsp,v 206.1 2003/09/26 13:03:07 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Standard Prelude
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/31/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

;;;; built-in sorts
;;; four functions:
;;;   (1) token predicate
;;;   (2) function that maps token to lisp value [read-from-string]
;;;   (3) function to print value [prin1]
;;;   (4) predicate true for lisp values of that sort

(defun obj$false_fn (x) (declare (ignore x)) nil)
(defun obj$true_fn (x) (declare (ignore x)) t)
(defun obj$create_error (x)
  (declare (ignore x))
  (break "error: tried to create built-in sort value when meaningless"))

;;; obj_UNIVERSAL
; var *obj$sort_Universal* -- the universal sort
(defvar *obj$sort_Universal* 'void)
(defun obj_UNIVERSAL$install ()
  (let ((UNIVERSAL (modexp_eval$eval "UNIVERSAL")))
  (setq *obj$sort_Universal*
    (mod_eval$$find_sort_in UNIVERSAL "Universal"))
  ))

;;; obj_LAST-TERM
(defvar *obj_LAST-TERM$op_term* nil)
(defvar *obj_LAST-TERM$term_eqn_rhs* nil)
(defun obj_LAST-TERM$install ()
  (let ((LAST-TERM (modexp_eval$eval "LAST-TERM")))
  (setq *obj_LAST-TERM$op_term*
	(mod_eval$$find_operator_named_in LAST-TERM '("[" "term" "]")))
  (when *obj_LAST-TERM$op_term*
  (let ((ruls (operator_info$rules_with_different_top
	      (optable$operator_info
	       (module$operator_table LAST-TERM)
	       *obj_LAST-TERM$op_term*))))
    (when ruls
      (setq *obj_LAST-TERM$term_eqn_rhs* (rule$rhs (car ruls)))))
  )))
(defun term$set (term)
  (when *obj_LAST-TERM$term_eqn_rhs*
    (term$!replace *obj_LAST-TERM$term_eqn_rhs* term)))

;;; obj_ERR
(defvar obj_ERR$sort_Err 'none)
(defun obj_ERR$is_Err_token (tok) (declare (ignore tok)) nil)
(defun obj_ERR$create_Err (tok) (declare (ignore tok)) nil)
    ; should never be used
(defun obj_ERR$print_Err (val)
  (princ "err!!") (print$simple_princ val))
(defun obj_ERR$is_Err (val) (declare (ignore val)) t)
(defun obj_ERR$install ()
  (setq obj_ERR$sort_Err
	(mod_eval$$find_sort_in (modexp_eval$eval "ERR") "Err")))

;;; obj_IDENTICAL
(defvar *obj_IDENTICAL$module* nil)
(defvar *obj_IDENTICAL$identical* 'void)
(defvar *obj_IDENTICAL$nonidentical* 'void)
; op obj_IDENTICAL$setup : ->
(defun obj_IDENTICAL$setup ()
  (setq *obj_IDENTICAL$module* (modexp_eval$eval "IDENTICAL"))
  (setq *obj_IDENTICAL$identical*
    (mod_eval$$find_operator_in
     *obj_IDENTICAL$module* '("_" "===" "_")
     (list *obj$sort_Universal* *obj$sort_Universal*)
     *obj_BOOL$sort_Bool*))
  (setq *obj_IDENTICAL$nonidentical*
    (mod_eval$$find_operator_in
     *obj_IDENTICAL$module* '("_" "=/==" "_")
     (list *obj$sort_Universal* *obj$sort_Universal*)
     *obj_BOOL$sort_Bool*))
  )

;;; obj_BUILT-IN
(defvar obj_BUILT-IN$keyword "built-in:")
(defstruct (bi-term (:print-function print$built-in)) val)
(defun print$built-in (x stream depth)
  (declare (ignore depth))
  (princ obj_BUILT-IN$keyword stream) (princ " " stream)
  (let ((*standard-output* stream)) (print$bi_rhs (bi-term-val x))))
(defun obj_BUILT-IN$is_Built-in_token (token) (typep token 'bi-term))
(defun obj_BUILT-IN$create_Built-in (token) (bi-term-val token))
(defun obj_BUILT-IN$print_Built-in (x)
  (princ obj_BUILT-IN$keyword) (princ " ") (print$bi_rhs x))
    ; was print$name, prin1
(defun obj_BUILT-IN$is_Built-in (x) (declare (ignore x)) t)
(defvar obj_BUILT-IN$sort_Built-in 'none)
(defun obj_BUILT-IN$install ()
  (setq obj_BUILT-IN$sort_Built-in
	(mod_eval$$find_sort_in (modexp_eval$eval "BUILT-IN") "Built-in")))

;;; obj_NZNAT
(defun obj_NZNAT$is_NzNat_token (token)
  (and (stringp token)
       (every #'digit-char-p token)
       ;(not (eql #\0 (char token 0))) ;???
       (some #'(lambda (ch) (not (eql #\0 ch))) token)))
(defun obj_NZNAT$create_NzNat (token) (read-from-string token))
(defun obj_NZNAT$is_NzNat (x) (and (integerp x) (< 0 x)))

;;; obj_NAT
(defun obj_NAT$is_Zero_token (x) (equal "0" x))
(defun obj_NAT$create_Zero (x) (declare (ignore x)) 0)
(defun obj_NAT$is_Zero (x) (= 0 x))

(defun obj_NAT$is_Nat_token (token) (declare (ignore token)) nil)
(defun obj_NAT$create_Nat (token) (read-from-string token))
(defun obj_NAT$is_Nat (x) (and (integerp x) (<= 0 x)))

;;; obj_INT
(defun obj_INT$is_NzInt_token (token)
  (and (stringp token)
       (general_read$$numberp token)
       (<= 2 (length token))
       (eql #\- (char token 0))
       ;(not (eql #\0 (char token 1))) ;???
       (some #'(lambda (ch) (not (eql #\0 ch))) token)))
(defun obj_INT$create_NzInt (x) (read-from-string x))
(defun obj_INT$is_NzInt (x) (and (integerp x) (not (= 0 x))))

;old: (defun obj_INT$is_Int_token (token) (general_read$$numberp token))
(defun obj_INT$create_Int (token) (read-from-string token))
(defun obj_INT$is_Int_token (tok) (declare (ignore tok)) nil)
(defun obj_INT$print_Int (x) (prin1 x))
(defun obj_INT$is_Int (x) (integerp x))

;;; obj_RAT
(defun obj_RAT$is_NzRat_token (token)
  (and (stringp token)
       (every #'(lambda (x)
           (or (digit-char-p x)
	       (eql #\- x)
	       (eql #\/ x)))
	 token)
       (let* ((first (if (eql #\- (char token 0)) 1 0))
	      (slash (position #\/ token)))
	 (and 
	  slash
	  (obj_NZNAT$is_NzNat_token (subseq token first slash))
	  (obj_NZNAT$is_NzNat_token (subseq token (+ slash 1)))))))
(defun obj_RAT$create_NzRat (x) (read-from-string x))
(defun obj_RAT$is_NzRat (x) (and (rationalp x) (not (= 0 x))))

(defun obj_RAT$is_Rat_token (tok) (declare (ignore tok)) nil)
(defun obj_RAT$create_Rat (x) (read-from-string x))
(defun obj_RAT$print (x)
  (if (typep x 'ratio)
      (progn (prin1 (numerator x)) (princ "/") (prin1 (denominator x)))
    (prin1 x)))

;;; obj_ID
(defun obj_ID$is_Id_token (token)
  (and (stringp token)
       (<= 1 (length token))
       (alpha-char-p (char token 0))
       ;(every #'alphanumericp token)
       ))
(defun obj_ID$create_Id (token) (intern token))
(defun obj_ID$print_Id (x) (princ (string x)))
(defun obj_ID$is_Id (x)
  (and (symbolp x)
       (obj_ID$is_Id_token (symbol-name x))))

;;; obj_QID
(defun obj_QID$is_Id_token (token)
  (and (stringp token)
       (<= 2 (length token))
       (eql #\' (char token 0))
       (alpha-char-p (char token 1))
       ;(every #'alphanumericp (subseq token 1))
       ))
(defun obj_QID$create_Id (token) (intern (subseq token 1)))
(defun obj_QID$print_Id (x) (princ "'") (princ (string x)))
(defun obj_QID$is_Id (x)
  (and (symbolp x)
       (obj_QID$is_Id_token (symbol-name x))))

;;; obj_FLOAT
(defun obj_FLOAT$is_Float_token (token)
  (and
   (stringp token)
   (or (digit-char-p (char token 0))
       (and (member (char token 0) '(#\+ #\. #\-))
            (<= 2 (length token))
            (digit-char-p (char token 1))))
   (multiple-value-bind (res len) (read-from-string token)
     (declare (ignore res))
     (and (= (length token) len)
          (member (type-of (read-from-string token))
                  '(#+LUCID float long-float short-float fixnum bignum ratio
                    #+CMU float short-float long-float single-float double-float
                    #+CLISP float long-float short-float single-float double-float
                    #+GCL single-float double-float short-float long-float
                    ))))))
(defun obj_FLOAT$create_Float (token)
  (coerce (read-from-string token) 'long-float))
(defun obj_FLOAT$print_Float (val) (prin1 val))
(defun obj_FLOAT$is_Float (val) (eq 'long-float (type-of val)))

(defun obj$prelude_install ()
  (unless *print$ignore_mods*
    (setq *print$ignore_mods*
      (mapcar #'modexp_eval$eval
	'("TRUTH-VALUE" "TRUTH" "BOOL" "NZNAT" "NAT" "INT" "RAT"))))
  )

;;; LISP
(defvar obj_LISP$keyword "lisp:")
(defstruct (lisp (:print-function print$lisp)) val)
(defun print$lisp (x stream depth)
  (declare (ignore depth))
  (princ obj_LISP$keyword stream) (princ " " stream)
  (prin1 (lisp-val x) stream))
(defvar obj_LISP$eval_input nil)
(defun obj_LISP$is_Lisp_token (token) (typep token 'lisp))
(defun obj_LISP$create_Lisp (token)
  (if obj_LISP$eval_input
    (eval (lisp-val token))
    (lisp-val token)))
(defun use (x) (throw 'direct-value x))
(defun obj_LISP$print_Lisp (x) (princ obj_LISP$keyword) (princ " ") (prin1 x))
(defun obj_LISP$is_Lisp (x) (declare (ignore x)) t)

(defun re-install-prelude ()
  (obj_UNIVERSAL$install)
  (obj_ERR$install)
  (obj_BUILT-IN$install)
  (obj_TRUTH$setup)
  ;(obj_TRUTH$install) ;don' want?
  ;(mod_eval$$!process_final *obj_TRUTH$module*)
  (obj_BOOL$setup)
  (obj_IDENTICAL$setup)
  (obj_LAST-TERM$install)
  (obj_BUILT-IN$install)
  (setq *print$ignore_mods* nil) ;goes with the following
  (obj$prelude_install)
  (setq theory_info_array
      (make-array '(16) :initial-contents
	(list
	 the_empty_property the_Z_property the_I_property the_IZ_property
	 the_C_property the_CZ_property the_CI_property the_CIZ_property
	 the_A_property the_AZ_property the_AI_property the_AIZ_property
	 the_AC_property the_ACZ_property the_ACI_property the_ACIZ_property
     )))
  'done
  )
