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

;; $Id: obj_trace.lsp,v 205.2.1.1 2003/09/23 13:49:22 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  OBJ trace routines 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; see also: lib/functions.lsp

(defvar obj_trace_time 0)
(defvar obj_traced_functions nil)
(defvar obj_trace_struct t)
(defvar obj_trace_context t)
(defvar obj_trace_stack '(top))
(defvar obj_trace_print_args_fn nil)
(defvar obj_trace_print_values_fn nil)
(defvar obj_trace_print_name_return t)
(defvar obj_trace_indent nil)
(defvar obj_trace_indent_incr 2)
(defvar obj_trace_printing t)
(defvar obj_trace_force t)
(defvar obj_trace_start 0)
(defvar obj_trace_end 1000000000)

(defun obj_print_indent (n)
  (dotimes (i (* n obj_trace_indent_incr)) (princ " ")))

(defun obj_print_current_indent ()
  (dotimes (i (* (1- (length obj_trace_stack))  obj_trace_indent_incr))
    (princ " ")))

(defun obj_print_current_indent_m1 ()
  (dotimes (i (* (- (length obj_trace_stack) 2)  obj_trace_indent_incr))
    (princ " ")))

(defun obj_trace_reset ()
  (setq obj_trace_time 0))

(defmacro obj_trace (&rest l)
  `(obj_trace_fn ',l))

(defun obj_trace_fn (&optional (l nil))
  (if (null l) obj_traced_functions
    (mapcar #'obj_trace_function l)))

(defun obj_trace_function (fn)
  (if (not (fboundp fn))
      (progn (prin1 fn) (princ " isn't a function") (terpri)
	     (list 'undef fn))
  (if (obj_is_traced fn)
      (progn (prin1 fn) (princ " is already traced") (terpri)
	     (list 'already_traced fn))
  (progn
    (obj_trace_fcn fn)
    (pushnew fn obj_traced_functions)
    fn))))

(defun obj_retrace_fn (&optional (l nil))
  (if (null l) (if (null obj_traced_functions) 'none
		 (obj_retrace_fn obj_traced_functions))
    (mapcar #'(lambda (fn)
		(when (and (fboundp fn) (not (obj_is_traced fn)))
		  (pushnew fn obj_traced_functions)
		  (obj_trace_fcn fn)))
	    l)))

(defun obj_trace_fcn (fn)
  (let ((gen (gensym))
	(defn (symbol-function fn)))
    (setf (symbol-function gen) defn)
    (define-function fn
	(function-build fn '(&rest obj_args)
		 `((obj_trace_call ',fn ',gen obj_args))))
    fn))

(defun obj_trace_call (name fn args)
  (let ((start_time obj_trace_time))
    (incf obj_trace_time)
    (when (and obj_trace_printing
	       (<= obj_trace_start start_time)
	       (<= start_time obj_trace_end))
    (when obj_trace_indent (obj_print_current_indent))
    (prin1 start_time) (princ "=> ") (prin1 name)
        (when obj_trace_context
	      (princ " in ") (prin1 (car obj_trace_stack)))
        (obj_trace_print_args name args))
    (let ((obj_trace_stack
	   (if obj_trace_context (cons start_time obj_trace_stack)
	     '(error))))
    (let ((results (multiple-value-list (apply fn args))))
      (when (and obj_trace_printing
	       (<= obj_trace_start start_time)
	       (<= start_time obj_trace_end))
      (when obj_trace_indent (obj_print_current_indent_m1))
      (prin1 start_time) (princ "<=") 
      (when obj_trace_print_name_return
	(princ " ")
        (prin1 name)
        (when obj_trace_context
	      (princ " in ") (prin1 (cadr obj_trace_stack))))
      (obj_trace_print_values name results))
      (values-list results)))))

(defun obj_trace_print (x)
  (if obj_trace_struct
    (print$struct x)
    (print$obj x nil)))

(defun obj_trace_print_args (nm x)
  (if (and (symbolp nm) (get nm 'print_args))
      (funcall (get nm 'print_args) x)
  (if obj_trace_print_args_fn
      (funcall obj_trace_print_args_fn x)
  (obj_trace_print_args_default x))))

(defun obj_trace_print_args_default (x)
  (if obj_trace_struct
    (progn (terpri) (print$struct x))
    (progn (princ " ") (print$obj x nil)))
  (terpri)
  (obj_trace_force_output))

(defun obj_trace_print_values (nm x)
  (if (and (symbolp nm) (get nm 'print_values))
      (funcall (get nm 'print_values) x)
  (if obj_trace_print_values_fn
      (funcall obj_trace_print_values_fn x)
  (obj_trace_print_values_default x))))

(defun obj_trace_print_values_default (x)
  (if obj_trace_struct
    (progn (terpri) (print$struct x))
    (progn (princ " ") (print$obj x nil)))
  (terpri)
  (obj_trace_force_output))

(defun obj_trace_force_output ()
  (when obj_trace_force (force-output)))

(defmacro obj_untrace (&rest l)
  `(obj_untrace_fn ',l))

(defun obj_untrace_fn (l)
  (mapcar #'obj_untrace_function
    (if (null l) obj_traced_functions l)))

(defun obj_untrace_function (fn)
  (cond
   ((obj_is_traced fn)
    (setf (symbol-function fn)
	  (symbol-function (cadr (caddr (car (function-body fn))))))
    (setq obj_traced_functions (delete fn obj_traced_functions))
    fn
    )
   (t (format t "not traced: ~s~%" fn)
      (setq obj_traced_functions (delete fn obj_traced_functions))
      (list fn 'not_traced)
      )))

(defun obj_trace_extract (fn)
  (if (fboundp fn)
    (if (obj_is_traced fn)
      (symbol-function (cadr (caddr (car (function-body fn)))))
      (symbol-function fn))
    'undefined))

(defun obj_is_traced (fn)
  (and (fboundp fn)
       (not (is-compiled fn))
       (let ((body (function-body fn)))
	 (and (consp (car body))
	      (equal 'obj_trace_call (car (car body)))))))

; quiet versions produce no output, but are recorded in the context
(defmacro obj_trace_quiet (&rest l)
  `(obj_trace_quiet_fn ',l))

(defun obj_trace_quiet_fn (&optional (l nil))
  (if (null l) obj_traced_functions
    (mapcar #'obj_trace_quiet_function l)))

(defun obj_trace_quiet_function (fn)
  (if (not (fboundp fn))
      (progn (prin1 fn) (princ " isn't a function") (terpri)
	     (list 'undef fn))
  (if (obj_is_traced fn)
      (progn (prin1 fn) (princ " is already traced") (terpri)
	     (list 'already_traced fn))
  (progn
    (obj_trace_quiet_fcn fn)
    (pushnew fn obj_traced_functions)
    fn))))

(defun obj_trace_quiet_fcn (fn)
  (let ((gen (gensym))
	(defn (symbol-function fn)))
    (setf (symbol-function gen) defn)
    (define-function fn
	(function-build fn '(&rest obj_args)
		 `((obj_trace_quiet_call ',fn ',gen obj_args))))
    fn))

(defun obj_trace_quiet_call (name fn args)
  (declare (ignore name))
  (let ((start_time obj_trace_time))
    (incf obj_trace_time)
    (let ((obj_trace_stack
	   (if obj_trace_context (cons start_time obj_trace_stack)
	     '(error))))
      (apply fn args))))

;;;; obj advise

(defun obj_advise_fcn (fn exprs)
  (let ((gen (gensym))
	(defn (symbol-function fn)))
    (setf (symbol-function gen) defn)
    (define-function fn
	(function-build fn (function-args defn)
           `('(advise ,gen)
	     ,@(subst gen :fcn exprs))))
    fn))

(defun obj_unadvise_function (fn)
  (cond
   ((obj_is_advised fn)
    (setf (symbol-function fn)
	  (symbol-function (cadr (cadr (car (function-body fn))))))
    fn
    )))

(defun obj_advise_extract (fn)
  (if (fboundp fn)
    (if (obj_is_advised fn)
      (symbol-function (cadr (cadr (car (function-body fn)))))
      (symbol-function fn))
    'undefined))

(defun obj_is_advised (fn)
  (and (fboundp fn)
       (not (is-compiled fn))
       (let ((body (function-body fn)))
	 (and (consp (car body))
	      (equal 'quote (car (car body)))
	      (consp (cdr (car body)))
	      (consp (cadr (car body)))
	      (equal 'advise (car (cadr (car body))))))))
