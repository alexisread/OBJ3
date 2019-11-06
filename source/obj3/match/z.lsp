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

;; $Id: z.lsp,v 206.1 2003/09/29 12:46:23 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    All the procedures specific to the theory with only identity
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Patrick Lincoln     May 1990, August 1990
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct z-state
  n
  sys)

;;; t1 of each eq of sys is pattern, has variables.  t2 is (ground) term.

(defun Z$state_initialize (sys env)
  env ;; why isn't env used here or in C$?
  (values (make-z-state :n 0 :sys sys) nil))

(defun Z$next_state (Z_st)
  (let* ((sys (z-state-sys Z_st))
	 (point (system$list_eq sys))
	 (equation nil)
	 (r 0)
	 (t1 nil)
	 (t2 nil)
	 (new_sys (system$new))
	 (lg (length point))
	 (op1 nil)
	 (op2 nil)
	 )
    (do* ((N (z-state-n Z_st))
          (q N N)
          (point2 point point))
         ((or (not (system$is_empty new_sys))
              (>= N (expt 5 lg)))
          (progn 
            (setf (z-state-n Z_st) N)
            (if (not (system$is_empty new_sys))
                (values new_sys Z_st nil) ;success case
                (values nil nil t))))     ; fail case
      (incf N)                            ; try the next N
      (dotimes (k lg)                     ; k = lg,...,1
                                        ; this treats q as a bitvector in base 5
        #+GCL (setq r (rem q 5) q (truncate q 5))
        #-GCL (multiple-value-setq (q r) (truncate q 5))
        (setq equation (car point2)
              point2 (cdr point2)
              t1 (match_equation$t1 equation)
              t2 (match_equation$t2 equation)
              op1 (if (or (term$is_constant t1)
                          (term$is_var t1))
                      nil 
                      (term$head t1))
              op2 (if (or (term$is_constant t2)
                          (term$is_var t2))
                      nil 
                      (term$head t2)))
        (cond ((and (= r 0)          ; as if no thoery applied - 11 22
                    op1 op2)
               (system$add_eq new_sys 
                              (match_equation$create (term$arg_1 t1) 
                                                     (term$arg_1 t2)))
               (system$add_eq new_sys 
                              (match_equation$create (term$arg_2 t1) 
                                                     (term$arg_2 t2))))
              ((and (= r 1)
                    op1                 ; term is non atomic
                    (not (term$is_zero_for_op (term$arg_1 t1) op1)))
               (system$add_eq new_sys
                              (match_equation$create (term$arg_1 t1)
                                                     (term$make_zero op1)))
               (system$add_eq new_sys
                              (match_equation$create (term$arg_2 t1) t2)))
              ((and (= r 2)
                    op1                 ; term is non atomic
                    (not (term$is_zero_for_op (term$arg_2 t1) op1)))
               (system$add_eq new_sys
                              (match_equation$create (term$arg_2 t1)
                                                     (term$make_zero op1)))
               (system$add_eq new_sys
                              (match_equation$create (term$arg_1 t1) t2)))
              ;; note these are redundant if we have terms 
              ;; in normal form (no identities).
              ((and (= r 3)
                    op2                 ; term is non atomic
                    (not (term$is_zero_for_op (term$arg_1 t2) op2)))
               (system$add_eq new_sys
                              (match_equation$create (term$make_zero op2)
                                                     (term$arg_1 t2)))
               (system$add_eq new_sys
                              (match_equation$create t1 (term$arg_2 t2))))
              ((and (= r 4)
                    op2                 ; term is non atomic
                    (not (term$is_zero_for_op (term$arg_2 t2) op2)))
               (system$add_eq new_sys
                              (match_equation$create (term$make_zero op2)
                                                     (term$arg_2 t2)))
               (system$add_eq new_sys
                              (match_equation$create t1 (term$arg_1 t2))))
              (t nil))))))

;; Z$equal assumes non-atomic args.
(defun Z$equal (t1 t2)
  "predicate which is true if t1 and t2 are equal modulo identity rule"
  (let ((op1 (term$head t1))
	(op2 (term$head t2)))
    (or (term$similar t1 t2)            ; was equal
        (and (term$is_zero_for_op (term$arg_1 t1) op1)
             (term$equational_equal (term$arg_2 t1) t2))
        (and (term$is_zero_for_op (term$arg_2 t1) op1)
             (term$equational_equal (term$arg_1 t1) t2))
        (and (term$is_zero_for_op (term$arg_1 t2) op2)
             (term$equational_equal t1 (term$arg_2 t2)))
        (and (term$is_zero_for_op (term$arg_2 t2) op2)
             (term$equational_equal t1 (term$arg_1 t2)))
        (and (term$equational_equal (term$arg_1 t1) (term$arg_1 t2))
             (term$equational_equal (term$arg_2 t1) (term$arg_2 t2))))))
