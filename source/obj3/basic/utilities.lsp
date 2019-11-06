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

;; $Id: utilities.lsp,v 206.2 2003/09/26 13:07:10 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                contains utilities functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary


(defun list2array (list)
  #+(or GENERIC CMU CLISP)
  (make-array (length list) :initial-contents list)
  #+GCL
  (let ((len (length list)))
  (let ((arr (si:make-vector t len nil nil nil 0 nil)) (i 0))
    (declare (fixnum i))
    (dolist (e list) (si:aset arr i e) (setq i (1+ i)))
    arr))
  )

; (a b a c d a) --> ((a.3)(b.1)(d.1)(c.1))

(defun list2multi_set_list_direct (list)
  (let ((ms_list nil))
    (dolist (x list)
	    ;; (ck) 30 dec 86      (let ((ms_x (assoc x ms_list))) ...)
	    (let ((ms_x (dolist (pr ms_list nil)
			  (when (term$equational_equal x (car pr))
			      (return pr))))) 
	      (if ms_x 
		  (rplacd ms_x (1+ (cdr ms_x)))
		(push (cons x 1) ms_list))
	      ))
    ms_list)
  )

(defvar l2msla1 nil) (defvar l2mslv1 nil)
(defvar l2msla2 nil) (defvar l2mslv2 nil)

(defun list2multi_set_list (list)
  (if (and l2msla1 (test_same_term_list list l2msla1))
      (copy-alist l2mslv1)
  (if (and l2msla2 (test_same_term_list list l2msla2))
      (progn
	(rotatef l2msla1 l2msla2)
	(rotatef l2mslv1 l2mslv2)
	(copy-alist l2mslv1))
  (let ((res (list2multi_set_list_direct list)))
    (setq l2msla2 l2msla1  l2mslv2 l2mslv1)
    (setq l2msla1 list l2mslv1 res)
    (copy-alist res)
    ))))

(defun test_same_term_list (x y)
 (loop
  (when (null x) (return (null y)))
  (unless
   (eq (car x) (car y))
   (return nil))
  (setq x (cdr x))
  (setq y (cdr y))
  ))

; ((a.1)(b.3)) --> (a b)

(defun multi_set_list2set (ms_list)
  (let ((result nil))
    (dolist (x ms_list)
      (push (car x) result)
      )
    result
    )
  )

; op util$make_list_1_n: Int -> List .

(defun util$make_list_1_n (n)
  (let ((result nil))
    (dotimes (x n)
	     (push (+ x 1) result)
	     )
    (reverse result)
    )
  )

; op util$make_list_1_n_0: Int -> List .

(defun util$make_list_1_n_0 (n)
  (let ((result nil))
    (dotimes (x n)
	     (push (+ x 1) result)
	     )
    (push 0 result)
    (reverse result)
    )
  ) 

;;; set equality
(defun set$equal (x y)
  (and (set$subset x y)
       (set$subset y x)))

(defun set$subset (x y)
  (and
   (<= (length x) (length y))
   (every #'(lambda (elt-x) (member elt-x y :test #'eq)) x)))
