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

;; $Id: hash.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

;; Hash routines
;; DANGER -- this is implementation dependent.
;; This is necessary for efficient memoization.  This provides more
;; functions than necessary hasheq should have been part of Common
;; Lisp.

(defun hashequal (x) (sxhash x))

#+GCL
(defCfun "static object hasheql(x) object x;" :int
  " Creturn(make_fixnum(hasheq(x)));")

#+GCL
(defentry hasheql (object) (object hasheql))

#+GCL
;; Addresses are typically multiples of 4; so this is not a very good
;; hash function in itself
(defCfun "static object hasheq(x) object x;" :int
  " Creturn(make_fixnum(((((int)x) & 0x7fffffff)+3)>>3));")

#+GCL
(defentry hasheq (object) (object hasheq))

#+(or LUCID CMU CLISP)
(defun hasheq (object) (truncate (addr_of object) 4))

#+(or LUCID CMU CLISP)
;; This is only an approximation.
(defun hasheql (object)
  (if (integerp object) object
      (if (numberp object)
          (hasheqlnumber object)
          (if (characterp object) (char-int object)
              (hasheq object)))))

#+(or LUCID CMU CLISP)
;; This is written for a 32 bit machine.
(defun hasheqlnumber (n)
  (if (integerp n) n
      (if (floatp n)
          (multiple-value-bind (fr exp sgn) (decode-float n) (declare (ignore sgn))
                               (hash-comb
                                (if (minusp n)
                                    (- (fround (* fr 1000000000)))
                                    (fround (* fr 1000000000)))
                                exp))
          (if (rationalp n)
              (hash-comb (hasheqlnumber (numerator n)) (hasheqlnumber (denominator n)))
              (if (complexp n)
                  (hash-comb (hasheqlnumber (realpart n)) (hasheqlnumber (imagpart n)))
                  ;; for our purposes it is probably better to return something than to fail
                  (hasheq n))))))

;;; DANGER -- this is implementation dependent

(defun tophash (x)
  (cond
    ((typep x 'sequence)
     (let ((len (length x))
           (val (hasheql (type-of x))))
       (dotimes (i len)
         (setq val (hash-comb val (elt x i))))
       val))
    (t (hasheql x))))

(defun hash-comb (x y)
  (logxor (ash x -5) y (ash (logand x 31) 26)))

(defun topequal (x y)
  (cond
    ((typep x 'sequence)
     (cond
       ((typep y (type-of x))
        (let ((lenx (length x))
              (leny (length y)))
          (cond
            ((= lenx leny)
             (dotimes (i lenx t)
               (when (not (eql (elt x i) (elt y i)))
                 (return nil))))
            (t nil))))
       (t nil)))
    (t (eql x y))))

;----------------------------------------------------------------------

;;; a separate version of hash table; no rehashing (for now)
;;; provide hashfcn and comparison function -- need to be consistent
;;; i.e.  comp(a)=comp(b) => hashfn(a)=hashfn(b)

;;; looking at CLtR book:
;;; make-hashtab size test
;;; gethashtab key table
;;; sethashtab key table value --- implicit function
;;; --- remhashtab key table: won't provide to start
;;; clrhashtab table
;;; -- below the names will be changed

;;; We will use out of table chaining to resolve collisions.

;;; General representation.
(defstruct (hashtab
             (:conc-name hashtab$)
             (:constructor hashtab$create (hashfn compfn size table)))
  "an simple alternative form of hashtable; full choice of functions"
  hashfn
  compfn
  size
  table
  )

;; Representation: table is array of initial size; entries of this
;; table are a-lists of key-value to an entry.

(defun hashtab$make (size test compar)
  (hashtab$create test compar size (make-array size :initial-element nil))
  )

(defun hashtab$get (key hashtab)
  (let ((val (funcall (hashtab$hashfn hashtab) key)))
    (let ((ent (aref (hashtab$table hashtab) (mod val (hashtab$size hashtab)))))
      (cdr (assoc key ent :test (hashtab$compfn hashtab))))))

(defun hashtab$set (key hashtab avalue)
  (let ((val (funcall (hashtab$hashfn hashtab) key))
        (tabl (hashtab$table hashtab)))
    (let ((ind (mod val (hashtab$size hashtab))))
      (let ((ent (aref tabl ind)))
        (let ((pr (assoc key ent :test (hashtab$compfn hashtab))))
          (if pr (rplacd pr avalue)
              (setf (aref tabl ind) (cons (cons key avalue) ent))))))))

(defun hashtab$clear (hashtab)
  (let ((tabl (hashtab$table hashtab)))
    (dotimes (i (hashtab$size hashtab))
      (setf (aref tabl i) nil))))

(defun hashtab$rem (key hashtab)
  (let ((val (funcall (hashtab$hashfn hashtab) key))
        (tabl (hashtab$table hashtab)))
    (let ((ind (mod val (hashtab$size hashtab))))
      (let ((ent (aref tabl ind)))
        (let ((last nil) (pos ent) (compfn (hashtab$compfn hashtab)) (found nil))
          (loop
             (when (null pos) (return))
             (when (funcall compfn key (caar pos))
               (setq found t)
               (return))
             (setq last pos  pos (cdr pos)))
          (when found
            (if last
                (rplacd last (cdr pos))
                (setf (aref tabl ind) (cdr pos)))))))))

; will mapping removals work?
(defun hashtab$map (fn hashtab)
  (let ((tabl (hashtab$table hashtab)))
    (dotimes (i (hashtab$size hashtab))
      (dolist (ae (aref tabl i))
        (funcall fn (car ae) (cdr ae))))))

;----------------------------------------------------------------------
; functions for memoization
;----------------------------------------------------------------------

(defun hash$term (tm)
  (cond
    ((term$is_var tm) (hasheq tm))
    ((term$is_built_in_constant tm)
     (hash-comb
      (hasheq (term$sort tm))
      (hashequal (term$built_in_value tm))))
    (t (let ((res (hasheq (operator$name (term$head tm)))))
         ;; Nota Bene (this last line)
         (dolist (st (term$subterms tm))
           (setq res (hash-comb res (hash$term st))))
         res))))

