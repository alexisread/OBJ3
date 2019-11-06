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

;; $Id: type-of-safe.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;
; A safe type-of function (never calls system exit).
; DANGER -- This is implementation dependent.
; type-of-safe can be replaced by constant true function.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+GCL (defun type-of-safe (x)
        (declare (ignore x))
        t)

#+GCL (defun is-illegal-type (x)
        (declare (ignore x))
        nil)

#+GCL (defCfun "object typecodesafe(x) object x;" :int
        "Creturn(make_fixnum(type_of(x)));")

#+GCL (defentry typecodesafe (object) (object typecodesafe))

#+GCL (defvar typecode-start 0)

#+GCL (defvar typecode-end 21)

#+GCL (defvar typecode-names 
        '(cons fixnum bignum ratio shortfloat
          longfloat complex character symbol package
          hashtable array vector string bitvector
          structure stream random readtable pathname
          cfun cclosure spice end contiguous
          relocatable other))

;; @review kiniry 25 Sept 2003 - What are there two declarations of
;; is-illegal-type and type-of-safe for GCL?

#+GCL (defun is-illegal-type (x)
        (let ((code (typecodesafe x)))
          (not (and (<= typecode-start code) (<= code typecode-end)))))

#+GCL (defun type-of-safe (x)
        (let ((code (typecodesafe x)))
          (if (and (<= typecode-start code) (<= code typecode-end))
              (nth code typecode-names)
              'illegal)))

#+(or LUCID CMU CLISP) (defun is-illegal-type (x) (declare (ignore x)) nil)

#+(or LUCID CMU CLISP) (defun type-of-safe (x) (type-of x))
