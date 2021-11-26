;;; Time-stamp: <2021-11-26 14:06:56 andersvi>
;;;
;;; Patterns library for OM, Anders Vinjar
;;;
;;; This library is an adaption of Rick Taube's "Item Streams" (or "Patterns
;;; Streams") as found in Common Music.  Variations of Rick's original "Item
;;; Streams" can be found in other composition environments, notably
;;; SuperColliders "Pattern" classes.
;;;
;;; A good general intro to Item Streams is found in chapter 20 of Rick Taube's
;;; book "Notes from the Metalevel":
;;; 
;;;	http://www.moz.ac.at/sem/lehre/lib/cm/Notes%20from%20the%20Metalevel/index.html
;;;
;;; Copyright (C) 2017 Anders Vinjar
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.

(in-package :om)

(let* ((srcdir (append (pathname-directory *load-pathname*) '("sources")))
       (pattern-files '("package" "utils" "pattern-classes" "OM-classes")))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f)))
	pattern-files))


(defparameter ompa::*pattern-version* '1.0)
(defparameter ompa::*pattern-date* '2019-09-23)

;; (om::addpackage2pack (find-library "PATTERNS") om::*om-package-tree*)

(let ((funcs '(p-next p-eod? p-eop?
	       p-cycle p-palindrome p-line p-heap
	       p-weighting p-w-node
	       p-markov p-markov-analyze p-transition
	       p-graph p-graph-node p-accumulation p-thunk p-rotation
	       p-rewrite p-rewrite-node
	       p-range p-join p-copier
	       pval set-pval get-pval
	       ))
      (p (find-library "PATTERNS")))
  (AddGenFun2Pack funcs p))


(set-lib-release ompa::*pattern-version* (find-library "PATTERNS"))

;; (om::addclass2pack '(ompa::pattern) (find-library  "PATTERNS") :protect nil)


(print
 (format nil "
;; ============================================
;;  Patterns - a Pattern Stream Library for OM
;;  Version:	~A
;;  Date:	~A
;;  Author:	Anders Vinjar
;; ============================================
"
	 ompa::*pattern-version*
	 ompa::*pattern-date*))

;; generate html doc:
;; (om::gen-lib-reference (exist-lib-p "Patterns"))
