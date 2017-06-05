(in-package :om)

;;;
;;; Time-stamp: <2017-03-20 14:57:37 andersvi>
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
;;; Copyright (C) 2017 Anders Vinjar, <anders (dot) vinjar (at) bek (dot) no>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.


(defpackage "PATTERNS"
  (:nicknames :ompa)
  (:use :common-lisp :om)
  (:export :patterns :cycle :palindrome :line :heap :weighting
	   :markov :markov-analyze :graph :accumulation :thunk
	   :rotation :rewrite :range :join :pval :copier
	   :new :next :*pattern-version* :*pattern-date*))

(import '(clos:slot-definition-name 
          clos:slot-definition-initargs 
          clos:slot-definition-initform 
          clos:class-direct-superclasses
          clos:class-direct-subclasses
	  clos:finalize-inheritance
          hcl:class-slots
          hcl:class-direct-slots
          hcl:validate-superclass
          mp:without-interrupts)
	:ompa)

(let* ((srcdir (append (pathname-directory *load-pathname*) '("sources")))
       (pattern-files '("utils" "pattern-classes" "OM-classes")))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f :type "lisp")))
	pattern-files))

;; (om::addpackage2pack (find-library "PATTERNS") om::*om-package-tree*)

(let ((funcs '(p-next p-eod? p-eop?
	       p-cycle p-palindrome p-line p-heap p-weighting p-markov p-markov-analyze
	       p-graph p-accumulation p-thunk p-rotation p-rewrite
	       p-range p-join p-copier pval
	       ))
      (p (find-library "PATTERNS")))
  (AddGenFun2Pack funcs p))

(set-lib-release ompa:*pattern-version* (find-library "PATTERNS"))

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
	 ompa:*pattern-version*
	 ompa:*pattern-date*))

;; generate html doc:
;; (om::gen-lib-reference (exist-lib-p "Patterns"))
