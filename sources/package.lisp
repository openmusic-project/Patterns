
(in-package :om)


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
          mp:without-interrupts)@
	:ompa)