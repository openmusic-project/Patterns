(in-package :om)


(defpackage "PATTERNS"
  (:nicknames :ompa)
  (:use :common-lisp))


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
