;;; Copyright (C) 2017 Anders Vinjar, <anders (dot) vinjar (at) bek (dot) no>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License.  See
;;; http://www.cliki.net/LLGPL for the text of this agreement.

(in-package :om)

;; (defparameter options-for-all-patterns '(name for eop-hook repeat))
;; (defparameter options-and-values-for-all-patterns '(:name name :for for :eop-hook eop-hook repeat t))

(defparameter pattern-icon 3144)
;; (defparameter pattern-icon 3142)

(defmethod! p-next ((obj t) &optional num)
  :icon pattern-icon
  :indoc '("pattern")
  :doc "Returns next item or [num next] items from a pattern.
	If 'num'=t returns one generations worth of items"
  (ompa::next obj num))

(defmethod! p-eop? (x)
  :icon pattern-icon
  (ompa::eop? x))

(defmethod! p-eod? (x)
  :icon pattern-icon
  (ompa::eod? x))


;; TODO: add rest of slots: traversing, counting, through, parser...

(defmethod! p-cycle (of &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("data" "for" "repeat" "of pattern" "returning" "eop-hook")
  :doc "Returns a cycle pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::cycle :of of 
			     :name name :for for :eop-hook eop-hook
			     :repeat repeat :returning returning))

(defmethod! p-palindrome (of elide &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :menuins '((1 (("t" t) ("nil" nil))))
  :indoc '("of" "elide?" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a palindrome pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::palindrome :of of :elide elide
				  :name name :for for :eop-hook eop-hook
				  :repeat repeat :returning returning))

(defmethod! p-line (of &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a line pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::line :of of
			    :name name :for for :eop-hook eop-hook
			    :repeat repeat :returning returning))

(defmethod! p-heap (of &key elide-last? for (repeat t) name (state *random-state*) returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "elide-last?" "for" "repeat" "name" "state" "returning" "eop-hook")
  :doc "Returns a heap pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::heap :of of
			    :elide-last? elide-last?
			    :state state 
			    :name name :for for :eop-hook eop-hook
			    :repeat repeat :returning returning))

(defmethod! p-weighting (of &key for (repeat t) name (state *random-state*) returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "repeat" "name" "state" "returning" "eop-hook")
  :doc "Returns a weighting pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::weighting :of of
				 :state state
				 :name name :for for :eop-hook eop-hook
				 :repeat repeat :returning returning))

(defmethod! p-w-node (x &key (weight 1) min max)
  :icon pattern-icon
  :indoc '("x" "weight" "min" "max")
  :initvals '(nil 1 nil nil)
  :doc "returns a node for a p-weighting pattern"
  (let ((min? (when min (list :min min)))
	(max? (when max (list :max max))))
    `(,x :weight ,weight ,@min? ,@max?)))


(defmethod! p-markov (of &key for past (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "past" "repeat" "name" "state" "returning" "eop-hook")
  :doc "Returns a markov pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::markov :of of
			      :past past 
			      :name name :for for :eop-hook eop-hook :repeat repeat
			      :past past :returning returning))

(defmethod! p-transition (lhs rhs)
  :icon pattern-icon
  :initvals '(nil nil)
  :indoc '("left hand side" "right hand side")
  :doc "returns a transition rule for use with patterns"
  (append (list! lhs) '(:->) (list! rhs)))


(defmethod! p-markov-analyze ((seq list) &key (order 1) (print? nil) (pattern? t)
					 sort? (print-decimals 3) (period nil) key returning)
  :indoc '("seq" "order" "print?" "pattern?" "sort?" "print-decimals" "period" "key" "returning")
  :icon pattern-icon
  :doc "(markov-analyze list [:order i] [:print? b] [:pattern? b] [:key l])

Performs an analysis of elements in list and prints or returns the results."
  (ompa::markov-analyze seq
			:order order :print? print? :pattern? pattern?
			:sort? sort? :print-decimals print-decimals
			:period period :key key :returning returning))

(defmethod! p-graph (of &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a graph pattern"
  (assert repeat (repeat) "repeat must be 't' or a number")
  (make-instance 'ompa::graph :of of
			     :name name :for for :eop-hook eop-hook
			     :repeat repeat :returning returning))


;;; a graph node {element &key :id :to}:

(defmethod! p-graph-node (element &key id to)
  :icon pattern-icon
  :initvals '(nil nil)
  :indoc '("element" "node id {integer | symbol}" "to {id | pattern}")
  :doc "Each element in the pattern is specified as a graph node list:

    (element {keyword value}+)

where element is the value or sub-pattern in the node and is followed by one or more keyword value pairs:

    :id {integer | symbol}

        Identifies the node with a unique name or number in the graph. If omitted the identifier defaults to the
        element, but since the element may be a sub-pattern or a list it is good practice to always provide explicit
        ids.

    :to {id | pattern}

        Specifies the transition rule for the graph node. The value may be a node identifier or a pattern of
        identifiers. This keyword can also be specified as a \"right-arrow\" :->."
  (let ((nodeform (if id (list :id id)))
	(toform (if to (list :to to))))
    `(,element ,@nodeform ,@toform)))

(defmethod! p-accumulation (of &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a accumulation pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::accumulation :of of
				    :name name :for for :eop-hook eop-hook
				    :repeat repeat :returning returning))

(defmethod! p-thunk ((function function) &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("function" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a pattern based on function 'thunk'"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::thunk :of function
			     :name name :for for :eop-hook eop-hook
			     :repeat repeat :returning returning))

(defmethod! p-rotation (of rotations &key for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "for" "repeat" "name" "returning" "eop-hook")
  :initvals '(nil (0 1 1))
  :doc "Returns a rotation pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::rotation :of of :rotations rotations
				:name name :for for :eop-hook eop-hook
				:repeat repeat :returning returning))

(defmethod! p-rewrite (of &key (rules nil rules?)
			  (initially nil initially?)
			  (generations nil generations?)
			  (for nil for?)
			  (repeat t repeat?)
			  (name nil name?)
			  returning
			  (eop-hook nil eop-hook?))
  :icon pattern-icon
  :indoc '("of" "rules" "initially" "generations" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a rewrite pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  ;; (make-instance 'ompa:rewrite :of of :rules rules :initially initially
  ;; 		 :generations generations :name name :for for
  ;; 		 :eop-hook eop-hook :repeat repeat)
  (eval `(make-instance 'ompa::rewrite
			:of ',of
			,@(when rules? `(:rules ',rules))
			,@(when initially? `(:initially ',initially))
			,@(when generations? `(:generations ',generations))
			,@(when name? `(:name ',name))
			,@(when for? `(:for ',for))
			,@(when eop-hook? `(:eop-hook ',eop-hook))
			,@(when repeat? `(:repeat ',repeat))
			,@(when returning `(:returning ',returning)))))

(defmethod! p-rewrite-node (element &key id to)
  :icon pattern-icon
  :initvals '(nil nil)
  :indoc '("element" "node id {integer | symbol}" "to {id | pattern}")
  :doc "Each element in the pattern is specified as a graph node list:

    (element {keyword value}+)

where element is the value or sub-pattern in the node and is followed by one or more keyword value pairs:

    :id {integer | symbol}

        Identifies the node with a unique name or number in the graph. If omitted the identifier defaults to the
        element, but since the element may be a sub-pattern or a list it is good practice to always provide explicit
        ids.

    :to {id | pattern}

        Specifies the transition rule for the graph node. The value may be a node identifier or a pattern of
        identifiers. This keyword can also be specified as a \"right-arrow\" :->."
  (let ((nodeform (if id (list :id id)))
	(toform (if to (list :-> to))))
    `(,element ,@nodeform ,@toform)))


(defmethod! p-range (&key (from 0 from?)
			  (initially nil initially?)
			  (to nil to?)
			  (below nil below?)
			  (downto nil downto?)
			  (above nil above?)
			  (by nil by?)
			  (stepping nil stepping?)
			  for (repeat t)
			  name
			  returning
			  eop-hook)
  :icon pattern-icon
  :indoc '("from" "initially" "to" "below" "downto" "above" "by" "stepping" "for" "name" "returning" "eop-hook")
  :initvals '(0 nil 1 1 nil)
  :doc "Returns a range pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (eval `(make-instance 'ompa::range
  			,@(when from? `(:from ',from))
  			,@(when initially? `(:initially ',initially))
  			,@(when to? `(:to ',to))
  			,@(when below? `(:below ',below))
  			,@(when downto? `(:downto ',downto))
  			,@(when above? `(:above ',above))
  			,@(when by? `(:by ',by))
  			,@(when stepping? `(:stepping ',stepping))
  			:name ,name :for ,for :eop-hook ,eop-hook
  			:repeat ,repeat
  			:returning ,returning)))

#|

(setf aaa (p-range :from (p-cycle  '(60 74)) :downto 48 :by (p-cycle '(1 2 3)) :repeat 6 :returning #'(lambda (o) (cons ’a o))))


(setf aaa (p-range :below 10 :returning #'(lambda (o) (cons ’a o))))
(p-next aaa t)
(p-next (p-range :from 0 :by 1 :for 10) 20)

|#

(defmethod! p-join (of &key format for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "format" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a joiner pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::join :of of :format format
			    :name name :for for :eop-hook eop-hook :repeat repeat
			    :returning returning))

(defmethod! p-copier (of &key repeat-for for (repeat t) name returning eop-hook)
  :icon pattern-icon
  :indoc '("of" "repeat-for" "for" "repeat" "name" "returning" "eop-hook")
  :doc "Returns a copier pattern"
  (assert repeat (repeat) "repeat must be set to 't' or a number")
  (make-instance 'ompa::copier :of of :repeat-for repeat-for
			      :name name :for for :eop-hook eop-hook :repeat repeat
			      :returning returning))



;;;
;;;  pval
;;;

(defmethod! pval (name)
  :icon pattern-icon
  :doc "returns a closure around name, in a null lexical environment"
  (let ((sym (intern (string name))))
    (setf (symbol-value sym) nil)
    (enclose (list 'lambda '() `,name) nil)))

(defmethod! set-pval (name val)
  :icon pattern-icon
  :doc "sets a named variable (created by pval)"
  (let ((sym (intern (string name))))
    (setf (symbol-value sym) val)))

(defmethod! get-pval (name &optional (value nil not-bound-value))
  :icon pattern-icon
  :doc "gets the value of a named variable (created by pval, set by set-pval)"
  (let ((sym (intern (string name))))
    (cond ((boundp sym) (symbol-value sym))
	  (not-bound-value value)
	  (t (error (format nil "pval ~A not bound" name))))))

#|

(pval 'yoyoba)
(setf xxx (p-cycle (list 1 2 (pval 'yoyoba))))
(p-next xxx t)
(set-pval 'yoyoba 12)
(p-next xxx t)

(get-pval 'nilsen)
(get-pval 'nilsen :kaboom)
(get-pval 'nilsen)

|#


