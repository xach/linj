;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Sun Dec 24 13:30:32 2000
;;; Copyright (C) eValuator, Lda

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "LINJ")

;;This file requires the Linj readtable
(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* *linj-readtable*))


(def-transform method-call-expression (expt ?x ?y) (pow ?x ?y))

(def-transform expression most-positive-fixnum (in (the java.lang.Long) +max-value+))
(def-transform expression most-negative-fixnum (in (the java.lang.Long) +min-value+))

(def-real-the-form numerator linj.bignum)
(def-real-the-form denominator linj.bignum)

(def-transform method-call-expression (ceiling ?x) (ceil ?x))
(def-transform method-call-expression (ceiling ?x ?y) (floor (1+ (/ ?x ?y))))

(def-transform method-call-expression (atan ?x ?y) (atan2 ?x ?y))
(def-transform method-call-expression (log ?x ?y) (/ (log ?x) (log ?y)))

(def-transform method-call-expression (evenp ?n) (= (rem ?n 2) 0))
(def-transform method-call-expression (oddp ?n) (/= (rem ?n 2) 0))
(def-transform expression (not (evenp ?x)) (oddp ?x))
(def-transform expression (not (oddp ?x)) (evenp ?x))

(def-transform method-call-expression (plusp ?n) (> ?n 0))
(def-transform method-call-expression (minusp ?n) (< ?n 0))
(def-transform expression (not (plusp ?n)) (<= ?n 0))
(def-transform expression (not (minusp ?n)) (>= ?n 0))

(def-transform expression (zerop ?x) (= ?x 0))
(def-transform expression (not (zerop ?x)) (/= ?x 0))

(def-transform expression (float ?x) (maybe-convert-to-type ?x float))

(def-transform expression (ffloor . ?args) (float (floor . ?args)))
(def-transform expression (fceiling . ?args) (float (fceiling . ?args)))
(def-transform expression (ftruncate . ?args) (float (ftruncate . ?args)))
(def-transform expression (fround . ?args) (float (fround . ?args)))

(def-transform expression (gcd) 0)
(def-transform expression (gcd ?x) (abs ?x))
(def-transform expression (gcd ?x ?y ?z . ?others) (gcd (gcd ?x ?y) ?z . ?others))

(def-transform expression (lcm ?x) (abs ?x))
(def-transform expression (lcm ?x ?y ?z . ?others) (lcm (lcm ?x ?y) ?z . ?others))

(def-transform expression (integer-length ?x) (send (maybe-convert-to-type ?x java.math.big-integer) bit-length))

(def-transform expression #'(lambda . ?rest)
  (lambda . ?rest))

;;We want to be able to automatically build lambda expressions.
;;Some of them are already pre-defined:

(def-transform expression #'cons
  (slot-value (the linj.cons) '+cons-function+))

(def-transform expression #'car
  (slot-value (the linj.cons) '+car-function+))

(def-transform expression #'cdr
  (slot-value (the linj.cons) '+cdr-function+))

(def-transform expression #'first
  (slot-value (the linj.cons) '+car-function+))

(def-transform expression #'second
  (slot-value (the linj.cons) '+second-function+))

(def-transform expression #'rest
  (slot-value (the linj.cons) '+rest-function+))

(def-transform expression #'identity
  (slot-value (the linj.cons) '+identity-function+))

(def-transform expression #'eq
  (slot-value (the linj.predicate-2) '+eq-function+))

(def-transform expression #'eql
  (slot-value (the linj.predicate-2) '+eql-function+))

(def-transform expression #'equals
  (slot-value (the linj.predicate-2) '+equals-function+))

;;Now, the generic case

(def-transform expression #'(?is ?name symbolp)
  (lambda ((arg (the object)))
    (?name arg)))

(def-transform expression #'((?is ?name symbolp) ?arg-type)
  (lambda ((arg (the ?arg-type)))
    (?name arg)))

(def-transform expression #'((?is ?name symbolp) ?arg-type0 ?arg-type1)
  (lambda ((arg0 (the ?arg-type0)) (arg1 (the ?arg-type1)))
    (?name arg0 arg1)))

;;another Common Lisp thing:

(def-macro-transform nil (with-slots ?slots ?instance . ?body)
  ;;adapt to avoid multiple evaluation
  (if (symbolp ?instance)
    `(let-macro-transform
      ,(mapcar #'(lambda (slot)
		   (multiple-value-bind (var-name slot-name)
		       (if (consp slot) (values-list slot) (values slot slot))
		     `(expression ,var-name `(slot-value ,',?instance ',',slot-name))))
	       ?slots)
      (progn ,@?body))
    ;;HACK: this should be improved to pick better names
    (let ((name (conc-symbol (first ?instance) '- (second ?instance))))
      `(let ((,name ,?instance))
	 (let-macro-transform 
	  ,(mapcar #'(lambda (slot)
		       (multiple-value-bind (var-name slot-name)
			   (if (consp slot) (values-list slot) (values slot slot))
			 `(expression ,var-name `(slot-value ,',?instance ',',slot-name))))
		   ?slots)
	  ?slots)
	 (progn ,@?body)))))

;;Random states

(def-transform expression (make-random-state t) (new 'java.util.random))
(def-transform expression (random-state-p ?x) (typep ?x 'java.util.random))

(def-linj-macro expression (random ?limit/expression)
  `(the ,(copy-type (get-type ?limit))
     (* (send (the java.lang.Math) random) ,?limit)))

(def-linj-macro expression (random ?limit/expression ?state/expression)
  (let ((type (get-type ?limit)))
  `(the ,(copy-type type)
     (* (send ,?state
	      ,(cond ((int-type-p type) 'next-int)
		     ((long-type-p type) 'next-long)
		     ((float-type-p type) 'next-float)
		     (t 'next-double)))
	,?limit))))

;; String operations

(def-linj-macro expression (position ?item/expression ?sequence/expression . ?options)
  (let ((type (get-type ?sequence)))
    (if (or (string-type-p type)
	    (equal-type-p type (parse 'java.util.Vector 'type-reference)))
      ;;Could also add LinkedList but Linj's cons is better
      (destructuring-bind (&key from-end test test-not start end key)
	  ?options
	(let ((oper (if from-end 'last-index-of 'index-of)))
	  (assert (not (or test test-not key (if from-end start end))))
	  `(,oper ,?sequence ,?item
		 ,@(if from-end
		     (and end (list (if (numberp end) (1- end) `(1- ,end))))
		     (and start (list start))))))
      (fail))))

(def-linj-macro expression (search ?item/expression ?sequence/expression . ?options)
  (if (string-type-p (get-type ?sequence))
    (destructuring-bind (&key from-end test test-not key start1 start2 end1 end2)
	?options
      (let ((oper (if from-end 'last-index-of 'index-of)))
	(assert (not (or test test-not key start1 end1 (if from-end start2 end2))))
	`(send ,?sequence ,oper ,?item
	       ,@(if from-end
		   (and end2 (list (if (numberp end2) (1- end2) `(1- ,end2))))
		   (and start2 (list start2))))))
    (fail)))

;; Iterate trough java.util.Vector

(def-macro-transform statement
  (do-dynamic-vector (?var (?is ?vector symbolp)) . ?body)
  (let ((i (gen-iter)))
    `(dotimes (,i (size ,?vector))
       (let ((,?var (element-at ,?vector ,i)))
	 ,@?body))))

(def-transform expression (stringp ?obj)
  (typep ?obj 'java.lang.string))

;;; Hashtables

(def-macro-transform expression (make-hash-table . ?options)
  (destructuring-bind (&key (test '#'equals) size (rehash-size nil rehash-size-p) rehash-threshold)
      ?options
    (declare (ignore rehash-size))
    (assert (equal test '#'equals))
    (assert (not rehash-size-p))
    (when rehash-threshold
      (assert size))
    `(new 'java.util.hashtable ,@(and size (list size)) ,@(and rehash-threshold (list rehash-threshold)))))

(def-transform expression (hash-table-p ?obj) (typep ?obj 'java.util.hashtable))

(def-transform expression (hash-table-count ?obj) (send (real-the java.util.hashtable ?obj) size))

;;We don't use 'send' to benefit from automatic wrappings
(def-transform expression (gethash ?key ?table) (get (real-the java.util.hashtable ?table) ?key))

(def-macro-transform expression (gethash ?key ?table ?default)
  (assert (atom ?key))
  (assert (atom ?table))
  `(if (contains-key (real-the java.util.hashtable ,?table) ,?key)
     (get (real-the java.util.hashtable ,?table) ,?key)
     ,?default))

(def-transform setf-expression (setf-gethash ?value ?key ?table . ?ignore) (put (real-the java.util.hashtable ?table) ?key ?value))

(def-transform expression (remhash ?key ?table) (remove (real-the java.util.hashtable ?table) ?key))

(def-transform statement (clrhash ?table) (send (real-the java.util.hashtable ?table) clear))

(def-transform expression (sxhash ?obj) (send ?obj hash-code))

;;; symbols

(def-transform expression (symbolp ?obj) (typep ?obj 'linj.symbol))

(def-transform expression (make-symbol ?name) (make-instance 'linj.symbol :name ?name))

(def-transform expression (intern ?name) (send (the linj.symbol) intern ?name))

(def-transform expression (find-symbol ?name) (send (the linj.symbol) find-symbol ?name))

(def-transform expression (gensym ?name) (send (the linj.symbol) gensym ?name))

;;Now, a special one to allow 'functions'  to be used in mapc

(def-transform expression
    (mapc #'(lambda ?params (?is ?first (lambda (e) (not (eq e :returns)))) . ?rest) . ?args)
  (mapc #'(lambda ?params :returns void ?first . ?rest) . ?args))

;; integers

(def-transform expression (numberp ?obj) (typep ?obj 'java.lang.number))

(def-linj-macro expression (integerp ?obj/expression)
  (if (bignum-type-p (get-type ?obj))
    (fail)
    (progn
      (assert (atom (ast-node-form ?obj)))
      `(or (typep ,?obj 'java.lang.integer)
	   (typep ,?obj 'java.lang.Long)
	   (and (typep ,?obj 'linj.bignum) (integerp (the linj.bignum ,?obj)))
	   (typep ,?obj 'java.math.big-integer)))))
  
(def-macro-transform expression (rationalp ?obj)
  (assert (atom ?obj))
  `(or (typep ,?obj 'java.lang.integer)
       (typep ,?obj 'java.lang.Long)
       (typep ,?obj 'linj.bignum)
       (typep ,?obj 'java.math.big-integer)))

(def-transform expression (parse-integer ?str)
  (send (the java.lang.Long) parse-long ?str))

(def-transform expression (parse-int ?str)
  (send (the java.lang.Integer) parse-int ?str))

(def-transform expression (parse-double ?str)
  (send (the java.lang.Double) parse-double ?str))

(def-transform expression (parse-float ?str)
  (send (the java.lang.Float) parse-float ?str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;Without the next two macros, the function

;; (defun foo ((x <int>) (y <int>))
;;   (and (> x y) (< y 3)))

;; ;;is translated into

;; public int foo (int x, int y) {
;;   return (x > y) && (y < 3);
;; }

(def-transform statement (and ?x) ?x)
(def-transform statement (and ?x . ?rest) (if (not ?x) nil (and . ?rest)))

;;using the macros, the translation becomes:

;; public int foo (int x, int y) {
;;   if (x <= y) {
;;     return false;
;;   } else {
;;     return y < 3;
;;   }
;; }

;;The 'problem' is more or less similar to the if-expression/if-statement
;;duality (obvious, as the translation depends on if)

(def-transform statement (or ?x) ?x)
(def-transform statement (or ?x . ?rest) (if ?x t (or . ?rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-linj-macro expression (string ?arg/expression)
  (let ((type (get-type ?arg)))
    (cond ((string-type-p type)
	   ?arg)
	  ((symbol-type-p type)
	   `(send ,?arg symbol-name))
	  ((char-type-p type)
	   (if (literal-p ?arg)
	     (string (literal-value ?arg))
	     `(send (the string) value-of ,?arg)))
	  (t
	   `(send (the string) value-of ,?arg)))))

(def-transform expression
    (princ-to-string ?obj)
  (format nil "~A" ?obj))

(def-transform expression
    (prin1-to-string ?obj)
  (format nil "~S" ?obj))

(defun build-string-expression (string start end)
  (let ((string `(real-the java.lang.string ,string)))
    (values (if (and start (numberp start) (> start 0))
	      `(send ,string substring 0 ,start)
	      nil)
	    (cond (end
		   `(send ,string substring ,(or start 0) ,end))
		  (start
		   `(send ,string substring ,start))
		  (t
		   string))
	    (if end
	      `(send ,string substring ,end)
	      nil))))



(defun string-case-expression (string case start end)
  (multiple-value-bind (start middle end)
      (build-string-expression string start end)
    (when (or (and start middle) (and middle end))
      (assert (symbolp string))) ;;to avoid multiple evaluation
    `(concat ,@(and start (list start))
	     (send ,middle ,case)
	     ,@(and end (list end)))))


(def-macro string-upcase (string &key start end)
  (string-case-expression string 'to-upper-case start end))

(def-macro string-downcase (string &key start end)
  (string-case-expression string 'to-lower-case start end))

(def-macro string-capitalize (string &key start end)
  `(key-send (the linj.util)
	     string-capitalize
	     ,string
	     ,@(when start (list :start start))
	     ,@(when end (list :end end))))

(def-macro-transform expression (string-trim ?character-bag ?string)
  (assert (equal ?character-bag ''(#\ )))
  `(send (real-the java.lang.string ,?string) trim))

(def-macro string= (string1 string2 &key start1 end1 start2 end2)
  `(equals ,(nth-value 1 (build-string-expression string1 start1 end1))
	   ,(nth-value 1 (build-string-expression string2 start2 end2))))

(def-macro string-equal (string1 string2 &key start1 end1 start2 end2)
  `(equals-ignore-case
    ,(nth-value 1 (build-string-expression string1 start1 end1))
    ,(nth-value 1 (build-string-expression string2 start2 end2))))

;; (def-macro string-replace (string1 string2 &key (start1 `0) (end1 `(length ,string1)) (start2 `0) (end2 `(length ,string2)))
;;   `(to-string
;;     (replace (new 'java.lang.StringBuffer ,string1)
;;      ,start1
;;      ,end1
;;      ,(nth-value 1 (build-string-expression string2 start2 `(min (+ ,start2 (- ,end1 ,start1)) ,end2))))))

;;   (multiple-value-bind (start middle end)
;;       (build-string-expression string1 start1 end1)
;;     (when (or (and start middle) (and middle end))
;;       (assert (atom string1))) ;;to avoid multiple evaluation
;;     `(concat 
;;       ,@(and start (list start))
;;       ,(nth-value 1 (build-string-expression string2 start2 `(max (length ,string1) ,(or end2 0))))
;;       ,@(and end (list end))))


(def-macro-transform expression (concatenate ?type . ?args)
  (assert (equal ?type ''string))
  `(concat ,@(mapcar #'(lambda (arg) `(string ,arg)) ?args)))



(def-transform statement
  (setf (getf ?place ?indicator . ?ignore) ?value)
  (setf ?place (put-plist ?place ?indicator ?value)))

(def-transform statement
  (remf ?place ?indicator)
  (setf ?place (rem-plist ?place ?indicator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-linj-macro expression (substitute ?newitem/expression ?olditem/expression ?sequence/expression)
  (if (string-type-p (get-type ?sequence))
    `(send ,?sequence replace ,?olditem ,?newitem)
    (error "substitute not implemented yet")))

(def-linj-macro expression (subseq ?sequence/expression ?start/expression &optional ?end/expression)
  (if (string-type-p (get-type ?sequence))
    `(substring ,?sequence ,?start ,@(and ?end (list ?end)))
    (error "subseq not implemented yet")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-macro qlet (expr binds &body body)
  `(if ,expr
     (let ,(mapcar #'(lambda (bind)
		       `(,(first bind)
			 (future ,(second bind))))
		   binds)
       ,@body)
     (let ,binds
       ,@body)))