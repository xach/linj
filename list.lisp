;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Jan  5 23:09:07 2001
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


;;This file extends Linj to support lists directly in the code.  Note that
;;the result can't be "assembled" by the java compiler without the file 'cons.linj'

;;We use the quote to distinguish between nil and ()
(def-transform expression '()
  (slot-value (the cons) '+empty-list+))

(def-transform expression '(?arg)
  (key-send (the linj.cons) list '?arg))

(def-transform expression '(?arg0 ?arg1)
  (key-send (the linj.cons) list '?arg0 '?arg1))

(def-transform expression '(?arg0 ?arg1 ?arg2)
  (key-send (the linj.cons) list '?arg0 '?arg1 '?arg2))

(def-transform expression '(?arg0 ?arg1 ?arg2 ?arg3)
  (key-send (the linj.cons) list '?arg0 '?arg1 '?arg2 '?arg3))

(def-transform expression '(?arg0 ?arg1 ?arg2 ?arg3 ?arg4)
  (key-send (the linj.cons) list '?arg0 '?arg1 '?arg2 '?arg3 '?arg4))

(def-transform expression '(?first . ?rest)
  (cons '?first '?rest))

;; (def-transform expression (list) '())

;; (def-transform expression (list ?first . ?rest) (cons ?first (list . ?rest)))

(def-macro-transform expression (list . ?args)
  (if (<= (length ?args) 5)
    `(key-send (the linj.cons) list . ,?args)
    `(cons ,(first ?args) (list . ,(rest ?args)))))

(def-transform expression (null ?obj) (not ?obj))

(def-transform expression (not (null ?obj)) ?obj)

(def-transform expression (cons ?car ?cdr) (make-instance 'linj.cons :car ?car :cdr ?cdr))

(def-transform expression (acons ?car ?cdr ?rest) (cons (cons ?car ?cdr) ?rest))

(def-transform expression (consp ?obj) (and (typep ?obj 'cons) (not (eq ?obj '()))))

(def-transform expression (atom ?obj) (or (eq ?obj '()) (not (typep ?obj 'cons))))

(def-transform expression (listp ?obj) (typep ?obj 'cons))

(def-transform expression (list* ?arg) ?arg)

(def-transform expression (list* ?first . ?rest) (cons ?first (list* . ?rest)))

;;Let's also define a macro to expand other types of quoted expressions:

;; (def-transform expression '(?arg) (list '?arg))
;; (def-transform expression '(?first . ?rest) (cons '?first '?rest))

(def-macro-transform statement (push ?obj (gethash ?key ?table ?default))
  (cond ((and (atom ?key)
	      (atom ?table))
	 `(if (contains-key (real-the java.util.hashtable ,?table) ,?key)
	   (setf (gethash ,?key ,?table)
	    (cons ,?obj (gethash ,?key ,?table)))
	   (setf (gethash ,?key ,?table)
	    (cons ,?obj ,?default))))
	((atom ?key)
	 `(let ((table ,?table))
	   (push ,?obj (gethash ,?key table ,?default))))
	(t
	 `(let ((key ,?key))
	   (push ,?obj (gethash key ,?table ,?default))))))

;;We also need this to be parsable as expression (bc the Java API also implements such method)
(add-expression-word 'push)
(def-linj-macro statement (push ?obj/expression ?place/expression)
  (if (or (cons-type-p (get-type ?place)) (object-type-p (get-type ?place)))
    `(setf ,(ast-node-form ?place) (cons ,?obj ,?place))
    (fail)))

;;We also need this to be parsable as expression (bc the Java API also implements such method)
(add-expression-word 'pop)
(def-linj-macro statement (pop ?place/expression)
  (if (cons-type-p (get-type ?place))
      (if (reference-p ?place)
	  `(prog1
	    (first ,?place)
	    (setf ,(ast-node-form ?place) (rest ,?place)))
	  (linj-error "Can't deal with such pop expression ~A" ?place))
    (fail)))

(def-macro-transform statement (doplist (?props-vals ?list) . ?body)
  `(do ((plist ,?list (rest (rest plist))))
       ((endp plist)) ;;HACK: Should use this scheme to other forms (dolist, doassoc, etc)
     (let ,(mapcar #'(lambda (var selector)
		       (if (consp var) ;;With type
			 `(,(first var) (the ,(second var) (,selector plist)))
			 `(,var (,selector plist))))
	    ?props-vals
	    '(first second third fourth fifth))
       ,@?body)))

(def-transform statement (doassoc ((?prop ?val) ?list . ?result-forms) . ?body)
  (do ((the-assoc ?list (rest the-assoc)))
      ((endp the-assoc) . ?result-forms)
    (let ((pair (the cons (first the-assoc))))
      (let ((?prop (car pair))
	    (?val (cdr pair)))
	. ?body))))

(def-macro-transform for-statement (dolist2 (?var1 ?expr1 ?var2 ?expr2 . ?result-forms) . ?body)
  (with-new-names (list1 list2)
    `(do ((,list1 ,?expr1 (rest ,list1))
	  (,list2 ,?expr2 (rest ,list2)))
	 ((endp ,list1) . ,?result-forms)
       (let ((,?var1 (first ,list1))
	     (,?var2 (first ,list2))) . ,?body))))

(def-real-the-form setf-car t linj.cons)
(def-real-the-form setf-cdr t linj.cons)
(def-real-the-form setf-first t linj.cons)
(def-real-the-form setf-rest t linj.cons)
(def-real-the-form car linj.cons)
(def-real-the-form cdr linj.cons)
(def-real-the-form caar linj.cons)
(def-real-the-form cadr linj.cons)
(def-real-the-form cdar linj.cons)
(def-real-the-form cddr linj.cons)
(def-real-the-form first linj.cons)
(def-real-the-form second linj.cons)
(def-real-the-form rest linj.cons)
(def-real-the-form endp linj.cons)
(def-real-the-form copy-list linj.cons)
(def-real-the-form mapcar linj.function linj.cons)
(def-real-the-form mapc t linj.cons)
(def-real-the-form mapcar linj.function-2 linj.cons linj.cons)
(def-real-the-form assoc t linj.cons)


(def-transform expression (rplaca ?cons ?car) (setf (car (real-the linj.cons ?cons)) ?car))
(def-transform expression (rplacd ?cons ?cdr) (setf (cdr (real-the linj.cons ?cons)) ?cdr))

(def-transform expression (caddr ?cons) (car (cddr ?cons)))
(def-transform expression (cdddr ?cons) (cdr (cddr ?cons)))

(def-transform expression (cadddr ?cons) (cadr (cddr ?cons)))
(def-transform expression (cddddr ?cons) (cddr (cddr ?cons)))

;;The first five are implemented directly
(def-transform expression (sixth ?list) (nth 5 (real-the linj.cons ?list)))
(def-transform expression (seventh ?list) (nth 6 (real-the linj.cons ?list)))
(def-transform expression (eighth ?list) (nth 7 (real-the linj.cons ?list)))
(def-transform expression (ninth ?list) (nth 8 (real-the linj.cons ?list)))
(def-transform expression (tenth ?list) (nth 9 (real-the linj.cons ?list)))

(def-transform expression (setf-second ?value ?list) (setf (first (nthcdr 1 ?list)) ?value))
(def-transform expression (setf-third ?value ?list) (setf (first (nthcdr 2 ?list)) ?value))
(def-transform expression (setf-fourth ?value ?list) (setf (first (nthcdr 3 ?list)) ?value))
(def-transform expression (setf-fifth ?value ?list) (setf (first (nthcdr 4 ?list)) ?value))
(def-transform expression (setf-sixth ?value ?list) (setf (first (nthcdr 5 ?list)) ?value))
(def-transform expression (setf-seventh ?value ?list) (setf (first (nthcdr 6 ?list)) ?value))
(def-transform expression (setf-eighth ?value ?list) (setf (first (nthcdr 7 ?list)) ?value))
(def-transform expression (setf-ninth ?value ?list) (setf (first (nthcdr 8 ?list)) ?value))
(def-transform expression (setf-tenth ?value ?list) (setf (first (nthcdr 9 ?list)) ?value))

(defmacro generalize-to-multi-arguments (name)
  `(def-macro-transform expression (,name ?arg1 ?arg2 ?arg3 . ?args)
     (let ((args (list* ?arg1 ?arg2 ?arg3 ?args)))
       (reduce #'(lambda (arg1 arg2)
		   `(,',name ,arg1 ,arg2))
	       (rest args)
	       :initial-value (first args)
	       :from-end t))))

(generalize-to-multi-arguments append)
(generalize-to-multi-arguments nconc)

(def-transform expression (iota n) (iota n '()))