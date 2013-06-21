;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Oct 13 09:24:50 2000
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

;;;For the pattern compiler

(defun pat-var? (elem)
  (and (symbolp elem)
       (char= (char (symbol-name elem) 0) #\?)))

(defun pat-var-name-categorie (sym)
  (let ((name (symbol-name sym)))
    (let ((pos (position #\/ name)))
      (if (not (null pos))
          (values (intern (subseq name 0 pos))
                  (intern (subseq name (1+ pos))))
                                        ;(error "Can't find category in ~a" sym)
          (values sym
                  nil)))))

(defun var-name (name)
  (intern (subseq (symbol-name name) 1)))

(defun collect-pattern-vars (tree)
  (cond ((atom tree)
         (if (and (pat-var? tree)
                  (not (member tree '(?if ?is))))
             (list tree)
             ()))
        (t
         (nconc (collect-pattern-vars (car tree))
                (collect-pattern-vars (cdr tree))))))

;;Removes all ?is ?if etc
(defun simplify-pattern-tree (tree)
  (cond ((atom tree)
         tree)
        ((eq (first tree) '?is)
         (second tree))
        ((and (rest tree)
              (consp (rest tree))
              (consp (second tree))
              (eq (first (second tree)) '?if))
         (cons (simplify-pattern-tree (first tree))
               (simplify-pattern-tree (rest (rest tree)))))
        (t
         (cons (simplify-pattern-tree (car tree))
               (simplify-pattern-tree (cdr tree))))))

(defun collect-non-pattern-vars (tree)
  (cond ((atom tree)
         (if (or (null tree) (pat-var? tree))
             ()
             (list tree)))
        ((member (car tree) '(?if ?is))
         ())
        (t
         (nconc (collect-non-pattern-vars (car tree))
                (collect-non-pattern-vars (cdr tree))))))

;;This one deals with pattern variables of the form ?x/expression
;;and it doesn't have to be concerned with repeated pattern variables
;; (defun compile-pattern (pat dat func body)
;;   (cond ((null pat)
;;          `(and (null ,dat) ,body))
;;         ((atom pat)
;;          (if (pat-var? pat)
;;            (if (eq pat '?ignore) ;;`A la Lisp Machine
;;              `(progn ,dat ,body)
;;           (multiple-value-bind (name categorie)
;;               (pat-var-name-categorie pat)
;;             (if categorie
;;               `(let ((,name (,func ,dat ',categorie)))
;;                  (and ,name ,body))
;;               `(let ((,name ,dat))
;;                  ,body))))
;;            `(and (,(if (symbolp pat) 'eq 'eql) ,dat ',pat)
;;                  ,body)))
;;      ((eq (car pat) '?is)
;;       (let ((pat-var (second pat))
;;             (predicate (third pat)))
;;         (multiple-value-bind (name categorie)
;;             (pat-var-name-categorie pat-var)
;;           (if categorie
;;             (let ((let-dat (gensym)))
;;               `(let ((,let-dat ,dat))
;;                  (and (,predicate ,let-dat)
;;                       (let ((,name (,func ,let-dat ',categorie)))
;;                         (and ,name ,body)))))
;;             `(let ((,name ,dat))
;;                (and (,predicate ,name)
;;                     ,body))))))
;;      ((eq (car pat) '&optional)
;;       (let ((pat-var (second pat))
;;             (predicate (third pat)))
;;         (multiple-value-bind (name categorie)
;;             (pat-var-name-categorie pat-var)
;;           (if categorie
;;             (let ((let-dat (gensym)))
;;               `(let ((,let-dat ,dat))
;;                  (and (,predicate ,let-dat)
;;                       (let ((,name (,func ,let-dat ',categorie)))
;;                         (and ,name ,body)))))
;;             `(let ((,name ,dat))
;;                (and (,predicate ,name)
;;                     ,body))))))
;;      ((and (consp (car pat)) (eq (caar pat) '?if))
;;       `(and ,(second (car pat))
;;             ,(let ((let-dat (gensym)))
;;                `(let ((,let-dat ,dat))
;;                   ,(compile-pattern (cdr pat)
;;                                     let-dat
;;                                     func
;;                                     body)))))
;;         (t
;;          (let ((let-dat (gensym)))
;;            `(let ((,let-dat ,dat))
;;               (and (consp ,let-dat)
;;                    ,(compile-pattern (car pat)
;;                                      `(car ,let-dat)
;;                                   func
;;                                      (compile-pattern (cdr pat)
;;                                                       `(cdr ,let-dat)
;;                                                    func
;;                                                       body))))))))

;;This one deals with pattern variables of the form ?x/expression and also &optional and &rest
;;and it doesn't have to be concerned with repeated pattern variables
(defun compile-pattern (pat dat func body &optional (processing :required))
  (cond ((null pat)
         (case processing
           (:required
            `(and (null ,dat) ,body))
           (:optional
            body)))
        ((atom pat)
         (if (pat-var? pat)
             (if (eq pat '?ignore) ;;`A la Lisp Machine
                 `(progn ,dat ,body)
                 (multiple-value-bind (name categorie)
                     (pat-var-name-categorie pat)
                   (case processing
                     ((:required :optional)
                      (if categorie
                          `(let ((,name (,func ,dat ',categorie)))
                             (and ,name ,body))
                          `(let ((,name ,dat))
                             ,body)))
                     ;;                  (:optional
                     ;;                   (if categorie
                     ;;                     `(let ((,name (and ,dat (,func ,dat ',categorie))))
                     ;;                        ,body)
                     ;;                     `(let ((,name ,dat))
                     ;;                        ,body)))
                     )))
             `(and (,(if (symbolp pat) 'eq 'eql) ,dat ',pat)
                   ,body)))
        ((eq (car pat) '?is)
         (let ((pat-var (second pat))
               (predicate (third pat)))
           (multiple-value-bind (name categorie)
               (pat-var-name-categorie pat-var)
             (if categorie
                 (let ((let-dat (gensym)))
                   `(let ((,let-dat ,dat))
                      (and (,predicate ,let-dat)
                           (let ((,name (,func ,let-dat ',categorie)))
                             (and ,name ,body)))))
                 `(let ((,name ,dat))
                    (and (,predicate ,name)
                         ,body))))))
        ((eq (car pat) '&optional)
         `(if ,dat
              ,(compile-pattern (cdr pat) dat func body :optional)
              ,(let ((remaining (collect-pattern-vars (cdr pat))))
                 `(let ,(mapcar #'pat-var-name-categorie remaining)
                    ,body))))
        ((and (member (car pat) '(&rest &body)))
         `(and (listp ,dat)
               ,(compile-pattern (cadr pat) dat func body :required)))
        ((and (consp (car pat)) (eq (caar pat) '?if))
         `(and ,(second (car pat))
               ,(let ((let-dat (gensym)))
                  `(let ((,let-dat ,dat))
                     ,(compile-pattern (cdr pat) let-dat func body processing)))))
        (t
         (let ((let-dat (gensym)))
           `(let ((,let-dat ,dat))
              ,(let ((expr (compile-pattern (car pat)
                                            `(car ,let-dat)
                                            func
                                            (compile-pattern (cdr pat)
                                                             `(cdr ,let-dat)
                                                             func
                                                             body
                                                             processing)
                                            processing)))
                 (case processing
                   (:required
                    `(and (consp ,let-dat)
                          ,expr))
                   (:optional
                    expr))))))))

;;This one deals with pattern variables of the form ?x
;;and it needs to be concerned with repeated pattern variables
(defun compile-pattern-special (pat dat body)
  (labels ((equal-pat-var? (v1 v2)
             (eq v1 v2))
           (compile-pattern (pat dat body vars)
             (cond ((null pat)
                    `(and (null ,dat) ,body))
                   ((atom pat)
                    (if (pat-var? pat)
                        (if (equal-pat-var? pat '?ignore) ;;`A la Lisp Machine
                            `(progn ,dat ,body)
                            (if (member pat vars :test #'equal-pat-var?)
                                `(and (equal ,pat ,dat)
                                      ,body)
                                `(let ((,pat ,dat)) ,body)))
                        `(and (,(if (symbolp pat) 'eq 'eql) ,dat ',pat)
                              ,body)))
                   ((eq (car pat) '?is)
                    (let ((pat-var (second pat))
                          (predicate (third pat)))
                      `(let ((,pat-var ,dat))
                         (and (,predicate ,pat-var)
                              ,body))))
                   ((and (consp (car pat)) (eq (caar pat) '?if))
                    `(and ,(second (car pat))
                          ,(let ((let-dat (gensym)))
                             `(let ((,let-dat ,dat))
                                ,(compile-pattern (cdr pat)
                                                  let-dat
                                                  body
                                                  vars)))))
                   (t
                    (let ((let-dat (gensym))
                          (car-vars (collect-pattern-vars (car pat))))
                      `(let ((,let-dat ,dat))
                         (and (consp ,let-dat)
                              ,(compile-pattern (car pat)
                                                `(car ,let-dat)
                                                (compile-pattern (cdr pat)
                                                                 `(cdr ,let-dat)
                                                                 body
                                                                 (union car-vars vars :test #'equal-pat-var?))
                                                vars))))))))
    (compile-pattern pat dat body (list))))


(defmacro rpush (elem place)
  `(setf ,place (append ,place (list ,elem))))


(defun iota (n)
  (labels ((iter (n r)
             (if (zerop n)
                 (cons 0 r)
                 (iter (- n 1) (cons n r)))))
    (iter n (list))))


(defun conc-symbol (&rest syms)
  (let ((string (apply #'concatenate 'string (mapcar #'prin1-to-string syms))))
    (let ((*package* (if (keywordp (first syms))
                         (symbol-package (first syms))
                         *package*)))
      (read-from-string string))))

(defun break-list-if (pred list)
  (labels ((iter (l accum)
             (if (or (null l) (funcall pred (first l)))
                 (values (nreverse accum) l)
                 (iter (rest l) (cons (first l) accum)))))
    (iter list ())))
