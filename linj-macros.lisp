;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu Sep 18 04:14:25 2003
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


(def-macro-transform nil (ecase ?expr . ?clauses)
  `(case ,?expr
     ,@?clauses
     (t (error "~A fell through ECASE expression." ,?expr))))


(def-macro-transform nil (typecase (?is ?expr atom) . ?clauses)
  `(cond ,@(mapcar #'(lambda (clause)
		       (if (member (first clause) '(t otherwise))
			 clause
			 `((typep ,?expr ',(first clause))
			   ,@(rest clause))))
		   ?clauses)))

(def-macro-transform statement (typecase (?is ?expr consp) . ?clauses)
  (with-new-names (type-expr)
		  `(let ((,type-expr ,?expr))
		     (typecase ,type-expr . ,?clauses))))

(def-macro-transform nil (etypecase ?expr . ?clauses)
  `(typecase ,?expr
     ,@?clauses
     (t (error "~A fell through ETYPECASE expression." ,?expr))))

(def-macro assert (test &optional places datum-form &rest argument-forms)
  (assert (listp places))
  `(unless ,test
     (error 'runtime-exception
      (format nil
       ,(or datum-form (format nil "The assertion ~A failed." test))
       ,@argument-forms))))

(def-macro time (expr)
  `(let ((start-time (in (the system) (current-time-millis))))
    ,expr
    (let ((end-time (in (the system) (current-time-millis))))
      (format *trace-output*
	      "The evaluation took ~A milliseconds~%"
	      (- end-time start-time)))))
