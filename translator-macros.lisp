;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Oct 13 09:40:26 2000
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro match ((pat dat) &body body)
  (compile-pattern pat dat 'parse `(progn ,@body)))

(defmacro try-match ((pat dat) &body body)
  (compile-pattern pat dat 'try-to-parse `(progn ,@body)))

(defmacro match-force-match ((pat dat) &body body)
  `(match (,(car pat) (car ,dat))
     (or (match (,(cdr pat) (cdr ,dat)) ,@body)
         (error "Wrong form for ~A: ~A" ',(car pat) ,dat))))


;;Now, the def-rule macro
(defmacro def-parse-rule (category label phase (&rest args) &body body)
  (assert category)
  `(add-parse-rule
    (make-parse-rule
     :category ',category
     :pattern ',label
     :function #'(lambda ,args
                   ,@body))
    ,phase))

;;;To iterate lists doing something in-between

(defmacro with-list-iteration ((var list separator) &body body)
  `(let ((first t))
     (dolist (,var ,list)
       (if first
           (setq first nil)
           ,separator)
       ,@body)))

(defmacro dolist-2 (((key val) list &optional result) &body body)
  (let ((l (gensym)))
    `(do ((,l ,list (cddr ,l)))
         ((null ,l) ,result)
       (let ((,key (car ,l))
             (,val (cadr ,l)))
         ,@body))))

(defparameter *debug-level* nil)

;;To unparse the language in java format
(defmacro def-unparse (type (arg) &body body)
  (let ((stream (gensym)))
    `(progn
       (defmethod print-object ((,arg ,type) ,stream)
         (format ,stream "~W" (ast-node-form ,arg))
         (case *debug-level*
           (0 (format ,stream ":~A" (type-of ,arg)))
           (1 (when (null (ast-node-parent ,arg))
                (format ,stream "[NO PARENT]")))))
       (defmethod unparse-object ((,arg ,type) ,stream)
         (let ((*standard-output* ,stream))
           ,@body)))))

(defmethod unparse-object ((obj t) stream)
  (princ obj stream))

(defmacro defconst (symbol value &optional doc)
  `(defparameter ,symbol ,value
     ,@(when doc (list doc))))
