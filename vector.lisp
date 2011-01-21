;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu Jul  1 18:05:03 2004
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


(def-transform expression (vector-length ?v)
  (slot-value ?v 'length))

(def-macro-transform statement (dovector (?var ?expr . ?result) . ?body)
  (with-new-names (i)
    `(dotimes (,i (vector-length ,?expr) . ,?result)
       (let ((,?var (aref ,?expr ,i)))
	 . ,?body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;We also implement the standard Common Lisp way of declaring arrays:

(def-transform array-type-reference vector (vector))

(def-transform array-type-reference (vector) (vector object))

(def-transform array-type-reference (vector ?type) (vector ?type *))

(def-transform array-type-reference (vector ?type ?dim) (array ?type (?dim)))

(def-macro-transform array-type-reference (array ?type ?dim)
  (cond ((numberp ?dim)
	 `(array ,?type ,(make-list ?dim :initial-element '*)))
	((consp ?dim)
	 nil);;`(array ,?type ,?dim))
	(t
	 (error "Unrecognized dimension in array type specifier ~A" ?dim))))

(def-macro-transform array-type-reference (array ?type (?ignore . ?dims))
  (if (endp ?dims)
    `(array-type ,(if (eq ?type '*) 'object ?type))
    `(array-type (array ,?type ,?dims))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-transform expression (svref ?array ?index) (aref ?array ?index))
(def-transform statement (setf (svref ?array ?index) ?value) (setf (aref ?array ?index) ?value))

(def-macro make-string (size &key (initial-element nil) (element-type ''char))
  `(new 'java.lang.string (make-array (list ,size)
			   :element-type ,element-type
			   ,@(when initial-element `
				   (:initial-element ,initial-element)))))