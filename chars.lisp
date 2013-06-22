;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Oct  3 08:44:05 2003
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


;;For Linj characters
(defmacro def-char-predicate (name gen-name)
  `(def-transform binary-operator-expression (,name ?x ?y)
     (,gen-name (real-the char ?x) (real-the char ?y))))

(def-char-predicate char= =)
(def-char-predicate char< <)
(def-char-predicate char> >)
(def-char-predicate char<= <=)
(def-char-predicate char>= >=)
(def-char-predicate char/= /=)

(def-ordered-predicate char=)
(def-ordered-predicate char<)
(def-ordered-predicate char>)
(def-ordered-predicate char<=)
(def-ordered-predicate char>=)
(def-unordered-predicate char/=)



;;Note: we restrict ?c to be an atom in the hope of avoiding duplicate evaluation
;;However, this should be done with an assert!
(def-transform expression (digit-char-p (?is ?c atom))
  (and (char>= ?c #\0) (char<= ?c #\9)))

(def-transform expression (alpha-char-p (?is ?c atom))
  (or (and (char>= ?c #\a) (char<= ?c #\z))
      (and (char>= ?c #\A) (char<= ?c #\Z))))


(def-transform expression (char-code ?c) ?c)

(defmacro def-char-no-case-test (name oper)
  `(def-transform expression (,name ?c1 ?c2) (,oper (char-upcase ?c1) (char-upcase ?c2))))

(def-char-no-case-test char-equal char=)
(def-char-no-case-test char-not-equal char/=)
(def-char-no-case-test char-lessp char<)
(def-char-no-case-test char-greaterp char>)
(def-char-no-case-test char-not-greaterp char<=)
(def-char-no-case-test char-not-lessp char>=)

(def-ordered-predicate char-equal)
(def-ordered-predicate char-lessp)
(def-ordered-predicate char-greaterp)
(def-ordered-predicate char-not-greaterp)
(def-ordered-predicate char-not-lessp)
(def-unordered-predicate char-not-equal)

(def-transform expression (char ?str ?i) (aref ?str ?i))
(def-transform expression (schar ?str ?i) (aref ?str ?i))
(def-transform setf-expression (setf-char ?c ?str ?i) (setf (aref ?str ?i) ?c))
(def-transform setf-expression (setf-schar ?c ?str ?i) (setf (aref ?str ?i) ?c))

(def-transform expression (digit-char-p ?c) (send (the java.lang.character) is-digit (real-the char ?c)))
(def-transform expression (alpha-char-p ?c) (send (the java.lang.character) is-letter (real-the char ?c)))
(def-transform expression (alphanumericp ?c) (send (the java.lang.character) is-letter-or-digit (real-the char ?c)))
(def-transform expression (upper-case-p ?c) (send (the java.lang.character) is-upper-case (real-the char ?c)))
(def-transform expression (lower-case-p ?c) (send (the java.lang.character) is-lower-case (real-the char ?c)))

(def-transform expression (char-upcase ?c) (send (the java.lang.character) to-upper-case (real-the char ?c)))
(def-transform expression (char-downcase ?c) (send (the java.lang.character) to-lower-case (real-the char ?c)))

(def-transform expression (char-digit ?c) (send (the java.lang.character) digit (real-the char ?c) 10))
(def-transform expression (digit-char ?d) (send (the java.lang.character) for-digit (real-the char ?d) 10))
