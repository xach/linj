;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Sat Mar 27 17:39:24 2004
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

(in-package :linj)

;;This file requires the Linj readtable
(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* *linj-readtable*))

;;Macros for the construction of user-interfaces using Swing.  The
;;idea is to specify the interface in a HTML-like form, using macros
;;tr and td for lines and columns and other macros such as text for
;;widgets.

;;The fundamental macro wraps everything, creates a local variable and
;;returns it in the end.
(def-macro with-swing ((&key) &body body)
  `(let ((component/j-component null))
     ,@body
     component))

;;First approach using a db and expanding all
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *swing-constructors* (make-hash-table :test #'eq)))

(defmacro def-swing-constructor (class-superclass (&rest setters) &body body)
  (multiple-value-bind (class superclass)
      (if (consp class-superclass)
	(values-list class-superclass)
	(values class-superclass nil))
    (let ((all-slots (append setters (gethash superclass *swing-constructors* (list)))))
      (setf (gethash class *swing-constructors*) all-slots)
      `(def-macro ,class (&key (variable 'component) ,@(mapcar #'(lambda (setter) `(,setter nil ,(conc-symbol setter '-p))) all-slots))
	 (if (or ,@(mapcar #'(lambda (setter)
			       (conc-symbol setter '-p))
			   all-slots))
	   `(let ((,',class (new ',',class)))
	      ,@,@(mapcar #'(lambda (setter)
			      `(when ,(conc-symbol setter '-p)
				 `((send ,',class ,',(conc-symbol 'set- setter) ,,setter))))
			  all-slots)
	      ,@',body
	      (setf ,variable ,',class))
	   `(setf ,variable (new ',',class)))))))

(def-swing-constructor component
  (component-orientation
   cursor
   drop-target
   locale
   location
   name
   size))
  
(def-swing-constructor (container component)
  (layout
   bounds))

(def-swing-constructor (j-component container)
  (action-map
   alignment-x
   alignment-y
   autoscrolls
   background
   border
   debug-graphics-options
   double-buffered
   enabled
   font
   foreground
   input-verifier
   maximum-size
   minimum-size
   next-focusable-component
   opaque
   preferred-size
   request-focus-enabled
   tool-tip-text
   u-i
   verify-input-when-focus-target
   visible))

(def-swing-constructor (j-label j-component)
  (text
   icon
   horizontal-alignment
   vertical-alignment
   disabled-icon
   displayed-mnemonic
   horizontal-text-position
   vertical-text-position 
   icon-text-gap
   label-for))

(def-swing-constructor (j-text-component j-component)
  (caret
   caret-color
   caret-position
   disabled-text-color
   document
   drag-enabled
   editable
   focus-accelerator
   highlighter
   keymap
   margin
   navigation-filter
   selected-text-color
   selection-color
   selection-end
   selection-start
   text
   ui))

(def-swing-constructor (j-text-field j-text-component)
  (action
   action-command
   columns
   horizontal-alignment
   scroll-offset))

(def-swing-constructor (j-table j-component)
  (auto-create-columns-from-model
   auto-resize-mode
   cell-editor
   cell-selection-enabled
   column-model
   column-selection-allowed
   editing-column
   editing-row
   grid-color
   intercell-spacing
   model
   preferred-scrollable-viewport-size
   row-height
   row-margin
   row-selection-allowed
   selection-background
   selection-foreground
   selection-mode
   selection-model
   show-grid
   show-horizontal-lines
   show-vertical-lines
   table-header))

(def-swing-constructor (j-panel j-component)
  ())

;;Now, layout generation:

(def-macro table ((&rest args) (&key insets fill) &rest rows)
  `(let ((container (the j-panel null))
	 (constraints (new 'grid-bag-constraints)))
     (j-panel :variable container ,@args)
     (setf (slot-value constraints 'gridy) 0
	   (slot-value constraints 'gridx) 0)
     ,@(when insets
	 `((setf (slot-value constraints 'insets) ,insets)))
     ,@(when fill
	 `((setf (slot-value constraints 'fill) ,fill)))
     (set-layout container (new 'grid-bag-layout))
     ,@(mapcan #'(lambda (row)
		   (list row `(incf (slot-value constraints 'gridy))))
	       rows)
     (setf component container)))

(def-macro tabulators ((&key model tab-placement tab-layout-policy) &rest tabs)
  `(let ((container (new 'j-tabbed-pane))
	 (index 0))
     ,@(when model
	 `((set-model container ,model)))
     ,@(when tab-placement
	 `((set-tab-placement containe ,tab-placement)))
     ,@(when tab-layout-policy
	 `((set-tab-layout-policy container ,tab-layout-policy)))
     ,@(mapcan #'(lambda (tab)
		   (list tab `(incf index)))
	       tabs)
     (setf component container)))

(def-macro tabulator ((&key background disabled-icon displayed-mnemonic-index title tool-tip-text
			    (enabled t enabled-p) foreground icon mnemonic (selected nil))
		      comp)
  `(let ((component/j-component null))
     ,comp
     (let ((pan (new 'j-panel (new 'flow-layout (in (the flow-layout) +left+)))))
       (add pan component)
       ,(if title
	  `(add-tab container ,title pan)
	  `(add container pan))
       ,@(when background
	   `((set-background-at container index ,background)))
       ,@(when foreground
	   `((set-foreground-at container index ,foreground)))
       ,@(when title
	   `((set-title-at container index ,title)))
       ,@(when tool-tip-text
	   `((set-tool-tip-text-at container index ,tool-tip-text)))
       ,@(when disabled-icon
	   `((set-disabled-icon-at container index ,disabled-icon)))
       ,@(when displayed-mnemonic-index
	   `((set-displayed-mnemonic-index-at container index ,displayed-mnemonic-index)))
       ,@(when enabled-p
	   `((set-enabled-at container index ,enabled)))
       ,@(when icon
	   `((set-icon-at container index ,icon)))
       ,@(when mnemonic
	   `((set-mnemonic-at container index ,mnemonic)))
       ,@(when selected
	   `((set-selected-index container index))))))

(def-macro tr (&rest cols)
  `(let ((component/j-component null))
     ,@cols
     (setf (slot-value constraints 'gridx) 0)))

(def-macro td (form &key rowspan colspan insets fill)
  `(let (,@(when rowspan
	     `((old-rowspan (slot-value constraints 'gridheight))))
	 ,@(when colspan
	     `((old-colspan (slot-value constraints 'gridwidth))))
	 ,@(when insets
	     `((old-insets (slot-value constraints 'insets))))
	 ,@(when fill
	     `((old-fill (slot-value constraints 'fill)))))
     ,@(when rowspan
	 `((setf (slot-value constraints 'gridheight) ,rowspan)))
     ,@(when colspan
	 `((setf (slot-value constraints 'gridwidth) ,colspan)))
     ,@(when insets
	 `((setf (slot-value constraints 'insets) ,insets)))
     ,@(when fill
	 `((setf (slot-value constraints 'fill) ,fill)))
     ,form
     (set-constraints (the grid-bag-layout (get-layout container)) component constraints)
     (add container component)
     ,@(when rowspan
	 `((setf (slot-value constraints 'gridheight) old-rowspan)))
     ,@(when colspan
	 `((setf (slot-value constraints 'gridwidth) old-colspan)))
     ,@(when insets
	 `((setf (slot-value constraints 'insets) old-insets)))
     ,@(when fill
	 `((setf (slot-value constraints 'fill) old-fill)))
     (incf (slot-value constraints 'gridx) ,(or colspan '1))))


