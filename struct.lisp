;;; This file is part of Cedilla.
;;; Copyright (C) 2002-2006 by Juliusz Chroboczek.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(in-package "CEDILLA")

(defclass font ()
  ((name :type (or null string)
         :initarg :name
         :initform nil
         :accessor font-name)
   (x0 :type real :initarg :x0 :accessor font-x0 :initform 0)
   (y0 :type real :initarg :y0 :accessor font-y0 :initform 0)
   (x1 :type real :initarg :x1 :accessor font-x1 :initform 0)
   (y1 :type real :initarg :y1 :accessor font-y1 :initform 0)
   (instances :type list :initform '() :accessor font-instances)
   (transformed-fonts :type list :initform '()
                      :accessor font-transformed-fonts)
   (resources :type list :initform nil :initarg :resources
              :accessor font-resources)
   ))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t :identity t)
    (format stream "~A" (or (font-name font) "(anonymous)"))))

(defclass font-instance ()
  ((font :type font
         :initarg :font
         :accessor instance-font)))

(defmethod print-object ((instance font-instance) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A" (or (font-name (instance-font instance))
                            "(anonymous)"))))

(defclass named-glyph-font (font)
  ((glyphs :type hashtable
           :initform (make-hash-table :test 'equal)
           :accessor font-glyphs)
   (encoding :type (or null function)
             :initform nil
             :initarg :encoding
             :accessor font-encoding)
   ))

(defclass named-glyph-font-instance (font-instance)
  ((glyphs :type vector
           :initform (make-array 256 :initial-element nil :fill-pointer 32)
           :accessor instance-glyphs)))

(defclass fixed-encoding-font (font)
  ((glyphs :type simple-vector
           :initform (make-array 255 :initial-element nil)
           :accessor font-glyphs)
   (encoding :type function
             :initarg :encoding
             :accessor font-encoding)
   ))

(defclass fixed-encoding-font-instance (font-instance)
  ()
  )

(defclass space-font-instance (font-instance)
  ()
)

(defclass ps-font (font)
  ((ps-name :type (or null string) :initform nil :accessor ps-font-name))
  )

(defclass ps-font-instance (font-instance)
  ((ps-name :type (or null string) :initform nil 
            :accessor ps-font-instance-name))
  )

(defclass named-glyph-ps-font (named-glyph-font ps-font)
  ()
  )

(defun make-named-glyph-ps-font (&rest args)
  (apply #'make-instance 'named-glyph-ps-font args))

(defclass fixed-encoding-ps-font (fixed-encoding-font ps-font)
  ()
  )

(defun make-fixed-encoding-ps-font (&rest args)
  (apply #'make-instance 'fixed-encoding-ps-font args))

(defclass named-glyph-ps-font-instance (named-glyph-font-instance
                                        ps-font-instance)
  ()
  )

(defclass fixed-encoding-ps-font-instance (fixed-encoding-font-instance
                                           ps-font-instance)
  ()
  )

(defclass space-font (font)
  ((width :type real :reader font-width :initarg :width :initform 600)
   (glyph :type (or null glyph) :initform nil :accessor font-glyph)
   ))

(defun make-space-font (&rest args)
  (apply #'make-instance 'space-font args))

(defclass built-in-font (named-glyph-ps-font)
  ()
  )

(defclass transformed-font (ps-font)
  ((font :initarg :font :type ps-font
         :accessor transformed-font-font)
   (matrix :initarg :matrix :type simple-vector
           :accessor transformed-font-matrix)))

(defclass transformed-named-glyph-font (transformed-font
                                        named-glyph-ps-font)
  ()
)

(defclass transformed-fixed-encoding-font (transformed-font
                                           fixed-encoding-ps-font)
  ()
)


                                              

(eval-when (load compile eval)
(defun print-glyph (glyph stream depth)
  (declare (ignore depth))
  (print-unreadable-object (glyph stream :type t :identity t)
    (when (pre-glyph-name glyph)
      (format stream "~A" (pre-glyph-name glyph)))))
)

(defstruct (pre-glyph (:print-function print-glyph))
  (name 'nil :type (or null string)))

(defstruct (glyph (:include pre-glyph))
  (width 0 :type real)
  (x0 0 :type real)
  (y0 0 :type real)
  (x1 0 :type real)
  (y1 0 :type real)
  )

(defstruct (font-glyph (:include glyph))
  (font 'nil :type (or null font))
  (instance 'nil :type (or null font-instance))
  (index 'nil :type (or null integer))
  )

(defun glyph-font (glyph) (font-glyph-font glyph))
(defun (setf glyph-font) (f glyph) (setf (font-glyph-font glyph) f))
(defun glyph-instance (glyph) (font-glyph-instance glyph))
(defun (setf glyph-instance) (i glyph) (setf (font-glyph-instance glyph) i))
(defun glyph-index (glyph) (font-glyph-index glyph))
(defun (setf glyph-index) (i glyph) (setf (font-glyph-index glyph) i))

(defstruct (composite-component (:conc-name "COMPONENT-"))
  (glyph nil :type (or null glyph))
  (dx 0 :type real)
  (dy 0 :type real))

(defstruct (composite-glyph (:include glyph) 
                            (:constructor %make-composite-glyph))
  (components '() :type list)
  (base '() :type (or null glyph))
  (base-dx 0 :type (or null real))
  (base-dy 0 :type (or null real))
)

(defun glyph-base (glyph)
  (if (composite-glyph-p glyph) (composite-glyph-base glyph) glyph))

(defun glyph-base-dx (glyph)
  (if (composite-glyph-p glyph) (composite-glyph-base-dx glyph) 0))

(defun glyph-base-dy (glyph)
  (if (composite-glyph-p glyph) (composite-glyph-base-dy glyph) 0))

(defun make-composite-glyph (&key name width components base base-dx base-dy)
  (flet ((c-x0 (c) (+ (component-dx c) (glyph-x0 (component-glyph c))))
         (c-y0 (c) (+ (component-dy c) (glyph-y0 (component-glyph c))))
         (c-x1 (c) (+ (component-dx c) (glyph-x1 (component-glyph c))))
         (c-y1 (c) (+ (component-dy c) (glyph-y1 (component-glyph c)))))
    (let* ((x0 (apply #'min (mapcar #'c-x0 components)))
           (y0 (apply #'min (mapcar #'c-y0 components)))
           (x1 (apply #'max (mapcar #'c-x1 components)))
           (y1 (apply #'max (mapcar #'c-y1 components))))
      (let* ((new-glyph 
              (%make-composite-glyph
               :name name
               :x0 x0 :y0 y0 :x1 x1 :y1 y1
               :components components))
             (base-glyph
              (cond
                ((eql base :self) new-glyph)
                ((null base) (glyph-base (component-glyph (car components))))
                (t base)))
             (base-glyph-dx
              (cond
                ((eql base :self) 0)
                (base-dx base-dx)
                (t (+ (component-dx (car components)) 
                      (glyph-base-dx base-glyph)))))
             (base-glyph-dy
              (cond
                ((eql base :self) 0)
                (base-dy base-dy)
                (t (+ (component-dy (car components)) 
                      (glyph-base-dy base-glyph)))))
             (width (or width (+ (glyph-width base-glyph) base-glyph-dx))))
        (setf (glyph-width new-glyph) width
              (composite-glyph-base new-glyph) base-glyph
              (composite-glyph-base-dx new-glyph) base-glyph-dx
              (composite-glyph-base-dy new-glyph) base-glyph-dy)
        new-glyph))))

(defstruct (space-glyph (:include font-glyph))
  )

(defstruct (built-in-glyph (:include font-glyph))
  (charstring nil :type (or null string)))

(defstruct (transformed-glyph (:include font-glyph) 
                              (:constructor %make-transformed-glyph))
  (glyph nil :type (or null glyph)))

(defun transformed-glyph-matrix (glyph)
  (transformed-font-matrix (glyph-font glyph)))

(defstruct (magic-glyph (:include glyph) (:constructor nil))
  (glyph nil :type (or null glyph)))

(defstruct (dotless-glyph (:include magic-glyph) 
                          (:constructor %make-dotless-glyph))
  )

(defstruct (enclosed-glyph (:include magic-glyph)
                           (:constructor %make-enclosed-glyph))
  (dx 0 :type real)
  (op nil :type (or null string)))

(defstruct (barred-glyph (:include magic-glyph)
                         (:constructor %make-barred-glyph))
  bx0 by0 bx1 by1)

(defstruct (generic-transformed-glyph (:include magic-glyph)
                                      (:constructor 
                                       %make-generic-transformed-glyph))
  (matrix nil :type (or null simple-vector)))


(defstruct (unresolved-composite-glyph (:include pre-glyph))
  (components '() :type list))

(defstruct paper-size
  x0 y0 x1 y1 left-margin top-margin right-margin bot-margin
)

(eval-when (load compile eval)
(defun print-ps-resource (resource stream depth)
  (declare (ignore depth))
  (print-unreadable-object (resource stream :type t :identity t)
    (format stream "~A " (ps-resource-type resource))
    (format stream "~A" (ps-resource-name resource))))
)

(defstruct (ps-resource (:print-function print-ps-resource))
  type
  name
  version
  release)

(defstruct (required-ps-resource (:include ps-resource))
  )

(defstruct (provided-ps-resource (:include ps-resource))
  )

(defstruct (file-ps-resource (:include provided-ps-resource))
  filename)

(defun ps-resource-filename (resource) (file-ps-resource-filename resource))

(defstruct (pfb-ps-resource (:include file-ps-resource)))

(defstruct (string-ps-resource (:include provided-ps-resource))
  value)

(defun ps-resource-value (resource) (string-ps-resource-value resource))

(defstruct (built-in-font-resource (:include provided-ps-resource))
  font)

(defun ps-resource-font (resource)
  (built-in-font-resource-font resource))
