;;; This file is part of Cedilla.
;;; Copyright (C) 2002 by Juliusz Chroboczek.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(in-package "CEDILLA")

(defun identity-p (matrix)
  (equal matrix #(1 0 0 1 0 0)))

(defun concat (m1 m2)
  (cond
    ((identity-p m1) m2)
    ((identity-p m2) m1)
    (t
     (vector
      (+ (* (aref m1 0) (aref m2 0)) (* (aref m1 1) (aref m2 2)))
      (+ (* (aref m1 0) (aref m2 1)) (* (aref m1 1) (aref m2 3)))
      (+ (* (aref m1 2) (aref m2 0)) (* (aref m1 3) (aref m2 2)))
      (+ (* (aref m1 2) (aref m2 1)) (* (aref m1 3) (aref m2 3)))
      (+ (* (aref m1 4) (aref m2 0)) (* (aref m1 5) (aref m2 2))
         (aref m2 4))
      (+ (* (aref m1 4) (aref m2 1)) (* (aref m1 5) (aref m2 3))
         (aref m2 5))))))

(defun transform (dx dy matrix)
  (values
   (+ (* dx (aref matrix 0)) (* dy (aref matrix 2)) (aref matrix 4))
   (+ (* dx (aref matrix 1)) (* dy (aref matrix 3)) (aref matrix 5))))

(defun dtransform (dx dy matrix)
  (values
   (+ (* dx (aref matrix 0)) (* dy (aref matrix 2)))
   (+ (* dx (aref matrix 1)) (* dy (aref matrix 3)))))

(defun transform-bbox (x0 y0 x1 y1 matrix)
  (multiple-value-bind (x0* y0*) 
      (transform x0 y0 matrix)
    (multiple-value-bind (x1* y1*) 
        (transform x1 y1 matrix)
      (when (> x0* x1*) (psetq x0* x1* x1* x0*))
      (when (> y0* y1*) (psetq y0* y1* y1* y0*))
      (values x0* y0* x1* y1*))))

(defun make-transformed-font (font matrix)
  (let ((font (if (typep font 'transformed-font)
                  (transformed-font-font font)
                  font))
        (matrix (if (typep font 'transformed-font)
                    (concat matrix (transformed-font-matrix font))
                    matrix)))
    (or (car (member matrix (font-transformed-fonts font)
                     :test #'(lambda (m f)
                               (equal m (transformed-font-matrix f)))))
        (let ((class
               (etypecase font
                 (named-glyph-ps-font 'transformed-named-glyph-font)
                 (fixed-encoding-ps-font 'transformed-fixed-encoding-font))))
          (multiple-value-bind (x0 y0 x1 y1)
              (transform-bbox (font-x0 font) (font-y0 font)
                              (font-x1 font) (font-y1 font) matrix)
            (let ((font*
                   (make-instance class
                                  :name (font-name font)
                                  :font font
                                  :x0 x0 :y0 y0 :x1 x1 :y1 y1
                                  :matrix matrix)))
              (push font* (font-transformed-fonts font))
              font*))))))

(defmethod make-transformed-glyph ((glyph font-glyph) matrix)
  (if (not (typep (glyph-font glyph) 'ps-font))
      (make-generic-transformed-glyph glyph matrix)
      (let ((font (make-transformed-font (glyph-font glyph) matrix))
            (glyph* (if (transformed-glyph-p glyph)
                        (transformed-glyph-glyph glyph)
                        glyph)))
        (multiple-value-bind (x0 y0 x1 y1)
            (transform-bbox (glyph-x0 glyph) (glyph-y0 glyph) 
                            (glyph-x1 glyph) (glyph-y1 glyph) matrix)
          (%make-transformed-glyph :font font :name (glyph-name glyph)
                                   :width (transform (glyph-width glyph) 0
                                                     matrix)
                                   :glyph glyph*
                                   :x0 x0 :y0 y0 :x1 x1 :y1 y1)))))

(defmethod make-transformed-glyph ((glyph composite-glyph) matrix)
  (let ((components
         (mapcar
          #'(lambda (c)
              (multiple-value-bind (dx dy)
                  (transform (component-dx c) (component-dy c) matrix)
                (make-composite-component
                 :glyph (make-transformed-glyph (component-glyph c) matrix)
                 :dx dx :dy dy)))
          (composite-glyph-components glyph))))
    (make-composite-glyph :width (transform (glyph-width glyph) 0 matrix)
                          :components components)))

(defun make-generic-transformed-glyph (glyph matrix)
  (let ((glyph* (if (generic-transformed-glyph-p glyph)
                    (magic-glyph-glyph glyph)
                    glyph))
        (matrix* (if (generic-transformed-glyph-p glyph)
                     (concat matrix (generic-transformed-glyph-matrix glyph))
                     matrix)))
    (multiple-value-bind (x0 y0 x1 y1)
        (transform-bbox (glyph-x0 glyph*) (glyph-y0 glyph*) 
                        (glyph-x1 glyph*) (glyph-y1 glyph*) matrix*)
      (%make-generic-transformed-glyph 
       :width (transform (glyph-width glyph*) 0 matrix*)
       :x0 x0 :y0 y0 :x1 x1 :y1 y1
       :matrix matrix* :glyph glyph*))))

(defmethod make-transformed-glyph ((glyph glyph) matrix)
  (make-generic-transformed-glyph glyph matrix))

(defmethod setup-font (out (font transformed-font) size name)
  (declare (ignore size))
  (setf (ps-font-name font) name)
  (let ((font* (transformed-font-font font))
        (matrix (transformed-font-matrix font)))
    (format out "/~A /~A findfont [~A ~A ~A ~A ~A ~A] makefont def~%"
            name (font-name font*)
            (* *size* (aref matrix 0))
            (* *size* (aref matrix 1))
            (* *size* (aref matrix 2))
            (* *size* (aref matrix 3))
            (* *size* (aref matrix 4))
            (* *size* (aref matrix 5)))))

(defmethod typeset-font-glyph ((glyph transformed-glyph) instance index out)
  (synchronise-position out)
  (let ((glyph* (transformed-glyph-glyph glyph))
        (matrix (transformed-glyph-matrix glyph))
        dx-0 dy-0)
    (let ((*current-x* 0) (*current-y* 0)
          (*typesetter-x* 0) (*typesetter-y* 0)
          (*current-instance* instance))
      (typeset-font-glyph glyph* instance index out)
      (setf dx-0 *typesetter-x* dy-0 *typesetter-y*))
    (multiple-value-bind (dx dy) (dtransform dx-0 dy-0 matrix)
      (incf *typesetter-x* (scaled-quantity dx))
      (incf *typesetter-y* (scaled-quantity dy)))
    (incf *current-x* (scaled-glyph-width glyph))))

(defmethod typeset-glyph ((glyph generic-transformed-glyph) out)
  (let ((glyph* (generic-transformed-glyph-glyph glyph))
        (matrix (generic-transformed-glyph-matrix glyph)))
    (unless (space-glyph-p glyph*)
      (finish-string out)
      (format out "GSV [~A ~A ~A ~A ~A ~A] C "
              (aref matrix 0) (aref matrix 1) (aref matrix 2) (aref matrix 3) 
              (aref matrix 4) (aref matrix 5))
      (multiple-value-bind (*current-x* *current-y*)
          (transform *current-x* *current-y* matrix)
        (multiple-value-bind (*typesetter-x* *typesetter-y*)
            (if *typesetter-x*
                (transform *typesetter-x* *typesetter-y* matrix)
              (values nil nil))
          (typeset-glyph glyph* out)
          (finish-string out)))
      (format out "GR~%")
      (setf *typesetter-x* nil *typesetter-y* nil)) ; for now
    (incf *current-x* (scaled-glyph-width glyph))))
