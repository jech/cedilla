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

(defmethod find-font-glyph (ccs (font space-font) &optional dotless)
  (declare (ignore dotless))
  (and (eql ccs #\Space)
       (or (font-glyph font)
           (let ((glyph (make-space-glyph :width (font-width font)
                                          :x0 0 :y0 0 :x1 0 :y1 0
                                          :font font)))
             (setf (font-glyph font) glyph)
             glyph))))

(defmethod ensure-instance-with-font (glyph (font space-font))
  (unless (eql glyph (font-glyph font))
    (error "Unknown glyph"))
  (unless (glyph-instance glyph)
    (unless (font-instances font)
      (make-font-instance font))
    (setf (glyph-instance glyph) (car (font-instances font))
          (glyph-index glyph) 32)))

(defmethod make-font-instance ((font space-font))
  (when (font-instances font)
    (error "Only one font instance per space font"))
  (let ((instance (make-instance 'space-font-instance :font font)))
    (push instance (font-instances font))
    instance))

(defmethod setup-font (out (font space-font) size name)
  (declare (ignore out size name))
  nil)

(defmethod setup-font-instance (out (instance space-font-instance) name)
  (declare (ignore out name))
  nil)

(defmethod typeset-font-glyph ((glyph space-glyph) 
                               (instance space-font-instance) index out)
  (declare (ignore out index))
  (incf *current-x* (scaled-glyph-width glyph)))
