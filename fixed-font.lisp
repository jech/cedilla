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

(defmethod get-glyph ((font fixed-encoding-font) name &optional index)
  (declare (ignore name))
  (and index (>= index 0) (< index (length (font-glyphs font)))
       (aref (font-glyphs font) index)))

(defmethod put-glyph ((font fixed-encoding-font) glyph &optional index)
  (when (and index (>= index 0) (< index (length (font-glyphs font))))
    (setf (aref (font-glyphs font) index) glyph)))

(defmethod find-font-glyph (ccs (font fixed-encoding-font) &optional dotless)
  (if dotless
      (find-font-glyph (dotless-character ccs) font nil)
      (let ((index (funcall (font-encoding font) ccs)))
        (and index (>= index 0) (< index (length (font-glyphs font)))
             (aref (font-glyphs font) index)))))

(defun fixed-glyph-index (glyph font)
  (loop for i from 0 upto (min 255 (- (length (font-glyphs font)) 1))
        when (eql glyph (aref (font-glyphs font) i))
        do (return-from fixed-glyph-index i))
  nil)

(defmethod ensure-instance-with-font (glyph (font fixed-encoding-font))
  (unless (glyph-instance glyph)
    (unless (font-instances font)
      (make-font-instance font))
    (let ((instance (car (font-instances font)))
          (index (fixed-glyph-index glyph font)))
      (unless index
        (error "Unknown glyph: ~S ~S" glyph font))
      (setf (glyph-instance glyph) instance
            (glyph-index glyph) index))))

(defmethod ensure-instance-with-font ((glyph transformed-glyph)
                                      (font fixed-encoding-font))
  (let ((glyph* (transformed-glyph-glyph glyph))
        (font* (transformed-font-font font)))
    (unless (font-instances font)
      (make-font-instance font))
    (let ((instance (car (font-instances font)))
          (index (fixed-glyph-index glyph* font*)))
      (unless index
        (error "Unknown glyph: ~S ~S" glyph font))
      (setf (glyph-instance glyph) instance
            (glyph-index glyph) index
            (aref (font-glyphs font) index) glyph))))

(defmethod make-font-instance ((font fixed-encoding-ps-font))
  (when (font-instances font)
    (error "Only one font instance per fixed encoding font"))
  (let ((instance (make-instance 'fixed-encoding-ps-font-instance
                                 :font font)))
    (push instance (font-instances font))
    instance))

(defmethod setup-font-instance (out (instance fixed-encoding-ps-font-instance)
                                name)
  (setf (ps-font-instance-name instance) name)
  (format out "/~A ~A def~%" 
          name (ps-font-name (instance-font instance))))
