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

(defmethod get-glyph ((font named-glyph-font) name &optional index)
  (declare (ignore index))
  (gethash name (font-glyphs font)))

(defmethod put-glyph ((font named-glyph-font) glyph &optional index)
  (declare (ignore index))
  (unless (pre-glyph-name glyph) (error "Cannot put unnamed glyph"))
  (setf (gethash (pre-glyph-name glyph) (font-glyphs font)) glyph))

(defmethod find-font-glyph (ccs (font named-glyph-font) &optional dotless)
  (let ((names
         (cond
           ((font-encoding font)
            (if dotless nil (funcall (font-encoding font) ccs)))
           (t (if dotless
                  (list (ccs-dotless-glyph-name ccs))
                  (ccs-glyph-names ccs))))))
    (or (some #'(lambda (name) (gethash name (font-glyphs font))) names)
        (let ((alternatives
               (and (not (font-encoding font)) (not dotless)
                    (ccs-alternate-glyph-names ccs))))
          (some #'(lambda (name) (gethash name (font-glyphs font)))
                alternatives)))))

(defmethod ensure-instance-with-font (glyph (font named-glyph-font))
  (unless (glyph-instance glyph)
    (or 
     (and (font-instances font)
          (add-glyph-to-instance glyph (car (font-instances font))))
     (progn (make-font-instance font)
            (ensure-instance-with-font glyph font)))))

(defmethod add-glyph-to-instance (glyph (instance named-glyph-font-instance))
  (let ((glyphs (instance-glyphs instance)))
    (cond
      ((< (fill-pointer glyphs) 256)
       (setf (glyph-instance glyph) instance)
       (setf (glyph-index glyph) (fill-pointer glyphs))
       (vector-push glyph glyphs))
      (t nil))))

(defmethod make-font-instance ((font named-glyph-ps-font))
  (let ((instance (make-instance 'named-glyph-ps-font-instance
                                 :font font)))
    (push instance (font-instances font))
    instance))

(defmethod setup-font-instance (out (instance named-glyph-ps-font-instance)
                                name)
  (setf (ps-font-instance-name instance) name)
  (format out "/~A dup ~A ~%" 
          name (ps-font-name (instance-font instance)))
  (output-encoding (instance-glyphs instance) out)
  (format out "RCF def~%"))

  