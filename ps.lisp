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

(defmethod select-font-instance ((instance ps-font-instance) out)
  (unless (ps-font-instance-name instance)
    (error "Selecting unnamed instance."))
  (format out "~A setfont~%" (ps-font-instance-name instance)))

(defmethod setup-font (out (font ps-font) size name)
  (setf (ps-font-name font) name)
  (format out "/~A /~A findfont ~A scalefont def~%"
          name (font-name font) size))

(defun output-encoding (vector out)
  (let ((n 0))
    (loop for i from 0 upto 255
          when (aref vector i)
          do (incf n))
    (cond
      ((>= n 100)
       (format out "[ ")
       (loop for i from 0 upto 255
            when (and (not (zerop i)) (= 0 (mod i 8)))
             do (format out "~%  ")
             do (format out "/~A "
                        (if (aref vector i)
                            (glyph-name (aref vector i))
                            ".notdef")))
       (format out "]~%"))
      (t
       (format out "UE~%")
       (loop for i from 0 upto 255
             when (aref vector i)
             do (format out "dup ~A /~A put~%" i 
                        (glyph-name (aref vector i))))))))

(defmethod typeset-font-glyph :before (glyph (instance ps-font-instance)
                                             index out)
  (declare (ignore glyph index))
  (unless (eql *current-instance* instance)
    (finish-string out)
    (select-font-instance instance out)
    (setf *current-instance* instance)))

(defmethod typeset-font-glyph ((glyph font-glyph) (instance ps-font-instance)
                               index out)
  (synchronise-position out)
  (output-character (code-char index) out)
  (let ((width (scaled-glyph-width glyph)))
    (incf *current-x* width)
    (incf *typesetter-x* width)))
