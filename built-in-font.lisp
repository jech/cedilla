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

(defun make-built-in-font (&key (width 600) (figure-width width)
                           (cap-height 562) (x-height (/ cap-height 1.32)))
  (let* ((font-name (format nil "Cedilla~,'0X~,'0X"
                            (get-universal-time)
                            (random #x10000)))
         (font (make-instance 'built-in-font
                              :name font-name)))
    (setf (font-resources font) (list (make-built-in-font-resource 
                                       :type :font
                                       :name font-name
                                       :font font)))
    (flet ((define-glyph (name width x0 y0 x1 y1 string)
             (let ((glyph
                    (make-built-in-glyph
                     :name name
                     :width width :x0 x0 :y0 y0 :x1 x1 :y1 y1
                     :font font
                     :charstring string)))
               (setf (gethash name (font-glyphs font)) glyph)
               (setf (font-x0 font) (min (font-x0 font) (glyph-x0 glyph)))
               (setf (font-y0 font) (min (font-y0 font) (glyph-y0 glyph)))
               (setf (font-x1 font) (max (font-x1 font) (glyph-x1 glyph)))
               (setf (font-y1 font) (max (font-y1 font) (glyph-y1 glyph))))))
      (define-glyph "Euro" figure-width 0 0 figure-width (* 1.03 cap-height)
        (format nil
          "gsave ~A ~A scale~%~
           955 232 MT 904 180 LT 829 117 734 83 636 83 CT~%~
           406 83 220 270 220 500 CT 220 730 406 916 636 916 CT~%~
           759 916 876 862 955 767 CT 988 855 LT~%~
           894 947 768 1000 636 1000 CT 360 1000 136 776 136 500 CT~%~
           136 224 360 0 636 0 CT 753 0 866 40 955 115 CT CP F~%~
           869 541 MT 901 625 LT 31 625 LT 0 541 LT CP F~%~
           805 375 MT 837 458 LT 31 458 LT 0 375 LT CP F~%~
           grestore~%"
          (/ figure-width 1000.0) (/ (* 1.03 cap-height) 1000.0)))
      (let* ((cx (/ width 2.0)) (cy (/ x-height 2.0)) 
             (r (* x-height 0.3)))
        (define-glyph "circle" width 
          (- cx (+ r 15)) (- cy (+ r 15)) (+ cx (+ r 15)) (+ cy (+ r 15))
          (format nil
            "30 SLW ~A ~A MT ~A ~A ~A 0 360 arc stroke"
            (+ cx r) cy cx cy r))
        (define-glyph "dottedcircle" width 
          (- cx (+ r 15)) (- cy (+ r 15)) (+ cx (+ r 15)) (+ cy (+ r 15))
          (format nil
            "30 SLW [60] 0 setdash ~A ~A MT ~A ~A ~A 0 360 arc stroke"
            (+ cx r) cy cx cy r)))
      (define-glyph "hookabovecomb" 200 5 -15 195 255
        (format nil
          "30 SLW ~%~
           100 0 MT 100 80 LT 100 160 80 -90 180 arc stroke"))
      (define-glyph "horncmb" 300 -30 -15 200 100
        (format nil
          "30 SLW~%~
           -15 0 MT 100 0 LT 100 100 100 -90 -45 arc stroke~%~
           170 30 MT 135 65 50 -45 315 arc fill"))
      (define-glyph "zerowidthspace" 0 0 0 0 0
        "")
      )
    font))

(defmethod download-resource ((resource built-in-font-resource) out)
  (let ((font (ps-resource-font resource))
        (glyphs '()))
    (dolist (i (font-instances font))
      (let ((g (instance-glyphs i)))
        (loop for i from 0 upto 255
              when (aref g i)
              do (push (aref g i) glyphs))))
    (nreverse glyphs)
    (format out "/~A 12 dict dup begin~%" (font-name font))
    (format out "/FontType 3 def~%/PaintType 0 def~%")
    (format out "/FontMatrix [0.001 0 0 0.001 0 0] readonly def~%")
    (format out "/FontBBox{~A ~A ~A ~A}readonly def~%"
            (font-x0 font) (font-y0 font) (font-x1 font) (font-y1 font))
    (format out "/StrokeWidth 0 def~%")
    (format out "/Encoding StandardEncoding def~%")
    (format out "/FontName /~A def~%" (font-name font))
    (format out "/BuildChar {~
                 1 index /Encoding get exch get ~
                 1 index /BuildGlyph get exec ~
                 } bind def~%")
    (format out "/BuildGlyph {~
                 exch begin ~
                 /CharProc load dup ~
                 2 index known {exch get exec} ~
                 {pop pop 600 0 0 0 0 0 setcachedevice} ~
                 ifelse end } bind def~%")
    (format out "/CharProc ~A dict dup begin~%" (length glyphs))
    (dolist (glyph glyphs)
      (format out "/~A {~%" (glyph-name glyph))
      (format out "~A 0 ~A ~A ~A ~A setcachedevice~%"
              (glyph-width glyph)
              (glyph-x0 glyph) (glyph-y0 glyph) 
              (glyph-x1 glyph) (glyph-y1 glyph))
      (format out "~A" (built-in-glyph-charstring glyph))
      (format out "} bind def~%"))
    (format out "end def end definefont pop~%")))

