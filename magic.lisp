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

(defmethod ensure-instance ((glyph magic-glyph))
  (ensure-instance (magic-glyph-glyph glyph)))

(defconstant *dotless-factor* 0.75)
(defconstant *dotless-metrics-factor* 0.65)

(eval-when (compile load eval)
(defconstant *enclosing-line-width* 30)
)
(defconstant *enclosing-line-12* (/ *enclosing-line-width* 2.0))

(defun make-dotless-glyph (glyph)
  (%make-dotless-glyph :name nil
                       :width (glyph-width glyph)
                       :x0 (glyph-x0 glyph)
                       :y0 (glyph-y0 glyph)
                       :x1 (glyph-x1 glyph)
                       :y1 (* *dotless-metrics-factor* (glyph-y1 glyph))
                       :glyph glyph))

(defmethod typeset-glyph ((glyph dotless-glyph) out)
  (let* ((g (magic-glyph-glyph glyph))
         (x *current-x*) 
         (y *current-y*)
         (x0 (- (scaled-quantity (glyph-x0 g)) 20))
         (y0 (- (scaled-quantity (glyph-y0 g)) 20))
         (x1 (+ (scaled-quantity (glyph-x1 g)) 20))
         (y1 (* *dotless-factor* (scaled-quantity (glyph-y1 g)))))
    (finish-string out)
    (format out "CSV ~A ~A ~A ~A RC~%"
            (+ x x0) (+ y y0) (- x1 x0) (- y1 y0))
    (setf *typesetter-x* nil *typesetter-y* nil)
    (typeset-glyph g out)
    (finish-string out)
    (format out "CR~%")))

(defun make-magic-glyph (type base glyph)
  (case type
    ((:dotless) (make-dotless-glyph glyph))
    ((:circle #.(code-char #x20DD)) (make-enclosed-glyph glyph "CRC" 150))
    ((:small-circle) (make-enclosed-glyph glyph "CRC" 300))
    ((:square #.(code-char #x20DE)) (make-enclosed-glyph glyph "SQR" 150))
    ((:small-square) (make-enclosed-glyph glyph "SQR" 0.1))
    ((:diamond #.(code-char #x20DF)) (make-enclosed-glyph glyph "DMND" 300))
    ((:screen #.(code-char #x20E2)) (make-enclosed-glyph glyph "SCRN" 200))
    ((:keycap #.(code-char #x20E3)) (make-enclosed-glyph glyph "KCP" 200))
    ((:bar) (make-barred-glyph base glyph))
    ((:double-v :double-h) (make-overstruck-glyph glyph glyph type))
    ((:small :narrow :wide :stretch :super :sub
             :mirror-h :mirror-v :rotate :turn)
     (make-trans-glyph glyph type))
    (t nil)))

(defun make-enclosed-glyph (glyph op &optional (frob 0))
  (let* ((xx (- (glyph-x1 glyph) (glyph-x0 glyph)))
         (dx (+  *enclosing-line-12* frob (- (glyph-x0 glyph))))
         (x0 (- *enclosing-line-12*))
         (x1 (+ dx xx frob *enclosing-line-width*))
         (y0 (- (+ (- (glyph-y0 glyph)) frob *enclosing-line-width*)))
         (y1 (+ (glyph-y1 glyph) frob *enclosing-line-width*)))
    (%make-enclosed-glyph :name nil
                          :width (+ x1 *enclosing-line-width*)
                          :x0 x0 :y0 y0 :x1 x1 :y1 y1
                          :dx dx
                          :glyph glyph
                          :op op)))

(defmethod typeset-glyph ((glyph enclosed-glyph) out)
  (let* ((g (enclosed-glyph-glyph glyph))
         (x0 (+ (glyph-x0 glyph) *enclosing-line-12*))
         (y0 (+ (glyph-y0 glyph) *enclosing-line-12*))
         (x1 (- (glyph-x1 glyph) *enclosing-line-12*))
         (y1 (- (glyph-y1 glyph) *enclosing-line-12*))
         (dx (enclosed-glyph-dx glyph))
         (x *current-x*) (y *current-y*))
    (incf *current-x* (scaled-quantity dx))
    (typeset-glyph g out)
    (finish-string out)
    (setf *current-line-width* (scaled-quantity *enclosing-line-width*))
    (synchronise-line-width out)
    (format out "~A ~A ~A ~A ~A~%"
            (+ x (scaled-quantity x0))
            (+ y (scaled-quantity y0))
            (scaled-quantity (- x1 x0))
            (scaled-quantity (- y1 y0))
            (enclosed-glyph-op glyph))
    (setf *current-x* (+ x (scaled-glyph-width glyph))
          *current-y* y)
    (setf *typesetter-x* nil *typesetter-y* nil)))


(defun make-barred-glyph (base glyph)
  (let* ((x0 (glyph-x0 glyph)) (y0 (glyph-y0 glyph))
         (x1 (glyph-x1 glyph)) (y1 (glyph-y1 glyph))
         (y12 (/ (+ y0 y1) 2.0))
         (ym (+ y12 170)) (yl (- y12 50)) (yll (- y12 200))
         (xl (- x0 50)) (xl* (+ x0 100)) (xlr (+ x0 250))
         (xr (+ x1 50)) (xr* (- x1 100)) (xrl (- x1 250))
         bx0 by0 bx1 by1)
    (case (ccs-base base)
      ((#\d) (setf bx0 xrl bx1 xr by0 ym by1 ym))
      ((#\D) (setf bx0 xl bx1 xr by0 ym by1 ym))
      ((#\g) (setf bx0 xrl bx1 xr by0 yll by1 yll))
      ((#\G) (setf bx0 xl bx1 xlr by0 y12 by1 y12))
      ((#\h) (setf bx0 xl bx1 xlr by0 ym by1 ym))
      ((#\H) (setf bx0 xl bx1 xr by0 ym by1 ym))
      ((#\i) (setf bx0 xl* bx1 xr* by0 yl by1 yl))
      ((#\I) (setf bx0 xl* bx1 xr* by0 y12 by1 y12))
      ((#\t) (setf bx0 xl bx1 xlr by0 yl by1 yl))
      ((#\T) (setf bx0 xl* bx1 xr* by0 y12 by1 y12))
      ((#\z) (setf bx0 xl* bx1 xr* by0 y12 by1 y12))
      ((#\Z) (setf bx0 xl* bx1 xr* by0 y12 by1 y12))
      (t (setf bx0 xl bx1 xr by0 y12 by1 y12)))
    (let ((gx0 (min (glyph-x0 glyph) (- bx0 *enclosing-line-12*)))
          (gy0 (min (glyph-y0 glyph) (- by0 *enclosing-line-12*)))
          (gx1 (min (glyph-x1 glyph) (+ bx1 *enclosing-line-12*)))
          (gy1 (min (glyph-y1 glyph) (+ by1 *enclosing-line-12*))))
      (%make-barred-glyph
       :width (glyph-width glyph)
       :glyph glyph
       :x0 gx0 :y0 gy0 :x1 gx1 :y1 gy1
       :bx0 bx0 :by0 by0 :bx1 bx1 :by1 by1))))

(defmethod typeset-glyph ((glyph barred-glyph) out)
  (let ((x *current-x*) (y *current-y*))
    (typeset-glyph (magic-glyph-glyph glyph) out)
    (finish-string out)
    (setf *current-line-width* (scaled-quantity *enclosing-line-width*))
    (synchronise-line-width out)
    (format out "~A ~A MT ~A ~A LT stroke~%"
            (+ x (scaled-quantity (barred-glyph-bx0 glyph)))
            (+ y (scaled-quantity (barred-glyph-by0 glyph)))
            (+ x (scaled-quantity (barred-glyph-bx1 glyph)))
            (+ y (scaled-quantity (barred-glyph-by1 glyph))))
    (setf *typesetter-x* nil *typesetter-y* nil)))

(defun make-overstruck-glyph (g1 g2 type)
  (let (dx dy width)
    (ecase type
      ((:double-h) 
       (setf dx 150 dy 0 
             width (max (glyph-width g1) (+ 150 (glyph-width g2)))))
      ((:double-v) 
       (setf dx 0 dy 150
             width (max (glyph-width g1) (glyph-width g2))))
      ((:overstrike) 
       (setf dx 0 dy 0
             width (max (glyph-width g1) (glyph-width g2)))))
    (make-composite-glyph
     :width width
     :base :self
     :components
     (list (make-composite-component :glyph g1 :dx 0 :dy 0)
           (make-composite-component :glyph g2 :dx dx :dy dy)))))

(defvar *small-matrix* #(0.5 0 0 0.5 0 0))
(defvar *narrow-matrix* #(0.5 0 0 1 0 0))
(defvar *wide-matrix* #(2 0 0 1 0 0))
(defvar *mirror-h-matrix* #(-1 0 0 1 0 0))
(defvar *mirror-v-matrix* #(1 0 0 -1 0 0))
(defvar *turn-matrix* #(-1 0 0 -1 0 0))
(defvar *rotate-matrix* #(0 1 -1 0 0 0))
(defvar *stretch-matrix* #(1 0 0 1.4 0 0))

(defun make-trans-glyph (glyph type)
  (let ((matrix 
         (ecase type
           ((:super :sub :small) *small-matrix*)
           ((:narrow) *narrow-matrix*)
           ((:wide) *wide-matrix*)
           ((:stretch) *stretch-matrix*)
           ((:mirror-h) *mirror-h-matrix*)
           ((:mirror-v) *mirror-v-matrix*)
           ((:rotate) *rotate-matrix*)
           ((:turn) *turn-matrix*))))
    (let ((glyph* (make-transformed-glyph glyph matrix)))
      (and glyph*
           (multiple-value-bind (width dx dy)
               (ecase type
                 ((:super) (values (glyph-width glyph*) 0 300))
                 ((:sub) (values (glyph-width glyph*) 0 -300))
                 ((:small :narrow :wide :stretch) 
                  (values (glyph-width glyph*) 0 0))
                 ((:mirror-h) (values (glyph-width glyph)
                                      (glyph-width glyph) 0))
                 ((:mirror-v) (values (glyph-width glyph*) 0 (glyph-y1 glyph)))
                 ((:turn) (values (glyph-width glyph) 
                                  (glyph-width glyph) (glyph-y1 glyph)))
                 ((:rotate) (values (glyph-y1 glyph) 
                                    (glyph-y1 glyph) 0)))
             (if (and (= width (glyph-width glyph*)) (= 0 dx) (= 0 dy))
                 glyph*
                 (make-composite-glyph
                  :base :self :width width
                  :components 
                  (list (make-composite-component 
                         :glyph glyph* :dx dx :dy dy)))))))))
