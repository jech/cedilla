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

(defvar *known-glyphs*)
(defvar *currently-computed-glyphs* '())
(defvar *currently-computed-simple-glyphs* '())

(defun recall-glyph (ccs)
  "Recall a previously computed glyph for CCS."
  (let ((val (gethash ccs *known-glyphs*)))
    (if (eql val :unknown) nil val)))

(defun remember-glyph (ccs glyph)
  "Remember GLYPH as being associated to CCS.  Returns GLYPH."
  (when ccs
    (setf (gethash ccs *known-glyphs*) (or glyph :unknown)))
  glyph)


(defun compute-stream-glyphs (in font-list)
  "Compute and setup all glyphs needed to typeset the character stream IN."
  (loop
   (let ((ccs (next-ccs in)))
     (when (eql ccs in) (return nil))
     (compute-glyph ccs font-list))))

(defun compute-glyph (ccs font-list &optional (fallback #\?))
  "Compute and setup the glyph needed to typeset CCS."
  (cond
    ((and (characterp ccs) (<= (char-code ccs) 31)) nil)
    (t
     (let ((glyph (or (recall-glyph ccs)
                      (find-glyph ccs font-list))))
       (etypecase glyph
         (null (when-verbose (warn "Giving up on ~S" ccs)))
         (glyph (ensure-instance glyph))
         (list (mapc #'ensure-instance glyph)))
       (or glyph
           (if fallback
               (compute-glyph fallback font-list nil)))))))

(eval-when (load compile eval)

(defmacro with-circularity-detection ((ccs &optional font) &body body)
  (let ((tested (gensym "TESTED")))
    `(let ((,tested (list ,font ,ccs)))
      (unless (member ,tested *currently-computed-glyphs* :test 'equal)
        (let ((*currently-computed-glyphs* 
               (cons ,tested *currently-computed-glyphs*)))
          ,@body)))))

(defmacro with-simple-circularity-detection ((ccs &optional font) &body body)
  (let ((tested (gensym "TESTED")))
    `(let ((,tested (list ,font ,ccs)))
      (unless (member ,tested *currently-computed-simple-glyphs* :test 'equal)
        (let ((*currently-computed-simple-glyphs* 
               (cons ,tested *currently-computed-simple-glyphs*)))
          ,@body)))))
)

(defun find-glyph (ccs font-list)
  "Memoised version of FIND-GLYPH*."
  (or (recall-glyph ccs)
      (remember-glyph ccs (find-glyph* ccs font-list))))
  
(defun find-glyph* (ccs font-list)
  "Compute the glyph needed to typeset CCS."
  (with-circularity-detection (ccs)
    (or
     (cond
       ((vectorp ccs)
        (let ((l (map 'list
                      #'(lambda (ccs)
                          (let ((glyph (find-glyph ccs font-list)))
                            (unless glyph
                              (return-from find-glyph* nil))
                            (if (listp glyph) glyph (list glyph))))
                      ccs)))
          (apply #'append l)))
       (t
        (or
         (dolist (font font-list)
           (let ((g (find-glyph-with-font ccs font font-list)))
             (when g (return-from find-glyph* g))))
         (when (and (consp ccs) (or (symbolp (car ccs)) 
                                    (eql 0 (ccs-combining-class (car ccs)))))
           (find-magic-glyph (car ccs) (cdr ccs) font-list))
         (some #'(lambda (fallback) (find-glyph fallback font-list))
               (ccs-fallbacks ccs))
         ;; This is needed in order to find the fallback of an alternative
         (some #'(lambda (alt) (find-glyph alt font-list))
               (ccs-alternatives ccs))
         (when (consp ccs)
           (some #'(lambda (fallback) 
                     (find-glyph (cons (car ccs) fallback) font-list))
                 (ccs-fallbacks (cdr ccs)))
           (some #'(lambda (alt)
                     (find-glyph (cons (car ccs) alt) font-list))
                 (ccs-alternatives (cdr ccs))))))))))

(defun find-simple-glyph (ccs font-list)
  "Compute a simple (not composite) glyph that can be used to typeset CCS."
  (with-simple-circularity-detection (ccs)
    (or
     (dolist (font font-list)
       (let ((g (find-simple-glyph-with-font ccs font font-list)))
         (when g (return-from find-simple-glyph g))))
     (when (and (consp ccs) (or (symbolp (car ccs)) 
                                (eql 0 (ccs-combining-class (car ccs)))))
       (find-magic-glyph (car ccs) (cdr ccs) font-list nil t))
     (some #'(lambda (fallback) (find-simple-glyph fallback font-list))
           (ccs-fallbacks ccs)))))

(defun find-simple-glyph-with-font (ccs font font-list)
  "Find a simple glyph for CCS in FONT."
  (with-simple-circularity-detection (ccs font)
    (cond
      ((and (consp ccs) (or (symbolp (car ccs)) 
                            (eql 0 (ccs-combining-class (car ccs)))))
       (find-magic-glyph (car ccs) (cdr ccs) font-list font t))
      (t 
       (or (find-font-glyph ccs font)
           (some
            #'(lambda (alt) (find-simple-glyph-with-font alt font font-list))
            (ccs-alternatives ccs))
           ;; This is needed in order to properly find precomposed glyphs
           ;; in the presence of the varia/grave and psili/acute nonsense.
           (and (consp ccs)
                (some #'(lambda (alt)
                          (find-simple-glyph-with-font 
                           (cons (car ccs) alt) font font-list))
                      (ccs-alternatives (cdr ccs)))))))))
  
(defun find-glyph-with-font (ccs font font-list)
  "Find a glyph suitable for typesetting CCS that uses FONT for its base part"
  (with-circularity-detection (ccs font)
    (or
     (cond
       ((vectorp ccs)
        (map 'list
             #'(lambda (ccs)
                (or (find-glyph-with-font ccs font font-list)
                    (return-from find-glyph-with-font nil)))
             ccs))
       (t
        (or
         (find-simple-glyph-with-font ccs font font-list)
         (when (consp ccs)
           (or
            ;; Special-case double combining characters that sometimes
            ;; come precomposed -- e.g. dialytika-tonos.  Yuck.
            (when (and (characterp (car ccs)) (consp (cdr ccs))
                       (characterp (cadr ccs)) (not (null (cddr ccs))))
              (find-composite-glyph (cons (car ccs) (cadr ccs))
                                    (cddr ccs)
                                    font-list font))
            (find-composite-glyph (car ccs) (cdr ccs) font-list font)))
         (some
          #'(lambda (alt) (find-glyph-with-font alt font font-list))
          (ccs-alternatives ccs))))))))

(defun find-magic-glyph (magic ccs font-list &optional font simple)
  "Compute a magic glyph."
  (flet ((f-g (ccs)
           (cond
             ((and font simple) 
              (find-simple-glyph-with-font ccs font font-list))
             ((and font (not simple))
              (find-glyph-with-font ccs font font-list))
             ((and (not font) simple)
              (find-simple-glyph ccs font-list))
             (t 
              (find-glyph ccs font-list)))))
    (cond
      ((and (eql magic ':dotless) 
            (or (not (dotted-character-p ccs))
                (consp ccs)))
       (f-g ccs))
      (t
       (or
        (and (eql magic ':dotless)
             font
             (find-font-glyph ccs font t))
        (let ((glyph (ensure-glyph (f-g ccs))))
          (and glyph (make-magic-glyph magic ccs glyph))))))))
     
(defun find-composite-glyph (c-ccs b-ccs font-list &optional font)
  "Compose C-CCS over B-CCS."
  (cond
    ((or (symbolp c-ccs) (eql 0 (ccs-combining-class c-ccs)))
     (find-magic-glyph c-ccs b-ccs font-list font))
    (t
     (let ((b-glyph 
            (ensure-glyph
             (if font
                 (find-glyph-with-font (cons ':dotless b-ccs) font font-list)
                 (find-glyph (cons ':dotless b-ccs) font-list)))))
       (and b-glyph
            (let* ((preferred-font
                    (or font
                        (and (font-glyph-p b-glyph) (glyph-font b-glyph))))
                   (c-glyph
                    (ensure-glyph
                     (or
                      (and preferred-font
                           (find-simple-glyph-with-font 
                            c-ccs preferred-font font-list))
                      (find-simple-glyph c-ccs font-list)
                      (find-special-case-combining-glyph
                       c-ccs font-list font)))))
              (and c-glyph
                   (compute-composite-glyph c-ccs c-glyph b-ccs b-glyph))))))))

(defun find-special-case-combining-glyph (ccs font-list &optional font)
  (labels ((g (ccs)
             (or
              (and font (find-simple-glyph-with-font ccs font font-list))
              (find-simple-glyph ccs font-list)))
           (mcg (g1 g2 dx dy)
             (make-composite-glyph
              :components
              (list
               (make-composite-component :glyph g1 :dx 0 :dy 0)
               (make-composite-component :glyph g2 :dx dx :dy dy))))
           (side-by-side (g1 g2)
             (let ((dx 0)
                   (x1-1 (glyph-x1 g1)) (x0-2 (glyph-x0 g2))
                   (y1 (/ (+ (glyph-y0 g1) (glyph-y1 g1)) 2.0))
                   (y2 (/ (+ (glyph-y0 g2) (glyph-y1 g2)) 2.0)))
               (mcg g1 g2 (+ dx (- x1-1 x0-2)) (- y1 y2))))
           (stack (g1 g2)
             (let ((xc-1 (/ (+ (glyph-x0 g1) (glyph-x1 g1)) 2.0))
                   (xc-2 (/ (+ (glyph-x0 g2) (glyph-x1 g2)) 2.0))
                   (y1-1 (glyph-y1 g1)) (y0-2 (glyph-y0 g2)))
               (mcg g2 g1 (- xc-2 xc-1) (- y0-2 y1-1)))))
    (when (and (consp ccs))
      (let* ((ccs1 (car ccs)) (ccs2 (cdr ccs))
            (c1 (and (characterp ccs1) (char-code ccs1)))
            (c2 (and (characterp ccs1) (char-code ccs2))))
        (cond
          ;; Greek
          ((and
            (member c1 '(#x300 #x301 #x302 #x303))
            (member c2 '(#x313 #x314 #x342))
           (let* ((g1 (g ccs1)) (g2 (and g1 (g ccs2))))
             (and g2 (side-by-side g2 g1)))))
          ((and
            (member c1 '(#x300 #x301 #x302 #x342))
            (equal c2 #x308)
           (let* ((g1 (g ccs1)) (g2 (and g1 (g ccs2))))
             (and g2 (stack g2 g1)))))
          ;; Vietnamese
          ((and (member c1 '(#x300 #x301)) (eql c2 #x302))
           (let ((g1 (g ccs1)) (g2 (g ccs2)))
             (when g2
               (let* ((x0-1 (glyph-x0 g1)) (y0-1 (glyph-y0 g1))
                      (x1-1 (glyph-x1 g1))
                      (x0-2 (glyph-x0 g2)) (x1-2 (glyph-x1 g2))
                      (ym-2 (+ (glyph-y0 g2) 
                               (* 0.5 (- (glyph-y1 g2) (glyph-y0 g2))))))
                 (mcg g2 g1 
                      (if (eql c1 #x300) ; grave
                          (+ (- x0-2 x1-1) (* (- x1-2 x0-2) 0.15))
                          (+ (- x0-2 x0-1) (* (- x1-2 x0-2) 0.85)))
                      (- ym-2 y0-1))))))
          ((and
            (member c1 '(#x300 #x301 #x303 #x309 #x313))
            (member c2 '(#x302 #x306)))
           (let* ((g1 (g ccs1)) (g2 (and g1 (g ccs2))))
             (and g2 (stack g2 g1))))
          (t nil))))))

(defun compute-composite-glyph (cc gc ccs gb)
  "Position a composite component over a base glyph.
The base glyph is GB (associated to CCS); the component is GC (CC)."
  (flet ((av (x y) (* 0.5 (+ x y))))
    (let* ((class (ccs-combining-class cc))
           (gbb (glyph-base gb))        ; hereditary base glyph
           (eb (or (= (glyph-x0 gb) (glyph-x1 gb)) ; empty base
                   (= (glyph-y0 gb) (glyph-y1 gb))))
           (x0-bb (+ (glyph-base-dx gb) (glyph-x0 gbb)))
           (y0-bb (+ (glyph-base-dy gb) (glyph-y0 gbb)))
           (x1-bb (+ (glyph-base-dx gb) (glyph-x1 gbb)))
           (y1-bb (+ (glyph-base-dy gb) (glyph-y1 gbb)))
           (h (not (member class '(202 214 220 230 233 234 240 1))))
           (v (not (member class '(200 202 204))))
           (x0-b (if eb 0 (if h (glyph-x0 gb) x0-bb)))
           (y0-b (if eb 0 (if v (glyph-y0 gb) y0-bb)))
           (x1-b (if eb 600 (if h (glyph-x1 gb) x1-bb)))
           (y1-b (if eb 430 (if v (glyph-y1 gb) y1-bb)))
           (x12-b (av x0-b x1-b))
           (y12-b (av y0-b y1-b))
           (x0-c (glyph-x0 gc)) (x1-c (glyph-x1 gc))
           (y0-c (glyph-y0 gc)) (y1-c (glyph-y1 gc))
           (x12-c (av x0-c x1-c))
           (y12-c (av y0-c y1-c))
           dx dy)
      (case class
        ((202 214 220 230 233 234 240 1)
         (cond
           ((eql cc (code-char #x328))  ; ogonek
            (case (ccs-base ccs)
              ((#\a #\A #\u) (setf dx (+ (- x12-b x12-c) 80)))
              ((#\e #\E) (setf dx (+ (- x12-b x12-c) 20)))
              (t (setf dx (- x12-b x12-c)))))
           ((and (eql class 230) (eql (ccs-base ccs) #\j))
            (setf dx (+ (- x12-b x12-c) 80)))
           ((and (consp cc) 
                 (eql (car cc) (code-char #x300))
                 (eql (cdr cc) (code-char #x302))) ; circumflex-grave
            (setf dx (+ (- x12-b x12-c) -65)))
           ((and (consp cc)
                 (eql (car cc) (code-char #x301))
                 (eql (cdr cc) (code-char #x302))) ; circumflex-acute
            (setf dx (+ (- x12-b x12-c) +65)))
           (t (setf dx (- x12-b x12-c)))))
        ((222 226 232)
         (setf dx (+ (- x1-b x0-c) 20)))
        ((216)
         (setf dx (+ (- x1-b x0-c) -80)))
        (t (warn "Unhandled combining class ~A (dx)" class) (setf dx 0)))
      (case class
        ((200 202 204) (setf dy (+ (- y0-b y1-c) 10)))
        ((214) (setf dy (+ (- y1-b y0-c) -10)))
        ((212 216) (setf dy (+ (- y1-b y1-c) 30)))
        ((1 208 210 224 226) (setf dy (- y12-b y12-c)))
        ((218 220 222 233 240) (setf dy (+ (- y0-b y1-c) -70)))
        ((228 230 232 234) (setf dy (+ (- y1-b y0-c) 70)))
        (t (warn "Unhandled combining class ~A (dy)" class) (setf dy 0)))
      (make-composite-glyph
       :components
       (list (make-composite-component :glyph gb :dx 0 :dy 0)
             (make-composite-component :glyph gc :dx dx :dy dy))))))

(defmethod ensure-glyph (list)
  "Given a glyph or list of glyphs, return an equivalent glyph."
  (cond
    ((glyph-p list) list)
    ((null (cdr list)) (car list))
    (t 
     (let ((components nil) (width 0))
       (dolist (g list)
         (push (make-composite-component :glyph g :dx width :dy 0) components)
         (incf width (glyph-width g)))
       (make-composite-glyph :width width
                             :base :self
                             :components (nreverse components))))))

(defmethod ensure-instance ((glyph font-glyph))
  (unless (glyph-instance glyph)
    (ensure-instance-with-font glyph (glyph-font glyph))))

(defmethod ensure-instance ((glyph composite-glyph))
  (dolist (c (composite-glyph-components glyph))
    (ensure-instance (component-glyph c))))
            
(defmethod put-glyph :before ((font font) (glyph font-glyph) &optional index)
  (declare (ignore index))
  (setf (glyph-font glyph) font))

(defun header-string (string)
  (with-output-to-string (s)
    (do ((i 0))
        ((= i (length string)))
      (cond
       ((eql (aref string i) #\%)
        (ecase (aref string (+ i 1))
          ((#\%) (princ #\%))
          ((#\,) (princ #\,))
          ((#\p) (format s "~A" *page-number*))
          ((#\P) (format s "~R" *page-number*))
          ((#\r) (format s "~(~@R~)" *page-number*))
          ((#\R) (format s "~@R" *page-number*)))
        (incf i 2))
       (t (princ (aref string i) s)
          (incf i))))))

(defun compute-header-glyphs (string font-list)
  (flet ((compute-digit-glyphs ()
          (dolist (c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
            (compute-glyph c font-list)))
         (compute-lower-roman-glyphs ()
          (dolist (c '(#\i #\v #\x #\c #\m))
            (compute-glyph c font-list)))
         (compute-upper-roman-glyphs ()
          (dolist (c '(#\I #\V #\X #\C #\M))
            (compute-glyph c font-list)))
         (compute-lower-glyphs ()
            (loop for i from (char-code #\a) upto (char-code #\z)
                  do (compute-glyph (code-char i) font-list)))
         (compute-upper-glyphs ()
            (loop for i from (char-code #\A) upto (char-code #\Z)
                  do (compute-glyph (code-char i) font-list))))
    (do ((i 0))
        ((= i (length string)))
      (cond
       ((eql (aref string i) #\%)
        (ecase (aref string (+ i 1))
          ((#\%) (compute-glyph #\% font-list))
          ((#\,) (compute-glyph #\, font-list))
          ((#\p) (compute-digit-glyphs))
          ((#\P) (compute-lower-glyphs) (compute-glyph #\- font-list))
          ((#\r) (compute-lower-roman-glyphs))
          ((#\R) (compute-upper-roman-glyphs)))
        (incf i 2))
       (t (compute-glyph (normalise-ccs (aref string i)) font-list)
          (incf i))))))

