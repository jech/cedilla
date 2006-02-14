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

(defparameter *cedilla-version* "0.6")

(defmacro define-fontset (name spec)
  (let ((symbol (gensym "SYMBOL")))
    `(let ((,symbol (intern (string-upcase (string ,name)) "CEDILLA")))
      (setf (get ,symbol 'fontset) ,spec)
      ,symbol)))

(defmacro define-paper-size (name x y
                                  &key
                                  left-margin
                                  top-margin
                                  right-margin
                                  bot-margin)
  (let ((symbol (gensym "SYMBOL")))
    `(flet ((in (x) (* x 72))
            (cm (x) (/ (* x 72) 2.54)))
      (let ((,symbol (intern (string-upcase (string ,name)) "CEDILLA")))
        (setf (get ,symbol 'paper-size)
              (make-paper-size 
               :x0 0 :y0 0 :x1 ,x :y1 ,y
               :left-margin (or ,left-margin (cm 1.5))
               :top-margin (or ,top-margin (cm 1.5))
               :right-margin (or ,right-margin (cm 1))
               :bot-margin (or ,bot-margin (cm 1.5))))
        ,symbol))))

(defvar *cedilla-verbose* nil)

(defmacro when-verbose (&body body)
  `(when *cedilla-verbose*
    ,@body))

(defmacro with-open-file-or-stream ((var filename &rest args) &body body)
  (let ((b (gensym "BODY")) (f-name (gensym "FILENAME")))
    `(flet ((,b (,var) ,@body))
      (let ((,f-name ,filename))
        (if (streamp ,f-name)
            (,b ,f-name)
            (with-open-file (,var ,filename ,@args)
              (,b ,var)))))))
