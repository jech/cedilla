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

(defpackage "CEDILLA"
    (:use "CL")
    (:export "CEDILLA" "CEDILLA-MAIN"))

(in-package "CEDILLA")

;;; CLISP is getting saner by the day
(eval-when (load compile eval)
  (let ((p (or (find-package "EXT") (find-package "LISP"))))
    (import (find-symbol "QUIT" p))
    (import (find-symbol "*ARGS*" p))))
