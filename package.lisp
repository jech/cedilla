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
    (:export "CEDILLA" "CEDILLA-MAIN" "*EXTERNAL-FORMAT-UTF8*")
    #+CLISP (:import-from "EXT" "*ARGS*" "QUIT")
    #+CCL (:import-from "CCL" "QUIT"))
