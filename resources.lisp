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

(defun identify-resource (filename)
  (handler-bind
      ((error
        #'(lambda (c)
            (warn "Unable to identify resource ~A: ~A"
                  filename c)
            (return-from identify-resource nil))))
    (let ((filename (find-file-with-path filename *resources-path*)))
      (when (null filename)
        (error 'file-error))
      (with-open-file (in filename :element-type '(unsigned-byte 8))
        (let* ((b0 (read-byte in nil nil))
               (b1 (and b0 (read-byte in nil nil)))
               (binary
                (cond
                  ((and (eql b0 #x80) (eql b1 1)) t)
                  ((and (eql b0 (char-code #\%)) (eql b1 (char-code #\!)))
                   nil)
                  (t (error "Unknown file type"))))
               (len
                (if binary
                    (let ((b2 (read-byte in nil nil))
                          (b3 (read-byte in nil nil))
                          (b4 (read-byte in nil nil))
                          (b5 (read-byte in nil nil)))
                      (+ b2 (ash b3 8) (ash b4 16) (ash b5 24)))
                    #x10000))
               (line (make-array 80 :element-type 'character
                                 :fill-pointer 0 :adjustable t)))
          (unless binary
            (vector-push-extend #\% line)
            (vector-push-extend #\! line))
          (when (< len 1) (return-from identify-resource nil))
          (do* ((b (read-byte in) (read-byte in))
                (c (code-char b) (code-char b))
                (i 0 (+ i 1)))
               ((or (>= i len) (member c '(#\Newline #\Return))))
            (vector-push-extend c line))
          (let* ((first-space (position #\Space line))
                 (second-space 
                  (and first-space
                       (position #\Space line :start (+ first-space 1))))
                 (first-word (subseq line 0 first-space))
                 (second-word (and first-space
                                   (subseq line 
                                           (+ first-space 1) second-space))))
            (let (type name)
              (cond
                ((and
                  (>= (length first-word) 15)
                  (equal (subseq first-word 0 15) "%!PS-AdobeFont-"))
                 (setf type :font name second-word))
                (t (error "Unknown resource type ~A" first-word)))
              (and type
                   (if binary
                       (make-pfb-ps-resource :type :font
                                             :name second-word
                                             :filename filename)
                       (make-file-ps-resource :type :font
                                              :name second-word
                                              :filename filename))))))))))

(defun resource-equivalent-p (r1 r2)
  (and (eql (ps-resource-type r1) (ps-resource-type r2))
       (equal (ps-resource-name r1) (ps-resource-name r2))))

(defmethod download-resource ((resource required-ps-resource) out)
  (case (ps-resource-type resource)
    (:font
     (format out "%%IncludeFont: ~A~%" (ps-resource-name resource)))
    (t
     (if (eql (ps-resource-type resource) ':procset)
         (format out "%%IncludeResource: ~(~A~) ~A~@[ ~A~]~@[ ~A~]~%"
                 (ps-resource-type resource)
                 (ps-resource-name resource)
                 (ps-resource-version resource)
                 (ps-resource-release resource))
         (format out "%%IncludeResource: ~(~A~) ~A~%"
                 (ps-resource-type resource)
                 (ps-resource-name resource))))))

(defmethod download-resource :before ((resource provided-ps-resource) out)
  (case (ps-resource-type resource)
    (:font
     (format out "%%BeginFont: ~A~%" (ps-resource-name resource)))
    (t
     (if (eql (ps-resource-type resource) ':procset)
         (format out "%%BeginResource: ~(~A~) ~A~@[ ~A~]~@[ ~A~]~%"
                 (ps-resource-type resource)
                 (ps-resource-name resource)
                 (or (ps-resource-version resource) "0.0")
                 (or (ps-resource-release resource) "0"))
         (format out "%%BeginResource: ~(~A~) ~A~%"
                 (ps-resource-type resource)
                 (ps-resource-name resource))))))

(defmethod download-resource :after ((resource provided-ps-resource) out)
    (case (ps-resource-type resource)
      (:font
       (format out "%%EndFont~%"))
      (t
       (format out "%%EndResource~%"))))

(defmethod download-resource ((resource file-ps-resource) out)
  (with-open-file (in (ps-resource-filename resource))
    (loop
     (let ((char (read-char in nil in)))
       (when (eql char in)
         (return nil))
       (write-char char out)))))

(defmethod download-resource ((resource pfb-ps-resource) out)
  (with-open-file (in (ps-resource-filename resource)
                      :element-type '(unsigned-byte 8))
    (loop
     (let* ((b0 (read-byte in nil in))
            (b1 (and b0 (read-byte in nil in)))
            (b2 (and b1 (read-byte in nil in)))
            (b3 (and b2 (read-byte in nil in)))
            (b4 (and b3 (read-byte in nil in)))
            (b5 (and b4 (read-byte in nil in))))
       (when (eql b1 3) (return-from download-resource nil))
       (unless (and b5 (eql b0 #x80)) (error "Incorrect PFB file"))
       (let ((len (+ b2 (ash b3 8) (ash b4 16) (ash b5 24))))
         (ecase b1
           (1 (pfb-text-section in out len))
           (2 (pfb-binary-section in out len))))))))

(defun pfb-text-section (in out len)
  (loop for i from 0 upto (- len 1)
        do (write-char (code-char (read-byte in)) out)))

(defun pfb-binary-section (in out len)
  (let ((h "0123456789ABCDEF"))
    ;; FORMAT is too slow
    (flet ((write-hex (code)
             (let ((c1 (aref h (floor code 16)))
                   (c2 (aref h (mod code 16))))
               (write-char c1 out)
               (write-char c2 out))))
      (loop for i from 0 upto (- len 1)
            for b = (read-byte in)
            when (and (> i 0) (= 0 (mod i 32)))
            do (write-char #\Newline out)
            do (write-hex b)))))

(defmethod download-resource ((resource string-ps-resource) out)
  (format out (ps-resource-value resource)))
