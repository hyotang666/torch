(in-package :cl-user)

(defpackage :torch
  (:use :cl :predcase)
  (:export ; primary api
           #:code-graph ; api
           #:system-graph
           #:package-graph
           #:file-graph
           #:function-graph
           #:object-graph ; variable
           #:*ignore-privates*
           #:*ignore-standalone*
           #:*split*
           #:*invalid-chars* ; constant
           #:+supported-formats+))

(in-package :torch)

;;; helpers

(defun filename (name type)
  (format nil "~(~A~).~(~A~)" (safety-name name) type))

(defun safety-name (name)
  (with-output-to-string (*standard-output*)
    (loop :for c :across (string name)
          :do (if (invalid-charp c)
                  (progn (write-char #\\) (write-char c))
                  (write-char c)))))

(defvar *invalid-chars* '(#\* #\? #\/ #\\ #\Space))

(defun invalid-charp (c) (find c *invalid-chars*))

(defun ensure-name (obj)
  (typecase obj
    ((or symbol string) obj)
    (package (package-name obj))
    (asdf:component (asdf:component-name obj))
    (function (millet:function-name obj))))

;;; general utilities.

(defun doc (system path)
  (uiop:read-file-string
    (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
                      path)))
