(in-package :torch)

(defvar *codes*)

(defparameter *ignore-privates* nil)

(defparameter *ignore-standalone* nil)

(defparameter *split* nil)

(defun supported-format ()
  (let ((*package* (find-package :keyword)))
    (with-input-from-string (s (supported-format-string))
      (loop :for k = (read s nil nil)
            :while k
            :collect k))))

(defun supported-format-string ()
  (flet ((dot ()
           (uiop:run-program "dot -T?"
                             :ignore-error-status t
                             :error-output :string)))
    (second (uiop:split-string (nth-value 1 (dot)) :separator ":" :max 2))))

(unless (boundp '+supported-formats+)
  (defconstant +supported-formats+ (supported-format)))