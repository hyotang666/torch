(in-package :torch)

(defstruct (code (:copier nil) (:predicate nil))
  (name (error "CODE-NAME is required") :type symbol :read-only t)
  (commons nil :type list)
  (privates nil :type list))

(defmethod print-object ((c code) *standard-output*)
  (if *print-readably*
      (call-next-method)
      (if *print-escape*
          (print-unreadable-object (c *standard-output*)
            (format t "~A ~:A" (code-name c) (code-commons c)))
          (call-next-method))))