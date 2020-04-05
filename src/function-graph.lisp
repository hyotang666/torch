(in-package :torch)

(defun function-graph
       (function
        &key (ignore-privates *ignore-privates*) (type :png) system direction)
  #.(doc :torch "doc/function-graph.md")
  (let* ((graph
          (graph
            (or system
                (string-downcase
                  (package-name (symbol-package (ensure-name function)))))))
         (privates (privates graph))
         (*codes* (make-hash-table :test #'eq))
         (*ignore-privates* ignore-privates))
    (setup graph privates *codes*)
    (make-dot function type (list (gethash function *codes*)) direction)))