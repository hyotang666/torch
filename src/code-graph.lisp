(in-package :torch)

(defun code-graph
       (system
        &key ((:ignore-privates *ignore-privates*) *ignore-privates*)
        ((:ignore-standalone *ignore-standalone*) *ignore-standalone*)
        ((:split *split*) *split*) (type :png) direction package)
  #.(doc :torch "doc/code-graph.md")
  (let* ((graph (graph system)) (*codes* (setup graph (privates graph))))
    (if (not *split*)
        (make-dot system type
                  (if *ignore-standalone*
                      (remove-if #'standalone-p
                                 (alexandria:hash-table-values *codes*))
                      (alexandria:hash-table-values *codes*))
                  direction)
        (if *ignore-standalone*
            (loop :for symbol :being :each :external-symbol :in
                       (or package system)
                  :for code = (gethash symbol *codes*)
                  :unless (standalone-p code)
                    :collect (make-dot symbol type (list code) direction))
            (loop :for symbol :being :each :external-symbol :in
                       (or package system)
                  :when (fboundp symbol)
                    :collect (make-dot symbol type (gethash symbol *codes*)
                                       direction))))))

(defun standalone-p (code)
  (typecase code (symbol (fboundp code)) (code (null (code-commons code)))))

(defun privates (graph)
  (loop :for node :in graph
        :when (= 1
                 (count (car node) graph
                        :test (lambda (subject edges)
                                (find subject edges :test #'eq))
                        :key #'cdr))
          :collect (car node)))

;; debug use.

(defun print-graph
       (system
        &key ((:ignore-privates *ignore-privates*) *ignore-privates*)
        ((:ignore-standalone *ignore-standalone*) *ignore-standalone*)
        ((:split *split*) *split*) direction package)
  (let* ((graph (graph system)) (*codes* (setup graph (privates graph))))
    (if (not *split*)
        (cl-dot:print-graph
          (apply #'cl-dot:generate-graph-from-roots 'code
                 (if *ignore-standalone*
                     (remove-if #'standalone-p
                                (alexandria:hash-table-values *codes*))
                     (alexandria:hash-table-values *codes*))
                 (when direction
                   `((:rankdir ,(string direction))))))
        (if *ignore-standalone*
            (loop :for symbol :being :each :external-symbol :in
                       (or package system)
                  :for code = (gethash symbol *codes*)
                  :unless (standalone-p code)
                    :do (cl-dot:print-graph
                          (apply #'cl-dot:generate-graph-from-roots 'code
                                 (list code)
                                 (when direction
                                   `((:rankdir ,(string direction)))))))
            (loop :for symbol :being :each :external-symbol :in
                       (or package system)
                  :when (fboundp symbol)
                    :do (cl-dot:print-graph
                          (apply #'cl-dot:generate-graph-from-roots 'code
                                 (gethash symbol *codes*)
                                 (when direction
                                   `((:rankdir ,(string direction)))))))))))

(defun make-dot (name type objects direction)
  (let ((namestring (filename name type)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'code objects
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format type)
    (pathname namestring)))

(defun setup (graph privates &optional (*codes* (make-hash-table :test #'eq)))
  (dolist (node graph *codes*)
    (let ((my-privates (intersection (cdr node) privates))
          (code (gethash (car node) *codes*)))
      (if code ; it is method.
          (setf (code-privates code) (union (code-privates code) my-privates)
                (code-commons code)
                  (union (code-commons code)
                         (set-difference (cdr node) my-privates)))
          (setf (gethash (car node) *codes*)
                  (make-code :name (car node)
                             :privates my-privates
                             :commons (set-difference (cdr node)
                                                      my-privates)))))))