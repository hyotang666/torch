(in-package :torch)

(defun package-graph (package &key (type :png) direction)
  #.(doc :torch "doc/package-graph.md")
  (let ((namestring (filename (ensure-name package) type)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'package
             (list (uiop:find-package* package))
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format type)
    (pathname namestring)))

(defmethod cl-dot:graph-object-node ((graph (eql 'package)) (p package))
  (make-instance 'cl-dot:node :attributes (list :label (package-name p))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'package)) (p package))
  (remove #.(find-package :cl) (package-use-list p)))