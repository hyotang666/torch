(in-package :torch)

(defun file-graph (system &key (type :png) direction)
  #.(doc :torch "doc/file-graph.md")
  (let ((namestring (filename (ensure-name system) type)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'file (files system)
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format type)
    (pathname namestring)))

(defun files (system) (asdf:component-children (asdf:find-system system)))

(defmethod cl-dot:graph-object-node
           ((graph (eql 'file)) (node asdf:cl-source-file))
  (make-instance 'cl-dot:node
                 :attributes (list :label (asdf:component-name node))))

(defmethod cl-dot:graph-object-points-to
           ((graph (eql 'file)) (node asdf:cl-source-file))
  (loop :for dep :in (asdf:component-sideway-dependencies node)
        :collect (gethash dep
                          (asdf:component-children-by-name
                            (asdf:component-parent node)))))