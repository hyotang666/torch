(in-package :torch)

(defun system-graph(system &key(type :png)direction)
  #.(doc :torch "doc/system-graph.md")
  (let((namestring(filename(ensure-name system)type)))
    (cl-dot:dot-graph (apply #'cl-dot:generate-graph-from-roots
			     'system(list(asdf:find-system system))
			     (when direction
			       `((:rankdir ,(string direction)))))
		      namestring 
		      :format type)
    (pathname namestring)))

(defmethod cl-dot:graph-object-node((graph(eql 'system))(s asdf:system))
  (make-instance 'cl-dot:node
		 :attributes
		 (list :label (asdf:component-name s))))

(defmethod cl-dot:graph-object-points-to((graph(eql 'system))(s asdf:system))
  (loop :for system :in (append (asdf:system-defsystem-depends-on s)
				(asdf:system-depends-on s))
	:unless (listp system)
	:collect (asdf:find-system system)))
