(in-package :torch)

(defun object-graph(object &key(type :png)direction)
  (let((namestring(filename(ensure-name object)type)))
    (cl-dot:dot-graph (apply #'cl-dot:generate-graph-from-roots
			     'object(list(find-class object))
			     (when direction
			       `((:rankdir ,(string direction)))))
		      namestring 
		      :format type)
    (pathname namestring)))

(defmethod cl-dot:graph-object-node((graph(eql 'object))(c class))
  (make-instance 'cl-dot:node
		 :attributes
		 (list :label (class-name c))))

(defvar *ignored-class* '(built-in-class standard-class standard-object standard-generic-function T structure-object slot-object structure-class))

(defun ignored-class-p(class)
  (find(class-name class)*ignored-class* :test #'string=))

(defmethod cl-dot:graph-object-points-to((graph(eql 'object))(c class))
  (unless(ignored-class-p c)
    (remove-if #'ignored-class-p (closer-mop:class-direct-subclasses c))))

(defmethod cl-dot:graph-object-knows-of((graph(eql 'object))(c class))
  (unless(ignored-class-p c)
    (remove-if #'ignored-class-p (closer-mop:class-direct-superclasses c))))
