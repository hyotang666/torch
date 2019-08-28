(in-package :torch)

(defvar *builtin-hook* *macroexpand-hook*)

(defvar *forms*)

(defun macroexpand-hook(expander form environment)
  (catch :do-nothing
	 (cond
	   ((string= :defun(car form))
	    (typecase(cadr form)
	      (atom(pushnew(form form)*forms* :test #'equal))
	      (list(pushnew (form form) *forms* :test #'equal))))
	   ((find(car form)'(defmacro):test #'string=)
	    (pushnew(form form)*forms* :test #'equal))
	   ((string= :defmethod(car form))
	    (when(eq  *package* (symbol-package(cadr form)))
	      (pushnew(form form)*forms* :test #'equal)))
	   ((string= :defgeneric(car form))
	    (loop :for option :in (cdddr form)
		  :when(eq :method (car option))
		  :do(pushnew (form(cddr option)) *forms* :test #'equal)))))
  (funcall *builtin-hook* expander form environment))

(defun form(form)
  (or (remove-if (complement(conjoin #'symbolp #'target-symbolp))
		 (delete-duplicates(flatten form)
		   :from-end t))
      (throw :do-nothing nil)))

(defun target-symbolp(symbol)
  (and (fboundp symbol)
       (eq *package* (symbol-package symbol))))

(defun graph(system)
  (loop :for system :in (asdf:system-depends-on(asdf:find-system system))
	:with loaded-systems = (asdf:already-loaded-systems)
	:unless (and (not(listp system)) ; ignoring (:version ...)
		     (find system loaded-systems :test #'string-equal))
	:collect system :into result
	:finally (when result (ql:quickload result :silent t)))
  (let((*compile-verbose* nil)
       (*compile-print* nil)
       (*load-verbose* nil)
       (*load-print* nil))
    (handler-bind((warning #'muffle-warning))
      (let((*macroexpand-hook* #'macroexpand-hook)
	   *forms*)
	(asdf:load-system system :force t)
	*forms*))))