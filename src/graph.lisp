(in-package :torch)

(defvar *builtin-hook* *macroexpand-hook*)

(defvar *forms*)

(defvar *packages*)

(defun macroexpand-hook (expander form environment)
  (catch :do-nothing
    (cond
     ((typep form '(cons (eql defun) *))
      (typecase (cadr form)
        (atom (pushnew (form form) *forms* :test #'equal))
        (list (pushnew (form form) *forms* :test #'equal))))
     ((typep form '(cons (eql defmacro) *))
      (pushnew (form form) *forms* :test #'equal))
     ((typep form '(cons (eql defmethod) *))
      (when (eq *package* (symbol-package (cadr form)))
        (pushnew (form form) *forms* :test #'equal)))
     ((typep form '(cons (eql defpackage) *)) (push (second form) *packages*))
     ((typep form '(cons (eql defgeneric) *))
      (loop :for option :in (cdddr form)
            :when (eq :method (car option))
              :do (pushnew (form (cddr option)) *forms* :test #'equal)))))
  (funcall *builtin-hook* expander form environment))

(defun form (form)
  (labels ((flatten (form)
             (mapcan
               (lambda (elt)
                 (cond #+sbcl
                       ((sb-int:comma-p elt) (flatten (sb-int:comma-expr elt)))
                       (t (list elt))))
               (alexandria:flatten form))))
    (or (remove-if (complement (alexandria:conjoin #'symbolp #'target-symbolp))
                   (delete-duplicates (flatten form) :from-end t))
        (throw :do-nothing nil))))

(defun target-symbolp (symbol)
  (and (fboundp symbol)
       (find (package-name (symbol-package symbol)) *packages*
             :test #'string=)))

(defun graph (system)
  (loop :for system :in (asdf:system-depends-on (asdf:find-system system))
        :with loaded-systems = (asdf:already-loaded-systems)
        :unless (and (not (listp system)) ; ignoring (:version ...)
                     (find system loaded-systems :test #'string-equal))
          :collect system :into result
        :finally (when result
                   (ql:quickload result :silent t)))
  (let ((*compile-verbose* nil)
        (*compile-print* nil)
        (*load-verbose* nil)
        (*load-print* nil))
    (handler-bind ((warning #'muffle-warning))
      (let ((*macroexpand-hook* #'macroexpand-hook) *forms* *packages*)
        (asdf:load-system system :force t)
        *forms*))))