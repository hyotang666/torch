(in-package :cl-user)

(defpackage :torch
  (:use :cl)
  (:export ;;;; primary api
           #:code-graph
           #:system-graph
           #:package-graph
           #:file-graph
           #:function-graph
           #:object-graph
           ;; variable
           #:*ignore-privates*
           #:*ignore-standalone*
           #:*split*
           #:*invalid-chars*
           ;; TYPEs
           #:file-format
           #:direction
           ;; constant
           #:+supported-formats+))

(in-package :torch)

(declaim (optimize speed))

;;; helpers

(declaim (type list *invalid-chars*))

(defvar *invalid-chars* '(#\* #\? #\/ #\\ #\Space))

(declaim (ftype (function (character) (values t &optional)) invalid-charp))

(defun invalid-charp (c) (find c *invalid-chars*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DEFTYPE FILE-FORMAT below needs this eval-when.
  (defun supported-format-string ()
    (flet ((dot ()
             (uiop:run-program "dot -T?"
                               :ignore-error-status t
                               :error-output :string)))
      (second (uiop:split-string (nth-value 1 (dot)) :separator ":" :max 2))))
  (defun supported-format ()
    (let ((*package* (find-package :keyword)))
      (with-input-from-string (s (supported-format-string))
        (loop :for k = (read s nil nil)
              :while k
              :collect k))))
  (unless (boundp '+supported-formats+)
    (defconstant +supported-formats+ (supported-format))))

(deftype file-format () `(member ,@+supported-formats+))

(declaim (ftype (function (simple-string) simple-string) safety-name))

(defun safety-name (name)
  (with-output-to-string (*standard-output*)
    (loop :for c :across name
          :do (if (invalid-charp c)
                  (progn (write-char #\\) (write-char c))
                  (write-char c)))))

(defun filename (name type)
  (format nil "~(~A~).~(~A~)" (safety-name name) type))

(declaim (ftype (function (t) (values simple-string &optional)) ensure-name))

(defun ensure-name (obj)
  (etypecase obj
    (string obj)
    (symbol (symbol-name obj))
    (package (package-name obj))
    (asdf:component (asdf:component-name obj))
    (function (symbol-name (millet:function-name obj)))))

(deftype direction () '(member nil :lr :rl :tb :bt))

;;; general utilities.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DOC is used in read time.
  (defun doc (system path)
    (uiop:read-file-string
      (uiop:subpathname
        (asdf:system-source-directory (asdf:find-system system)) path))))

;;;; SYSTEM-GRAPH.

(declaim
 (ftype (function
         ((or symbol asdf:system) &key (:type file-format)
          (:direction direction))
         (values pathname &optional))
        system-graph))

(defun system-graph (system &key (type :png) direction)
  #.(doc :torch "doc/system-graph.md")
  (let ((namestring (filename (ensure-name system) type)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'system
             (list (asdf:find-system system))
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format type)
    (pathname namestring)))

(defmethod cl-dot:graph-object-node ((graph (eql 'system)) (s asdf:system))
  (make-instance 'cl-dot:node
                 :attributes (list :label (asdf:component-name s))))

(defmethod cl-dot:graph-object-points-to
           ((graph (eql 'system)) (s asdf:system))
  (loop :for system
             :in (append (asdf:system-defsystem-depends-on s)
                         (asdf:system-depends-on s))
        :unless (listp system)
          :collect (asdf:find-system system)))

;;;; PACKAGE-GRAPH.

(declaim
 (ftype (function
         ((or symbol string package) &key (:type file-format)
          (:direction direction))
         (values pathname &optional))
        package-graph))

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

;;;; FILE-GRAPH.

(defun files (system) (asdf:component-children (asdf:find-system system)))

(declaim
 (ftype (function
         ((or symbol string asdf:system) &key (:type file-format)
          (:direction direction))
         (values pathname &optional))
        file-graph))

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

;;;; OBJECT-GRAPH.

(declaim
 (ftype (function (t &key (:type file-format) (:direction direction))
         (values pathname &optional))
        object-graph))

(defun object-graph (object &key (type :png) direction)
  (let ((namestring (filename (ensure-name object) type)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'object
             (list (find-class object))
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format type)
    (pathname namestring)))

(defmethod cl-dot:graph-object-node ((graph (eql 'object)) (c class))
  (make-instance 'cl-dot:node :attributes (list :label (class-name c))))

(declaim (type list *ignored-class*))

(defvar *ignored-class*
  '(built-in-class standard-class standard-object standard-generic-function t
    structure-object slot-object structure-class))

(defun ignored-class-p (class)
  (find (the symbol (class-name class)) *ignored-class* :test #'string=))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'object)) (c class))
  (unless (ignored-class-p c)
    (remove-if #'ignored-class-p (closer-mop:class-direct-subclasses c))))

(defmethod cl-dot:graph-object-knows-of ((graph (eql 'object)) (c class))
  (unless (ignored-class-p c)
    (remove-if #'ignored-class-p (closer-mop:class-direct-superclasses c))))

;;;; CODE-GRAPH.
;;; specials.

(defvar *codes*)

(defparameter *ignore-privates* nil)

(defparameter *ignore-standalone* nil)

(defparameter *split* nil)

;;; objects.

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

;;; GRAPH.

(declaim (type function *builtin-hook*))

(defvar *builtin-hook* (coerce *macroexpand-hook* 'function))

(defvar *forms*)

(declaim (type list *packages*))

(defvar *packages*)

(defun target-symbolp (symbol)
  (declare (optimize (speed 1)))
  (and (fboundp symbol)
       (find (package-name (symbol-package symbol)) *packages*
             :test #'string=)))

(defun form (form)
  (labels ((flatten (form)
             (mapcan
               (lambda (elt)
                 (cond #+sbcl
                       ((sb-int:comma-p elt) (flatten (sb-int:comma-expr elt)))
                       (t (list elt))))
               (alexandria:flatten form))))
    (or (remove-if
          (locally
           (declare (optimize (speed 1)))
           (complement (alexandria:conjoin #'symbolp #'target-symbolp)))
          (delete-duplicates (flatten form) :from-end t))
        (throw :do-nothing nil))))

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

(defun graph (system)
  (loop :for system :in (asdf:system-depends-on (asdf:find-system system))
        :with loaded-systems :of-type list = (asdf:already-loaded-systems)
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

;;;; CODE-GRAPH.

(defun external-symbolp (symbol)
  (eq :external (nth-value 1
                           (find-symbol (symbol-name symbol)
                                        (symbol-package symbol)))))

(defun symbol-names-file-p (symbol)
  (let ((system
         (asdf:find-system
           (string-downcase (package-name (symbol-package symbol))) nil)))
    (when system
      (values (gethash (string-downcase (symbol-name symbol))
                       (asdf:component-children-by-name system))))))

(defmethod cl-dot:graph-object-node ((graph (eql 'code)) (object code))
  (flet ((label (object)
           (substitute #\Newline #\Space
                       (format nil "~A~@[ ~A~]" (code-name object)
                               (when *ignore-privates*
                                 (code-privates object)))))
         (color (name)
           (cond ((external-symbolp name) :red)
                 ((symbol-names-file-p name) :blue)
                 (t :black))))
    (make-instance 'cl-dot:node
                   :attributes (list :label (label object)
                                     :shape :box
                                     :color (color (code-name object))))))

(defun edges (code)
  (append
    (loop :for common :in (code-commons code)
          :collect (or (gethash common *codes*) common))
    (unless *ignore-privates*
      (loop :for private :in (code-privates code)
            :collect (or (gethash private *codes*) private)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'code)) (object code))
  (edges object))

(defmethod cl-dot:graph-object-node ((graph (eql 'code)) (o symbol))
  (make-instance 'cl-dot:node
                 :id (format nil "~A~%(Accessor?)" o)
                 :attributes (list :color (if (external-symbolp o)
                                              :red
                                              :black))))

(defun standalone-p (code)
  (typecase code (symbol (fboundp code)) (code (null (code-commons code)))))

(defun privates (graph)
  (loop :for node :in graph
        :when (= 1
                 (count (car node) graph
                        :test (lambda (subject edges)
                                (declare (type list edges))
                                (find subject edges :test #'eq))
                        :key #'cdr))
          :collect (car node)))

(declaim
 (ftype (function ((or symbol string) file-format list direction)
         (values pathname &optional))
        make-dot))

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

(defun code-graph
       (system
        &key ((:ignore-privates *ignore-privates*) *ignore-privates*)
        ((:ignore-standalone *ignore-standalone*) *ignore-standalone*)
        ((:split *split*) *split*) (type :png) direction package)
  #.(doc :torch "doc/code-graph.md")
  (let* ((graph (graph system)) (*codes* (setup graph (privates graph))))
    (if (not *split*)
        (make-dot (asdf:coerce-name system) type
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

;;;; FUNCTION-GRAPH.

(defun function-graph
       (function
        &key (ignore-privates *ignore-privates*) (type :png) system direction)
  #.(doc :torch "doc/function-graph.md")
  (let* ((graph
          (graph
            (or system
                (string-downcase
                  (package-name
                    (symbol-package (millet:function-name function)))))))
         (privates (privates graph))
         (*codes* (make-hash-table :test #'eq))
         (*ignore-privates* ignore-privates))
    (setup graph privates *codes*)
    (make-dot function type (list (gethash function *codes*)) direction)))

;; debug use.

(declaim
 (ftype (function
         ((or symbol string asdf:system) &key (:ignore-privates boolean)
          (:ignore-standalone boolean) (:split boolean) (:direction direction)
          (:package (or symbol string package)))
         (values null &optional))
        print-graph))

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