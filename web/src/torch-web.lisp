(in-package :cl-user)

(defpackage :torch-web
  (:use :cl)
  (:export))

(in-package :torch-web)

(declaim (optimize speed))

;;;; For debug use.
;; To avoid redefine cl-dot print-object method if exists.

(unless (find-method #'print-object nil (list (find-class 'cl-dot:node) t) nil)
  (defmethod print-object ((node cl-dot:node) stream)
    (print-unreadable-object (node stream :type t)
      (let ((label (getf (cl-dot::attributes-of node) :label)))
        (funcall (formatter "~A") stream (or label (cl-dot::id-of node)))))))

;; To avoid redefine cl-dot print-object method if exists.

(unless (find-method #'print-object nil (list (find-class 'cl-dot::edge) t)
                     nil)
  (defmethod print-object ((edge cl-dot::edge) stream)
    (print-unreadable-object (edge stream :type t)
      (funcall (formatter "to ~A") stream
               (getf (cl-dot::attributes-of (cl-dot::target-of edge))
                     :label)))))

;;;; TYPES

(deftype algorithm ()
  "Graph layout algorithms of Graphviz."
  '(member :dot :neato :twopi :circo :fdp :sfdp :patchwork :osage))

;;;; SPECIAL VARIABLES

(defvar *cookie* (cl-cookie:make-cookie-jar))

(defvar *interval* 2)

(declaim (type (unsigned-byte 8) *interval*))

;;;; INTERMIDIATE OBJECTS.

(deftype http-method () '(member :get :post))

(defstruct uri
  (uri (error "URI is required.") :type string :read-only t)
  (method (error "METHOD is required.")
          :type (or http-method (unsigned-byte 16))
          :read-only t)
  (submethod nil :type (or null string)))

(defstruct (node (:include uri))
  (edges ; of-type (hash-table uri (or edge failed))
         (make-hash-table :test #'equal)
         :type hash-table))

(defstruct (edge (:include uri)))

(defstruct (failed (:include uri)) message)

(defun add (edge edges) (setf (gethash (uri-uri edge) edges) edge))

(defun decode-uri (uri)
  (handler-case (quri:url-decode uri)
    (error ()
      uri)))

;;;; SITE-TABLE
;;; CLHS does not guarantee AREF is the 'ONLY ONE' function that be able to ignore fill-pointer.
;;; 'ANY' functions and macros may ignore fill-pointer.

(declaim
 (ftype (function (vector)
         (values (integer 0 #.array-total-size-limit) &optional))
        vector-length))

(defun vector-length (vector)
  "Return vector length. Always interpret fill pointer if exists."
  (if (array-has-fill-pointer-p vector)
      (fill-pointer vector)
      (length vector)))

(defmacro do-selected ((var select) &body body)
  (let ((selected (gensym "SELECTED")) (i (gensym "INDEX")))
    `(loop :with ,selected := ,select
           :for ,i :upfrom 0 :below (vector-length ,selected)
           :for ,var := (aref ,selected ,i)
           :do (tagbody ,@body))))

(defun form-submethod (form)
  (do-selected (input (clss:select "input[name=method]" form))
    (let ((value (plump:attribute input "value")))
      (when value
        (return value)))))

(defparameter *ignore-external-links* nil "When true, bound root uri.")

(defun should-ignore-p (link)
  (or (uiop:string-prefix-p "#" link) ; fragment.
      (uiop:string-prefix-p "javascript" link)))

(declaim
 (ftype (function (simple-string &optional simple-string)
         (values hash-table ; of-type (hash-table uri (or edges failed))
                 &optional))
        uri-edges))

(defun uri-edges (uri &optional (root uri))
  (uiop:format! *trace-output* "~%REQUEST: ~S" uri)
  (let ((edges (make-hash-table :test #'equal)))
    (handler-case
        (dex:get (quri:render-uri (quri:merge-uris uri root))
                 :cookie-jar *cookie*)
      (dex:http-request-failed (c)
        (add
          (make-failed :uri uri
                       :method (dex:response-status c)
                       :message (princ-to-string (type-of c)))
          edges))
      (usocket:unknown-error (c)
        (warn "Ignore ~S due to ~A" uri (prin1-to-string c)))
      (:no-error (body status header quri socket)
        (declare (ignore status quri socket))
        (let* ((content-type (gethash "content-type" header))
               (html
                (when (uiop:string-prefix-p "text/html" content-type)
                  (plump:parse body))))
          (if (not html)
              (add (make-edge :uri uri :method :get) edges)
              (progn
               (do-selected (form (clss:select "form" html))
                 (let ((action (plump:attribute form "action")))
                   (when action
                     (add
                       (make-edge :uri action
                                  :method (intern
                                            (string-upcase
                                              (plump:attribute form "method"))
                                            :keyword)
                                  :submethod (form-submethod form))
                       edges))))
               (do-selected (anchor (clss:select "a" html))
                 (let ((href (plump:attribute anchor "href")))
                   (cond ((null href)) ; do nothing.
                         ((should-ignore-p href)) ; do nothing.
                         ((equal "/" href)
                          (add (make-edge :uri root :method :get) edges))
                         (t
                          (add (make-edge :uri href :method :get)
                               edges))))))))))
    edges))

(defun internal-link-p (link root)
  (or (uiop:string-prefix-p "/" link)
      (uiop:string-prefix-p "." link)
      (uiop:string-prefix-p root link)))

(declaim
 (ftype (function (simple-string &optional (or null cl-cookie:cookie-jar))
         (values hash-table ; as (hash-table simple-string node)
                 &optional))
        make-site-table))

(defun make-site-table (uri &optional cookie)
  (let ((*cookie* (or cookie *cookie*))
        (known-nodes (make-hash-table :test #'equal)))
    (labels ((already-seen-p (edge)
               (gethash (edge-uri edge) known-nodes))
             (rec (node)
               (loop :for edge :being :each :hash-value :of (node-edges node)
                     :if (or (failed-p edge) (already-seen-p edge))
                       :do '#:nothing
                     :else :if (eq :post (edge-method edge))
                       :do (setf (gethash (edge-uri edge) known-nodes)
                                   (make-node :uri (edge-uri edge)
                                              :method (edge-method edge)))
                     :else :if (internal-link-p (edge-uri edge) uri)
                       :do (sleep *interval*)
                           (rec
                             (setf (gethash (edge-uri edge) known-nodes)
                                     (make-node :uri (edge-uri edge)
                                                :method (edge-method edge)
                                                :edges (uri-edges
                                                         (edge-uri edge) uri))))
                     :else
                       :do (setf (gethash (edge-uri edge) known-nodes)
                                   (make-node :uri (edge-uri edge)
                                              :method (edge-method edge))))))
      (unless (find uri '("/" "#") :test #'equal)
        (rec
          (setf (gethash uri known-nodes)
                  (make-node :uri uri :method :get :edges (uri-edges uri))))))
    known-nodes))

;;;; QUERIES

(declaim
 (ftype (function (cl-dot::attributes-mixin) (values simple-string &optional))
        label-of))

(defun label-of (attributes-mixin)
  (getf (cl-dot::attributes-of attributes-mixin) :label ""))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is used in read time. EVAL-WHEN is needed.
  (defconstant string-class
    ;; To infer type for optimization, sbcl needs specializer as simple-string but
    ;; CLHS does not say simple-string is a class.
    '#.(if (find-class 'simple-string nil)
           'simple-string
           'string)))

(defgeneric who-refs (uri repository)
  (:method ((uri #.string-class) (table hash-table))
    (loop :for node-uri :being :each :hash-key :of table :using
               (:hash-value node)
          :if (gethash uri (node-edges node))
            :collect node))
  (:method ((uri #.string-class) (graph cl-dot::graph))
    (loop :for edge :in (cl-dot::edges-of graph)
          :for target := (cl-dot::target-of edge)
          :if (equal uri (label-of target))
            :collect (cl-dot::source-of edge) :into acc
          :finally (return
                    (delete-duplicates acc :key #'label-of :test #'equal)))))

(defgeneric external-links (root repository)
  (:method ((root #.string-class) (table hash-table))
    (loop :for node-uri :being :each :hash-key :of table :using
               (:hash-value node)
          :if (not (internal-link-p node-uri root))
            :collect node))
  (:method ((root #.string-class) (graph cl-dot::graph))
    (loop :for node :in (cl-dot::nodes-of graph)
          :if (not (internal-link-p (label-of node) root))
            :collect node)))

(defgeneric resource-links (root repository)
  (:method ((root #.string-class) (table hash-table))
    ;; KLUDGE: Should I introduce RESOURCE object?
    (loop :for node-uri :being :each :hash-key :of table :using
               (:hash-value node)
          :if (and (= 1 (hash-table-count (node-edges node)))
                   (internal-link-p node-uri root))
            :collect node))
  (:method ((hint #.string-class) (graph cl-dot::graph))
    (loop :for node :in (cl-dot::nodes-of graph)
          :if (search hint (label-of node))
            :collect node)))

;;;; CL-DOT

(defvar *site-table*)

(defmethod cl-dot:graph-object-node ((graph (eql 'web)) (node node))
  (make-instance 'cl-dot:node
                 :attributes (list :label (decode-uri (node-uri node)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'web)) (node node))
  (loop :for edge :being :each :hash-value :of (node-edges node)
        :if (or (not *ignore-external-links*)
                (internal-link-p (uri-uri edge) *ignore-external-links*))
          :collect (gethash (uri-uri edge) *site-table*)))

(defun which (algorithm)
  (let* ((search (format nil "~(~A~)" algorithm))
         (command (format nil "which ~A" search)))
    (multiple-value-bind (exe message status)
        (uiop:run-program command
                          :ignore-error-status t
                          :output '(:string :stripped t))
      (declare (ignore message))
      (case status
        (0 exe)
        (1 (error "~S is nonexistent or not executable." search))
        (2 (error "Invalid option is specified. ~S" command))
        (otherwise (error "Internal logical error. NIY."))))))

(declaim
 (ftype (function (hash-table &key (:direction torch:direction))
         (values cl-dot::graph &optional))
        graph<-table))

(defun graph<-table (table &key direction)
  (setf *site-table* table)
  (cl-dot:generate-graph-from-roots 'web
                                    (loop :for node :being :each
                                               :hash-value :of table :using
                                               (:hash-key uri)
                                          :if (or (not *ignore-external-links*)
                                                  (internal-link-p uri
                                                                   *ignore-external-links*))
                                            :collect node)
                                    `(:concentrate t
                                      ,@(when direction
                                          `(:rankdir ,(string direction))))))

(defmacro with-dot-syntax
          ((namestring
            &key
            (file "site-graph")
            (format :svg)
            direction
            (algorithm :sfdp)
            internal)
           &body body)
  (let ((vdir (gensym "DIRECTION")))
    `(let* ((,vdir ,direction)
            (,namestring (torch::filename ,file ,format))
            (*ignore-external-links* ,internal)
            (cl-dot:*dot-path*
             (if ,vdir
                 (which ,algorithm)
                 cl-dot:*dot-path*))
            (cl-dot:*neato-path*
             (if ,vdir
                 cl-dot:*neato-path*
                 (which ,algorithm))))
       ,@body
       (probe-file ,namestring))))

(declaim
 (ftype (function
         (simple-string &key (:file simple-string) (:format torch:file-format)
          (:direction torch:direction) (:algorithm algorithm)
          (:external-link boolean))
         (values pathname &optional))
        site-graph))

(defun site-graph
       (uri
        &key (file "site-graph") (format :svg) direction (algorithm :sfdp)
        (external-link t))
  (with-dot-syntax (namestring :file file
                               :format format
                               :direction direction
                               :algorithm algorithm
                               :internal (and (not external-link) uri))
    (cl-dot:dot-graph (graph<-table (make-site-table uri) :direction direction)
                      namestring
                      :directed direction
                      :format format)))

;;;; SAVE/LOAD -TABLE

(defun save-table (table &optional (pathname "site-table"))
  (cl-store:store table pathname))

(defun load-table (&optional (pathname "site-table"))
  (cl-store:restore pathname))

;;;; PRINT-DOT for debug use.

(defun print-dot (table &key direction)
  (cl-dot:print-graph (graph<-table table) :directed direction))

;;;; PRINT-LABELS
;; Especially for [graph_editor](ttps://csacademy.com/app/graph_editor/)

(defun print-labels (table)
  (loop :for uri :being :each :hash-key :of table
        :do (funcall (formatter "~S~%") *standard-output* (decode-uri uri)))
  (loop :for uri :being :each :hash-key :of table :using (:hash-value node)
        :do (loop :for edge :being :each :hash-value :of (node-edges node)
                  :do (funcall (formatter "~S ~S~%") *standard-output*
                               (decode-uri uri) (decode-uri (uri-uri edge))))))

;;;; WITH-COOKIE

(defmacro with-cookie (&body clauses)
  `(let ((*cookie* (cl-cookie:make-cookie-jar)))
     ,@(mapcar
         (lambda (clause)
           (let ((request
                  `(,(uiop:find-symbol* (string-upcase (car clause)) :dex)
                    ,(cadr clause) :cookie-jar *cookie*)))
             (if (not (eq :action (third clause)))
                 request
                 `(let* ((forms (clss:select "form" (plump:parse ,request)))
                         (form (elt forms 0))
                         (params ',(nthcdr 3 clause)))
                    (assert (= 1 (length forms))) ; TODO RESTART-CASE.
                    (funcall
                      (uiop:find-symbol*
                        (string-upcase (plump:attribute form "method")) :dex)
                      (quri:merge-uris (plump:attribute form "action")
                                       ,(cadr clause))
                      :cookie-jar *cookie*
                      :content (loop :for input
                                          :across (clss:select "input" form)
                                     :for name := (plump:attribute input "name")
                                     :for value
                                          := (plump:attribute input "value")
                                     :for specified
                                          := (getf params
                                                   (intern (string-upcase name)
                                                           :keyword))
                                     :if specified
                                       :collect (cons name specified)
                                     :else :if (and name value)
                                       :collect (cons name value)))))))
         clauses)
     *cookie*))

#++
(site-graph "http://localhost:5000/"
            (with-cookie
              (:get "http://localhost:5000/")
              (:get "http://localhost:5000/login" :action :name "name-A"
               :password "pass-A")))

#++
(site-graph "http://localhost:5000/")
