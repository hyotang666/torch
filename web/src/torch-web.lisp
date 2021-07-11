(in-package :cl-user)

(defpackage :torch-web
  (:use :cl)
  (:export))

(in-package :torch-web)

(declaim (optimize speed))

;;;; TYPES

(deftype algorithm ()
  "Graph layout algorithms of Graphviz."
  '(member :dot :neato :twopi :circo :fdp :sfdp :patchwork :osage))

;;;; SPECIAL VARIABLES

(defvar *cookie* (cl-cookie:make-cookie-jar))

;;;; INTERMIDIATE OBJECTS.

(deftype http-method () '(member :get :post))

(defstruct uri
  (uri (error "URI is required.") :type string :read-only t)
  (method (error "METHOD is required.")
          :type (or http-method (unsigned-byte 16))
          :read-only t)
  (submethod nil :type (or null string)))

(defstruct (edge (:include uri)))

(defstruct (node (:include uri))
  (edges nil :type list ; of-type (or edge failed)
         ))

(defstruct (failed (:include uri)) message)

;;;; CONSTRUCTOR
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

(defun form-submethod (form)
  (loop :with inputs = (clss:select "input[name=method]" form)
        :for i :upfrom 0 :below (vector-length inputs)
        :thereis (plump:attribute (aref inputs i) "value")))

(defvar *interval* 2)

(declaim (type (unsigned-byte 8) *interval*))

(declaim
 (ftype (function (simple-string &optional (or null simple-string))
         (values list ; of-type (or edges failed)
                 &optional))
        uri-edges))

(defun uri-edges (uri &optional root)
  (uiop:format! *trace-output* "~%REQUEST: ~S" uri)
  (handler-case
      (dex:get (quri:render-uri (quri:merge-uris uri (or root "")))
               :cookie-jar *cookie*)
    (dex:http-request-failed (c)
      (list
        (make-failed :uri uri
                     :method (dex:response-status c)
                     :message (princ-to-string (type-of c)))))
    (usocket:unknown-error (c)
      (warn "Ignore ~S due to ~A" uri (prin1-to-string c)))
    (:no-error (body status header quri socket)
      (declare (ignore status quri socket))
      (let* ((content-type (gethash "content-type" header))
             (dom
              (when (uiop:string-prefix-p "text/html" content-type)
                (plump:parse body))))
        (if (not dom)
            (list (make-edge :uri uri :method :get))
            (uiop:while-collecting (acc)
              (loop :with forms := (clss:select "form" dom)
                    :for i :upfrom 0 :below (vector-length forms)
                    :for action := (plump:attribute (aref forms i) "action")
                    :when action
                      :do (acc
                           (make-edge :uri action
                                      :method (intern
                                                (string-upcase
                                                  (plump:attribute
                                                    (aref forms i) "method"))
                                                :keyword)
                                      :submethod (form-submethod
                                                   (aref forms i)))))
              (loop :with anchors = (clss:select "a" dom)
                    :for i :upfrom 0 :below (vector-length anchors)
                    :for href := (plump:attribute (aref anchors i) "href")
                    :when (and href
                               (not (equal "/" href))
                               (not (uiop:string-prefix-p "#" href)))
                      :do (acc (make-edge :uri href :method :get)))))))))

;;;; SITE-GRAPH

(defun internal-link-p (link root)
  (or (uiop:string-prefix-p "/" link) (uiop:string-prefix-p root link)))

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
               (dolist (edge (node-edges node))
                 (cond ((or (failed-p edge) (already-seen-p edge)) nil)
                       ((eq :post (edge-method edge))
                        (setf (gethash (edge-uri edge) known-nodes)
                                (make-node :uri (edge-uri edge)
                                           :method (edge-method edge)
                                           :edges nil)))
                       ((internal-link-p (edge-uri edge) uri)
                        (sleep *interval*)
                        (rec
                          (setf (gethash (edge-uri edge) known-nodes)
                                  (make-node :uri (edge-uri edge)
                                             :method (edge-method edge)
                                             :edges (uri-edges (edge-uri edge)
                                                               uri)))))
                       (t
                        (setf (gethash (edge-uri edge) known-nodes)
                                (make-node :uri (edge-uri edge)
                                           :method (edge-method edge)
                                           :edges nil)))))))
      (unless (find uri '("/" "#") :test #'equal)
        (rec
          (setf (gethash uri known-nodes)
                  (make-node :uri uri :method :get :edges (uri-edges uri))))))
    known-nodes))

;;;; CL-DOT

(defvar *site-table*)

(defmethod cl-dot:graph-object-node ((graph (eql 'web)) (node node))
  (make-instance 'cl-dot:node
                 :attributes (list :label (decode-uri (node-uri node)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'web)) (node node))
  (loop :for edge :in (node-edges node)
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
 (ftype (function
         (simple-string &key (:file simple-string)
          (:format (member . #.torch:+supported-formats+))
          (:direction (member :lr :rl :bt)) (:algorithm algorithm))
         (values pathname &optional))
        site-graph2))

(defun site-graph
       (uri &key (file "site-graph") (format :svg) direction (algorithm :sfdp))
  (let ((namestring (the simple-string (torch::filename file format)))
        (cl-dot:*dot-path* (which algorithm))
        (*site-table* (make-site-table uri)))
    (cl-dot:dot-graph
      (apply #'cl-dot:generate-graph-from-roots 'web
             (alexandria:hash-table-values *site-table*)
             (when direction
               `((:rankdir ,(string direction)))))
      namestring
      :format format)
    (pathname namestring)))

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
