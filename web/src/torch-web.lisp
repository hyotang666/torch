(in-package :cl-user)

(defpackage :torch-web
  (:use :cl)
  (:export))

(in-package :torch-web)

;;;; SPECIAL VARIABLES

(defvar *cookie* (cl-cookie:make-cookie-jar))

;;;; INTERMIDIATE OBJECTS.

(defstruct uri uri method submethod)

(defstruct (node (:constructor %make-node) (:include uri)) edges)

(defstruct (edge (:include uri)))

(defstruct (failed (:include uri)) message)

;;;; CONSTRUCTOR

(defun make-node (uri method)
  ;; Canonicalize.
  (setf method (intern (string-upcase method) :keyword))
  ;; Body
  (%make-node :uri uri :method method :edges (uri-edges uri)))

(defun form-submethod (form)
  (loop :for input :across (clss:select "input[name=method]" form)
        :thereis (plump:attribute input "value")))

(defun uri-edges (uri)
  (handler-case (values (dex:get uri :cookie-jar *cookie*))
    (dex:http-request-failed (c)
      (list
        (make-failed :uri (dex:request-uri c)
                     :method (princ-to-string (dex:response-status c))
                     :message (princ-to-string (type-of c)))))
    (:no-error (body)
      (loop :for form :across (clss:select "form" (plump:parse body))
            :for action := (plump:attribute form "action")
            :for method := (plump:attribute form "method")
            :for next := (quri:merge-uris action uri)
            :for new
                 := (make-edge :uri next
                               :method method
                               :submethod (form-submethod form))
            :collect new))))

;;;; SITE-GRAPH

(defun make-site-table (uri &optional cookie)
  (setf uri (quri:uri uri))
  (unless (quri:uri-path uri)
    (setf (quri:uri-path uri) "/"))
  (let ((*cookie* (or cookie *cookie*))
        (known-nodes (make-hash-table :test #'equal)))
    (labels ((rec (node)
               (loop :for edge :in (node-edges node)
                     :if (or (typep edge 'failed)
                             (gethash (princ-to-string (edge-uri edge))
                                      known-nodes)
                             (eq :post (edge-method edge)))
                       :do '#:nothing
                     :else
                       :do (rec
                             (setf (gethash (princ-to-string (edge-uri edge))
                                            known-nodes)
                                     (make-node (edge-uri edge)
                                                (edge-method edge)))))))
      (rec
        (setf (gethash (princ-to-string uri) known-nodes)
                (make-node uri :get))))
    known-nodes))

(defun print-nodes (nodes)
  (pprint-logical-block (*standard-output* nil)
    (format t "digraph {~2I")
    (let ((id-table (make-hash-table :test #'equal)))
      (loop :for node :being :each :hash-value :of nodes :using (:hash-key key)
            :for id :upfrom 1
            :do (format t "~:@_~S [label=~S];"
                        (setf (gethash key id-table) (princ-to-string id))
                        (quri:uri-path (node-uri node))))
      (loop :for node :being :each :hash-value :of nodes :using (:hash-key key)
            :do (loop :for edge :in (node-edges node)
                      :do (format t "~:@_~S -> ~S[label=~S];"
                                  (gethash key id-table)
                                  (gethash (princ-to-string (uri-uri edge))
                                           id-table)
                                  (uri-method edge)))))
    (format t "~%}")))

(defun site-graph (uri &optional cookie)
  (let ((dot-path cl-dot:*dot-path*)
        (format (format nil "-T~(~a~)" :svg))
        (dot-string
         (with-output-to-string (*standard-output*)
           (print-nodes (make-site-table uri cookie)))))
    (uiop:run-program (list dot-path format "-o" "site-graph.svg")
                      :input (make-string-input-stream dot-string)
                      :output *standard-output*)))

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
