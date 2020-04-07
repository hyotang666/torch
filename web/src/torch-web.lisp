(in-package :cl-user)

(defpackage :torch-web
  (:use :cl)
  (:export))

(in-package :torch-web)

(defvar *visited* (make-hash-table :test #'equal))

(defun visitedp (cons) (values (gethash (princ-to-string cons) *visited*)))

(defun visited (uri list)
  (tagbody (setf (gethash (princ-to-string uri) *visited*) list)))

(defstruct uri uri method submethod)

(defstruct (failed (:include uri)) message)

(defmethod cl-dot:graph-object-node ((graph (eql 'web)) (uri uri))
  (make-instance 'cl-dot:node
                 :attributes (list :label (typecase uri
                                            (failed (failed-message uri))
                                            (uri
                                             (quri:uri-path (uri-uri uri))))
                                   :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'web)) (uri uri))
  (loop :for uri :in (uri-page-uris uri)
        :collect (make-instance 'cl-dot:attributed
                                :object uri
                                :attributes (list :label (string-upcase
                                                           (or (uri-submethod
                                                                 uri)
                                                               (uri-method
                                                                 uri)))))))

(defun uri-page-uris (uri)
  (or (visitedp uri)
      (when (and (string-equal :get (uri-method uri)) (not (failed-p uri)))
        (handler-case (values (dex:get (uri-uri uri)))
          (dex:http-request-failed (c)
            (let ((new
                   (make-failed :uri (dex:request-uri c)
                                :method (princ-to-string
                                          (dex:response-status c))
                                :message (dex:response-body c))))
              (list new)))
          (:no-error (body)
            (loop :for form :across (clss:select "form" (plump:parse body))
                  :for action := (plump:attribute form "action")
                  :for method := (plump:attribute form "method")
                  :for next := (quri:merge-uris action (uri-uri uri))
                  :for new
                       := (make-uri :uri next
                                    :method method
                                    :submethod (form-submethod form))
                  :unless (equalp new uri)
                    :collect new :into news
                  :finally (visited uri news)
                           (return news)))))))

(defun form-submethod (form)
  (loop :for input :across (clss:select "input[name=method]" form)
        :thereis (plump:attribute input "value")))

(defun site-graph (uri)
  (setf uri (quri:uri uri))
  (unless (quri:uri-path uri)
    (setf (quri:uri-path uri) "/"))
  (let* ((*visited* (make-hash-table :test #'equal))
         (graph
          (cl-dot:generate-graph-from-roots 'web
                                            (list
                                              (make-uri :uri uri
                                                        :method "get")))))
    (cl-dot:dot-graph graph "site.svg" :format :svg)))
