(in-package :cl-user)

(defpackage :torch-web
  (:use :cl)
  (:export))

(in-package :torch-web)

(defvar *visited* (make-hash-table :test #'equal))

(defun visitedp (cons) (values (gethash (princ-to-string cons) *visited*)))

(defun visited (cons)
  (tagbody (setf (gethash (princ-to-string cons) *visited*) (car cons))))

(defmethod cl-dot:graph-object-node ((graph (eql 'web)) (uri cons))
  (make-instance 'cl-dot:node
                 :attributes (list :label (quri:uri-path (cdr uri))
                                   :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'web)) (uri cons))
  (loop :for cons :in (page-uris uri)
        :collect (make-instance 'cl-dot:attributed
                                :object cons
                                :attributes (list :label (car cons)))))

(defun page-uris (uri)
  (unless (visitedp uri)
    (visited uri)
    (when (equal "get" (car uri))
      (handler-case (values (dex:get (cdr uri)))
        (dex:http-request-failed ())
        (:no-error (body)
          (delete-if
            (lambda (cons)
              (or (visitedp cons)
                  (equal (princ-to-string cons) (princ-to-string uri))))
            (map 'list
                 (lambda (form)
                   (let* ((attributes (plump-dom:attributes form))
                          (action (gethash "ACTION" attributes))
                          (method (gethash "METHOD" attributes)))
                     (cons method (quri:merge-uris action (cdr uri)))))
                 (clss:select "form" (plump:parse body)))))))))

(defun site-graph (uri)
  (setf uri (quri:uri uri))
  (unless (quri:uri-path uri)
    (setf (quri:uri-path uri) "/"))
  (let* ((*visited* (make-hash-table :test #'equal))
         (graph
          (cl-dot:generate-graph-from-roots 'web (list (cons "get" uri)))))
    (cl-dot:dot-graph graph "site.svg" :format :svg)))
