(in-package :torch)

(defmethod cl-dot:graph-object-node ((graph (eql 'code)) (object code))
  (make-instance 'cl-dot:node
                 :attributes (list :label (substitute #\Newline #\Space
                                                      (format nil "~A~@[ ~A~]"
                                                              (code-name
                                                                object)
                                                              (when *ignore-privates*
                                                                (code-privates
                                                                  object))))
                                   :shape :box
                                   :color (predcase (code-name object)
                                                    (external-symbolp :red)
                                                    (symbol-names-file-p :blue)
                                                    (t :black)))))

(defun external-symbolp (symbol)
  (eq :external (nth-value 1
                           (find-symbol (symbol-name symbol)
                                        (symbol-package symbol)))))

(defun symbol-names-file-p (symbol)
  (let ((system
         (asdf:find-system
           (string-downcase (package-name (symbol-package symbol))) nil)))
    (when system
      (values
        (gethash (string-downcase (symbol-name symbol))
                 (asdf:component-children-by-name system))))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'code)) (object code))
  (edges object))

(defmethod cl-dot:graph-object-node ((graph (eql 'code)) (o symbol))
  (make-instance 'cl-dot:node
                 :id (format nil "~A~%(Accessor?)" o)
                 :attributes (list :color (if (external-symbolp o)
                                              :red
                                              :black))))

(defun edges (code)
  (append
    (loop :for common :in (code-commons code)
          :collect (or (gethash common *codes*) common))
    (unless *ignore-privates*
      (loop :for private :in (code-privates code)
            :collect (or (gethash private *codes*) private)))))