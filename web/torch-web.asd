; vim: ft=lisp et
(in-package :asdf)
(defsystem "torch-web"
  :version
  "0.19.1"
  :depends-on
  (
   "quri" ; URI object.
   "dexador" ; HTTP client.
   "cl-cookie" ; Cookie.
   "plump" ; DOM.
   "clss" ; CSS like selector.
   "cl-dot" ; Graphviz dot language generator.
   "alexandria" ; Public domain utilities.
   "torch" ; Visualize graphs. Especially for types.
   "cl-store" ; object serializer to cache data.
   )
  :pathname
  "src/"
  :components
  ((:file "torch-web")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "torch-web").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "torch-web"))))
  (append (call-next-method) '((test-op "torch-web.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "torch-web")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "torch-web"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (symbol-call :jingoh.documentizer :import c)))))
