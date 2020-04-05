; vim: ft=lisp et
(in-package :asdf)
(defsystem "torch-web.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "torch-web")
  :components
  ((:file "torch-web"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :torch-web args)))