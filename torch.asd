; vim: ft=lisp et
(in-package :asdf)
(defsystem :torch
  :version "0.0.0"
  :depends-on
  (
   "cl-dot"     ; dot api.
   "alexandria" ; public domain utilities.
   "predcase"   ; control flow.
   "millet"     ; wrapper for implementation dependent utilities.
   )
  :components((:file "package")
              ; bottom
              (:file "system-graph" :depends-on ("package"))
              (:file "package-graph" :depends-on ("package"))
              (:file "file-graph" :depends-on("package"))
              (:file "object" :depends-on ("package"))

              (:file "specials" :depends-on ("package"))
              (:file "type" :depends-on ("package"))
              (:file "graph" :depends-on("package"))
              ; mid1
              (:file "dot" :depends-on ("type" "specials"))
              (:file "code-graph" :depends-on("graph" "type"))
              ; mid2
              (:file "function-graph" :depends-on("graph" "code-graph" "specials"))
              ))
