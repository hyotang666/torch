; vim: ft=lisp et
(in-package :asdf)
(defsystem :torch
  :version "0.1.3"
  :depends-on
  (
   "cl-dot"     ; dot api.
   "alexandria" ; public domain utilities.
   "predcase"   ; control flow.
   "millet"     ; wrapper for implementation dependent utilities.
   )
  :pathname "src/"
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

(defmethod operate :after(o (c (eql(find-system :torch)))&key)
  (unless(symbol-value(find-symbol "*DOT-PATH*" "CL-DOT"))
    (warn "Command \"dot\" is not installed. ~
          After install graphviz, do one of below.~%~
          ~2T1: (asdf:load-system :cl-dot :force t)~%~
          ~2T2: (setq cl-dot:*dot-path* \"path/to/dot\")")))
