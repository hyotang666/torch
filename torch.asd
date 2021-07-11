; vim: ft=lisp et
(in-package :asdf)
(defsystem :torch
  :version "0.2.1"
  :depends-on
  (
   "cl-dot"     ; dot api.
   "alexandria" ; public domain utilities.
   "predcase"   ; control flow.
   "millet"     ; wrapper for implementation dependent utilities.
   )
  :pathname "src/"
  :components((:file "torch")))

(defmethod operate :after(o (c (eql(find-system :torch)))&key)
  (unless(symbol-value(find-symbol "*DOT-PATH*" "CL-DOT"))
    (warn "Command \"dot\" is not installed. ~
          After install graphviz, do one of below.~%~
          ~2T1: (asdf:load-system :cl-dot :force t)~%~
          ~2T2: (setq cl-dot:*dot-path* \"path/to/dot\")")))
