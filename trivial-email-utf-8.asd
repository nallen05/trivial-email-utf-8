
(defpackage :trivial-email-utf-8-system
  (:use :cl))

(in-package :trivial-email-utf-8-system)

(asdf:defsystem :trivial-email-utf-8
  :depends-on (:cl-qprint :trivial-utf-8 :cl-ppcre :cl-smtp)
  :components
  ((:file "trivial-email-utf-8")))