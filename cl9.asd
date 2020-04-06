;;;; -*- Mode: LISP -*-
;;;;
;;;; The CL9 ASDF system definition
;;;;
;;;; Copyright (C) 2005-2008 Beta Nine BVBA. All Rights Reserved.

(in-package :asdf)

(defsystem :cl9
  :name "CL9"
  :author "Sven Van Caekenberghe - Beta Nine BVBA"
  :version "1"
  :maintainer "Sven Van Caekenberghe <sven@beta9.be>"
  :licence "Commercial. Copyright (C) 2005-2008 Beta Nine BVBA. All Rights Reserved."
  :description "Beta Nine Common Lisp Library and Framework"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "binary" :depends-on ("package"))
                 (:file "cache" :depends-on ("package"))
                 (:file "time" :depends-on ("package"))
                 (:file "cl9" :depends-on ("package")))))
  :depends-on (:s-utils :s-sysdeps))

;;;; eof
