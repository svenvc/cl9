;;;; -*- mode: lisp -*-
;;;;
;;;; Main file of the CL9 Library/Framework.
;;;;
;;;; Copyright (C) 2005-2008 Beta Nine BVBA. All Rights Reserved.

(in-package :cl9)

(defun fac (n)
  "Compute the factorial of n, n! as 1!=1, n!=n*(n-1)!"
  (if (< n 2) 
      1
      (* n (fac (1- n)))))

;;;; eof
