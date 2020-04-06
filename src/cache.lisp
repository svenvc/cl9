;;;; -*- mode: lisp -*-
;;;;
;;;; General purpose cache that limits the age of its entries as well as the total number
;;;;
;;;; Copyright (C) 2005-2009 Beta Nine BVBA. All Rights Reserved.

(in-package :cl9)

(defclass cache ()
  ((max-entries :reader get-max-entries :initarg :max-entries :initform 256)
   (time-to-live :accessor get-time-to-live :initarg :time-to-live :initform 60)
   (finder :reader get-finder :initarg :finder :initform (constantly nil))
   (hash-table :reader get-hash-table :initarg :hash-table :initform :uninitialized)
   (hit-count :reader get-hit-count :initform 0)
   (miss-count :reader get-miss-count :initform 0)
   (lock :reader get-lock :initform (s-sysdeps:make-process-lock "cache-lock"))))

(defun make-cache (finder &key (max-entries 256) (time-to-live 60) (test 'eql))
  (make-instance 'cache
                 :finder finder
                 :max-entries max-entries
                 :time-to-live time-to-live
                 :hash-table (make-hash-table :test test)))
 
(defmethod get-cache ((cache cache) key &optional default)
  (let ((now (get-universal-time)))
    (with-slots (hash-table finder max-entries time-to-live lock hit-count miss-count)
        cache
      (multiple-value-bind (value present-p)
          (s-sysdeps:with-process-lock (lock)
            (gethash key hash-table))
        (if (and present-p (< (- now (car value)) time-to-live))
            (progn
              ;; we don't lock here because this isn't critical
              (incf hit-count)
              (setf (car value) now)
              (values (cdr value) :hit))
          (let ((computed-value (funcall finder key)))
            (incf miss-count)
            (s-sysdeps:with-process-lock (lock)
              (unless (or (< (hash-table-count hash-table) max-entries) present-p)
                (shrink-cache cache))
              (setf (gethash key hash-table) (cons now computed-value)))
            (values computed-value :miss)))))))

(defmethod force-update-cache ((cache cache) key)
  (with-slots (hash-table finder lock max-entries)
      cache
    (let ((computed-value (funcall finder key))
          (now (get-universal-time)))
      (s-sysdeps:with-process-lock (lock)
        (multiple-value-bind (old-value present-p)
            (gethash key hash-table)
          (declare (ignore old-value))
          (unless (or (< (hash-table-count hash-table) max-entries) present-p)
            (shrink-cache cache))
          (setf (gethash key hash-table) (cons now computed-value))
          computed-value)))))

(defmethod shrink-cache ((cache cache) &optional (factor 0.1))
  (let* ((count (hash-table-count (get-hash-table cache)))
         (entries (make-array count))
         (index 0)
         (remove-count (floor (* count factor))))
    (map-cache #'(lambda (key value timestamp) 
                   (declare (ignore value))
                   (setf (aref entries index) (cons key timestamp))
                   (incf index))
               cache)
    (sort entries #'< :key #'cdr)
    (loop :for index :below remove-count
          :do (remove-cache cache (car (aref entries index))))
    remove-count))

(defmethod remove-cache ((cache cache) key)
  (s-sysdeps:with-process-lock ((get-lock cache))
    (remhash key (get-hash-table cache))))

(defmethod clear-cache ((cache cache))
  (s-sysdeps:with-process-lock ((get-lock cache))
    (clrhash (get-hash-table cache))
    (with-slots (hit-count miss-count)
        cache
      (setf hit-count 0 miss-count 0))))
    
(defmethod map-cache (function (cache cache))
  (s-sysdeps:with-process-lock ((get-lock cache))
    (maphash #'(lambda (key value) (funcall function key (cdr value) (first value)))
             (get-hash-table cache))))

(defmethod dump-cache ((cache cache))
  (let (all)
    (map-cache #'(lambda (key value timestamp) 
                   (push (list key value timestamp) all))
               cache)
    all))

(defun find-oldest-key (hash-table &optional (now (get-universal-time)))
  (let ((oldest-age now)
        oldest-key)
    (maphash #'(lambda (key value)
                 (when (<= (car value) oldest-age)
                   (setf oldest-age (car value)
                         oldest-key key)))
             hash-table)
    oldest-key))

(defmethod older-cache-entries ((cache cache) &optional (epsilon 120))
  (let ((now (get-universal-time))
        (time-to-live (get-time-to-live cache))
        older-keys)
    (map-cache #'(lambda (key value timestamp) 
                   (declare (ignore value))
                   (when (< time-to-live (+ (- now timestamp) epsilon))
                     (push key older-keys)))
               cache)
    older-keys))

;;;; eof
