;;;; -*- mode: lisp -*-
;;;;
;;;; This is the package definition for CL9
;;;;
;;;; Copyright (C) 2005-2008 Beta Nine BVBA. All Rights Reserved.

(defpackage :cl9
  (:use common-lisp)
  (:export
   
   #:read-u2
   #:read-u3
   #:read-u4
   #:read-u6
   #:read-u8
   #:write-u2
   #:write-u3
   #:write-u4
   #:write-u6
   #:write-u8
   #:read-bytes
   #:ascii->bytes
   #:bytes->ascii
   #:read-ascii-bytes
   #:write-ascii-bytes
   #:checksum-u2
   #:checksum-u4
   #:checksum-8
   #:checksum-8-bytes
   #:u4->s4
   #:s4->u4
   #:u8->s8
   #:s8->u8
   #:two-complement
   #:signed->unsigned
   #:unsigned->signed
   #:integer->bytes
   #:bytes->integer
   
   #:make-byte-array-input-stream
   #:with-input-from-byte-array
   #:make-byte-array-output-stream
   #:with-output-to-byte-array
   
   #:write-utf8-char
   #:read-utf8-char
   #:string->utf8-encoded-bytes
   #:utf8-encoded-bytes->string
   #:utf8-encoded-length
   #:flatten-higher-encodings
   
   #:make-byte-vector
   #:string->byte-vector
   #:byte-vector->string
   #:byte-vector->hex-string
   #:hex-string->byte-vector
   #:byte-vector->binary-string
   
   #:make-cache
   #:get-cache
   #:remove-cache
   #:clear-cache
   #:older-cache-entries
   #:force-update-cache
   #:map-cache
   #:get-hit-count
   #:get-miss-count

   #:*timezone*
   #:format-time
   #:universal-time->iso-8601
   #:iso-8601->universal-time
   #:universal-time->iso-8601-gmt
   #:iso-8601-gmt->universal-time
   #:ut
   #:parse-iso-8601-date
   #:universal-time->iso-8601-date
   #:parse-iso-8601-gmt-date
   #:universal-time->iso-8601-gmt-date
   #:universal-time-date
   #:print-time
   #:print-date
   #:print-duration
   #:universal-time-getf
   #:start-of-week
   #:end-of-week
   #:week-number
   #:start-of-month
   #:end-of-month
   #:universal-date-incf

   #:fac)
  (:documentation "Beta Nine Common Lisp Library and Framework"))

;;;; eof
