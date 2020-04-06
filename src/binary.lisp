;;;; -*- mode: lisp -*-
;;;;
;;;; Binary manipulation code
;;;;
;;;; Copyright (C) 2005-2009 Beta Nine BVBA. All Rights Reserved.

(in-package :cl9)

;; Support

(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

(defun read-u3 (in)
  (let ((u3 0))
    (setf (ldb (byte 8 16) u3) (read-byte in))
    (setf (ldb (byte 8 8) u3) (read-byte in))
    (setf (ldb (byte 8 0) u3) (read-byte in))
    u3))

(defun read-u4 (in)
  (let ((u4 0))
    (setf (ldb (byte 8 24) u4) (read-byte in))
    (setf (ldb (byte 8 16) u4) (read-byte in))
    (setf (ldb (byte 8 8) u4) (read-byte in))
    (setf (ldb (byte 8 0) u4) (read-byte in))
    u4))

(defun read-u6 (in)
  (let ((u6 0))
    (setf (ldb (byte 8 40) u6) (read-byte in))
    (setf (ldb (byte 8 32) u6) (read-byte in))
    (setf (ldb (byte 8 24) u6) (read-byte in))
    (setf (ldb (byte 8 16) u6) (read-byte in))
    (setf (ldb (byte 8 8) u6) (read-byte in))
    (setf (ldb (byte 8 0) u6) (read-byte in))
    u6))

(defun read-u8 (in)
  (let ((u8 0))
    (setf (ldb (byte 8 56) u8) (read-byte in))
    (setf (ldb (byte 8 48) u8) (read-byte in))
    (setf (ldb (byte 8 40) u8) (read-byte in))
    (setf (ldb (byte 8 32) u8) (read-byte in))
    (setf (ldb (byte 8 24) u8) (read-byte in))
    (setf (ldb (byte 8 16) u8) (read-byte in))
    (setf (ldb (byte 8 8) u8) (read-byte in))
    (setf (ldb (byte 8 0) u8) (read-byte in))
    u8))

(defun write-u2 (u2 out)
  (write-byte (ldb (byte 8 8) u2) out)
  (write-byte (ldb (byte 8 0) u2) out))

(defun write-u3 (u3 out)
  (write-byte (ldb (byte 8 16) u3) out)
  (write-byte (ldb (byte 8 8) u3) out)
  (write-byte (ldb (byte 8 0) u3) out))

(defun write-u4 (u4 out)
  (write-byte (ldb (byte 8 24) u4) out)
  (write-byte (ldb (byte 8 16) u4) out)
  (write-byte (ldb (byte 8 8) u4) out)
  (write-byte (ldb (byte 8 0) u4) out))

(defun write-u6 (u6 out)
  (write-byte (ldb (byte 8 40) u6) out)
  (write-byte (ldb (byte 8 32) u6) out)
  (write-byte (ldb (byte 8 24) u6) out)
  (write-byte (ldb (byte 8 16) u6) out)
  (write-byte (ldb (byte 8 8) u6) out)
  (write-byte (ldb (byte 8 0) u6) out))

(defun write-u8 (u8 out)
  (write-byte (ldb (byte 8 56) u8) out)
  (write-byte (ldb (byte 8 48) u8) out)
  (write-byte (ldb (byte 8 40) u8) out)
  (write-byte (ldb (byte 8 32) u8) out)
  (write-byte (ldb (byte 8 24) u8) out)
  (write-byte (ldb (byte 8 16) u8) out)
  (write-byte (ldb (byte 8 8) u8) out)
  (write-byte (ldb (byte 8 0) u8) out))

(defun read-bytes (in count &key fail-if-incomplete)
  (let* ((buffer (make-array count :element-type '(unsigned-byte 8)))
         (bytes-read (read-sequence buffer in)))
    (cond ((= bytes-read count)
           buffer)
          (fail-if-incomplete
           (error "Failed to read ~d bytes" count))
          (t
           (subseq buffer 0 bytes-read)))))

(defun bytes->integer (bytes)
  (loop :for x :across bytes 
        :for i :downfrom (1- (length bytes))
        :sum (* x (expt 256 i))))

(defun integer->bytes (integer)
  (map 'vector
       #'identity
       (nreverse (loop :until (zerop integer)
                       :collect (prog1 (logand integer #xff)
                                  (setf integer (ash integer -8)))))))

(defun ascii->bytes (string)
  (map 'vector #'char-code string))

(defun bytes->ascii (bytes)
  (map 'string #'code-char bytes))

(defun read-ascii-bytes (in count &key fail-if-incomplete)
  (bytes->ascii (read-bytes in count :fail-if-incomplete fail-if-incomplete)))

(defun write-ascii-bytes (string out)
  (write-sequence (ascii->bytes string) out))

(defun checksum-u2 (u2)
  (+ (ldb (byte 8 8) u2) 
     (ldb (byte 8 0) u2)))

(defun checksum-u4 (u4)
  (+ (ldb (byte 8 24) u4) 
     (ldb (byte 8 16) u4)
     (ldb (byte 8 8) u4) 
     (ldb (byte 8 0) u4)))

(defun checksum-8 (&rest bytes)
  (logand #b11111111 (apply #'+ bytes)))

(defun checksum-8-bytes (byte-array)
  (logand #b11111111 (loop :for x :across byte-array :sum x)))

(defun two-complement (int bits)
  (let ((mask (1- (expt 2 bits))))
    (logand (1+ (logxor (abs int) mask)) mask)))

(defun unsigned->signed (unsigned bits)
  (if (< unsigned (expt 2 (1- bits)))
      unsigned
    (- (two-complement unsigned bits))))

(defun signed->unsigned (signed bits)
  (if (minusp signed)
      (two-complement signed bits)
    signed)) 

(defconstant +8-bits-on+ #b11111111)

(defconstant +16-bits-on+ #b1111111111111111)

(defconstant +24-bits-on+ #b111111111111111111111111)

(defconstant +32-bits-on+ #b11111111111111111111111111111111)

(defun u4->s4 (u4)
  (if (< u4 (expt 2 31))
      u4
    (logand (1+ (logxor u4 +32-bits-on+)) +32-bits-on+)))

(defun s4->u4 (s4)
  (if (minusp s4)
      (logand (1- (logxor s4 +32-bits-on+)) +32-bits-on+))
  s4)

(defconstant +64-bits-on+ #b1111111111111111111111111111111111111111111111111111111111111111)

(defun u8->s8 (u8)
  (if (< u8 (expt 2 63))
      u8
    (logand (1+ (logxor u8 +64-bits-on+)) +64-bits-on+)))

(defun s8->u8 (s8)
  (if (minusp s8)
      (logand (1- (logxor s8 +64-bits-on+)) +64-bits-on+)
    s8))

;; A simple byte-array-input-stream implementation (LispWorks specific)

(defclass byte-array-input-stream (#+lispworks stream:fundamental-binary-input-stream #+sbcl fundamental-binary-input-stream)
  ((byte-array :reader get-byte-array :initarg :byte-array :initform #())
   (position :reader get-position :initarg :start :initform 0)
   (end :initarg :end :initform nil)))

(defun make-byte-array-input-stream (byte-array &key (start 0) end)
  (make-instance 'byte-array-input-stream :byte-array byte-array :start start :end end))

(defmethod #+lispworks stream:stream-read-byte #+sbcl stream-read-byte ((byte-array-input-stream byte-array-input-stream))
  (with-slots (byte-array position end)
      byte-array-input-stream
    (if (< position (or end (length byte-array)))
        (prog1
            (aref byte-array position)
          (incf position))
      :eof)))

(defmethod stream-element-type ((byte-array-input-stream byte-array-input-stream))
  (array-element-type (get-byte-array byte-array-input-stream)))

(defmacro with-input-from-byte-array ((var byte-array &key (start 0) end) &body body)
  (let ((stream-name (gensym "byte-array-input-stream")))
    `(let ((,stream-name (make-byte-array-input-stream ,byte-array :start ,start :end ,end)))
       (with-open-stream (,var ,stream-name)
         ,@body))))

;; A simple byte-array-output-stream implementation (LispWorks specific)

(defclass byte-array-output-stream (#+lispworks stream:fundamental-binary-output-stream #+sbcl fundamental-binary-input-stream)
  ((byte-array :reader get-byte-array 
               :initarg :byte-array 
               :initform (make-array 64
                                     :element-type '(unsigned-byte 8) 
                                     :adjustable t
                                     :fill-pointer 0))
   (position :reader get-position :initarg :start :initform nil)
   (end :initarg :end :initform nil)))

(defun make-byte-array-output-stream (&optional byte-array &key (start 0) end)
  (if byte-array
      (make-instance 'byte-array-output-stream :byte-array byte-array :start start :end end)
    (make-instance 'byte-array-output-stream)))

(defmethod #+lispworks stream:stream-write-byte #+sbcl stream-write-byte ((byte-array-output-stream byte-array-output-stream) byte)
  (with-slots (byte-array position end)
      byte-array-output-stream
    (if position
        (if (< position (or end (length byte-array)))
            (prog1
                (setf (aref byte-array position) byte)
              (incf position))
          :eof)
      (vector-push-extend byte byte-array))))

(defmacro with-output-to-byte-array ((var &optional byte-array &key (start 0) end) &body body)
  (let ((stream-name (gensym "byte-array-output-stream")))
    `(let ((,stream-name (make-byte-array-output-stream ,byte-array :start ,start :end ,end)))
       (with-open-stream (,var ,stream-name)
         ,@body)
       (values (get-byte-array ,stream-name)
               (get-position ,stream-name)))))

;; Unicode UTF-8 encoding following RFC-3629 (up to 4 bytes, actually the range 0 to 10FFFF)

(defconstant +max-utf8-code+ #x10FFFF)

(defun utf8-encoded-length (string)
  "Computed the byte length of UTF-8 encoding the chars in string"
  (loop :for char :across string
        :for code = (char-code char)
        :summing (cond ((< code 128) 1)
                       ((< code 2048) 2)
                       ((< code 65535) 3)
                       ((<= code +max-utf8-code+) 4)
                       (t (error "codepoint ~d is outside RFC-3629 UTF-8 encoding" code)))))

(defun write-utf8-char (char stream)
  "Write a Unicode char in UTF-8 encoding to stream"
  (let ((code (char-code char)))
    (cond ((< code 128)
           (write-byte code stream))
          ((< code 2048)
           (write-byte (+ #b11000000 
                          (ash code -6))
                       stream)
           (write-byte (+ #b10000000
                          (logand code #b111111))
                       stream))
          ((< code 65535)
           (write-byte (+ #b11100000 
                          (ash code -12))
                       stream)
           (write-byte (+ #b10000000
                          (logand (ash code -6) #b111111))
                       stream)
           (write-byte (+ #b10000000
                          (logand code #b111111))
                       stream))
          ((<= code +max-utf8-code+)
           (write-byte (+ #b11110000 
                          (ash code -18))
                       stream)
           (write-byte (+ #b10000000
                          (logand (ash code -12) #b111111))
                       stream)
           (write-byte (+ #b10000000
                          (logand (ash code -6) #b111111))
                       stream)
           (write-byte (+ #b10000000
                          (logand code #b111111))
                       stream))
          (t (error "codepoint ~d is outside RFC-3629 UTF-8 encoding" code)))))

(defun string->utf8-encoded-bytes (string &key (start 0) end buffer)
  "Convert a Unicode Lispworks string to an UTF-8 encoded byte vector"
  (flet ((convert (out)
           (with-input-from-string (in string :start start :end end)
             (loop
              (let ((char (read-char in nil :eof)))
                (if (eql char :eof)
                    (return)
                  (write-utf8-char char out)))))))
    (if buffer
        (multiple-value-bind (bytes length)
            (with-output-to-byte-array (out buffer)
              (convert out))
          (declare (ignore bytes))
          length)
      (with-output-to-byte-array (out)
        (convert out)))))

(defun read-utf8-char (stream &optional (eof-error-p t) eof-value)
  "Read a UTF-8 encoded Unicoded char from stream"
  (let ((octet (read-byte stream eof-error-p eof-value))
        (code 0))
    (flet ((read-next ()
             (setf octet (read-byte stream))
             (unless (= (ldb (byte 2 6) octet) #b10) 
               (error "illegal/incorrect RFC-3629 UTF-8 encoding non-first sequence byte ~d" octet))
             (setf code (+ (ash code 6) (ldb (byte 6 0) octet)))))
      (cond ((eql octet eof-value)
             (return-from read-utf8-char eof-value))
            ((null (logbitp 7 octet))
             (setf code octet))
            ((= (ldb (byte 3 5) octet) #b110)
             (setf code (ldb (byte 5 0) octet))
             (read-next))
            ((= (ldb (byte 4 4) octet) #b1110)
             (setf code (ldb (byte 4 0) octet))
             (read-next)
             (read-next))
            ((= (ldb (byte 5 3) octet) #b11110)
             (setf code (ldb (byte 3 0) octet))
             (read-next)
             (read-next)
             (read-next))
            (t (error "illegal/incorrect RFC-3629 UTF-8 encoding ~d" octet))))
    (code-char code)))

(defun utf8-encoded-bytes->string (bytes &key (start 0) end buffer)
  "Convert a byte vector encoded in UTF-8 to a Unicode Lispworks string" 
  (flet ((convert (out)
           (with-input-from-byte-array (in bytes :start start :end end)
             (loop
              (let ((char (read-utf8-char in nil :eof)))
                (if (eql char :eof)
                    (return)
                  (write-char char out)))))))
    (if buffer
        (with-output-to-string (out buffer)
          (convert out))
      (with-output-to-string (out nil :element-type #+lispworks 'lw:simple-char #+sbcl 'character)
        (convert out)))))

(defun flatten-higher-encodings (string &optional (cutoff 255))
  "Convert a string by replacing chars with higher than cuttoff char codes with ?s"
  (with-output-to-string (out)
    (loop :for char :across string :do 
          (if (< cutoff (char-code char))
              (write-char #\? out)
            (write-char char out)))))

;; Byte vector utility code

(defun make-byte-vector (size &optional (initial-element 0))
  "Make a byte vector of size bytes filled with initial-element"
  (make-array size :element-type '(unsigned-byte 8) :initial-element initial-element))

(defun string->byte-vector (string)
  "Convert an 8-bit ASCII string to a byte vector"
  (let ((bytes (make-byte-vector (length string))))
    (loop :for char :across string
          :for i :upfrom 0
          :do (setf (aref bytes i) (mod (char-code char) 256)))
    bytes))

(defun byte-vector->string (bytes)
  "Convert a byte vector to an 8-bit ASCII string"
  (let ((string (make-string (length bytes))))
    (loop :for byte :across bytes
          :for i :upfrom 0
          :do (setf (char string i) (code-char byte)))
    string))

(defparameter +hex-digits+ "0123456789ABCDEF")

(defun byte-vector->hex-string (bytes)
  "Convert a byte vector to a hex string, as in #(1 15 255) becomes '010FFF'"
  (let ((string (make-string (* 2 (length bytes)))))
    (loop :for byte :across bytes
          :for i :upfrom 0
          :do (let ((upper (ash byte -4))
                    (lower (logand byte #xF)))
                (setf (char string (* i 2)) (char +hex-digits+ upper)
                      (char string (1+ (* i 2))) (char +hex-digits+ lower))))
    string))

(defun hex-string->byte-vector (hex-string)
  "Convert a hex string to a byte vector, as '010FFF' becomes #(1 15 255)"
  (assert (evenp (length hex-string)))
  (let ((bytes (make-byte-vector (/ (length hex-string) 2))))
    (loop :for i :upfrom 0 :below (length bytes)
          :do (setf (aref bytes i) (+ (ash (position (char hex-string (* i 2)) +hex-digits+ :test #'char-equal) 4)
                                      (position (char hex-string (1+ (* i 2))) +hex-digits+ :test #'char-equal))))
    bytes))

(defun byte-vector->binary-string (bytes)
  (format nil "~{~8,'0b~^-~}" (map 'list #'identity bytes)))

;;;; eof
