;;; unit tests for our UTF-8 encoder/decoder

(in-package :cl9)

(let ((ascii "Simple ASCII"))
  (assert (equalp (string->utf8-encoded-bytes ascii)
                  (map 'vector 'char-code ascii)))
  (assert (equalp (utf8-encoded-bytes->string (map 'vector 'char-code ascii))
                  ascii)))

(let ((ascii "Simple ASCII")
      (french (map 'string 'code-char '(233 108 232 118 101 32 101 110 32 70 114 97 110 231 97 105 115)))
      (czech (with-output-to-string (out nil :element-type 'character)
               (loop :for c :in '(77 367 106 32 250 269 101 116)
                     :do (write-char (code-char c) out)))))
  (flet ((encode-decode (string)
           (let* ((encoded-bytes (string->utf8-encoded-bytes string))
                  (decoded-string (utf8-encoded-bytes->string encoded-bytes)))
             (assert (string-equal string decoded-string)))))
    (encode-decode ascii)
    (encode-decode french)
    (encode-decode czech)))

;;; eof

