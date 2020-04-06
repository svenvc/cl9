;;;; -*- mode: lisp -*-
;;;;
;;;; General purpose time related tools
;;;;
;;;; Copyright (C) 2005-2008 Beta Nine BVBA. All Rights Reserved.

(in-package :cl9)

(defvar *timezone* nil 
  "Timezone to use as default for time related functions, nil equals 0 equals GMT")

;; time related

(defun format-time (universal-time 
                    &key 
                    (format s-utils:+us-time-format+) 
                    (day-names s-utils:+us-day-names+)  
                    (month-names s-utils:+us-month-names+)
                    (timezone *timezone*)
                    stream)
  "Wrap s-utils:format-universal-time using *timezone*"
  (s-utils:format-universal-time universal-time
                                 :format format
                                 :day-names day-names
                                 :month-names month-names
                                 :decode-in-timezone timezone
                                 :stream stream))

;; time ISO 8601 support

(defun universal-time->iso-8601 (universal-time &key (timezone *timezone*))
  "Convert a Common Lisp universal time to a full 15 char ISO 8601 string 'YYYYMMDDTHHMMSS'"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time (or timezone 0))
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" 
            year month date hour minute second)))

(defun iso-8601->universal-time (string &key (timezone *timezone*))
  "Convert a full 15 (or short 8) char ISO 8601 string 'YYYYMMDDTHHMMSS' to a Common Lisp universal time"
  (cond ((= 15 (length string))
         (let ((year (parse-integer string :start 0 :end 4))
               (month (parse-integer string :start 4 :end 6))
               (date (parse-integer string :start 6 :end 8))
               (hour (parse-integer string :start 9 :end 11))
               (minute (parse-integer string :start 11 :end 13))
               (second (parse-integer string :start 13 :end 15)))
           (encode-universal-time second minute hour date month year (or timezone 0))))
        ((= 8 (length string))
         (let ((year (parse-integer string :start 0 :end 4))
               (month (parse-integer string :start 4 :end 6))
               (date (parse-integer string :start 6 :end 8)))
           (encode-universal-time 0 0 0 date month year (or timezone 0))))
        (t
         (error "String ~s must be 15 or 8 characters to represent an ISO 8601 GMT time" string))))

(defun universal-time->iso-8601-gmt (universal-time)
  "Convert a Common Lisp universal time to a full 15 char ISO 8601 GMT string 'YYYYMMDDTHHMMSS'"
  (universal-time->iso-8601 universal-time :timezone 0))

(defun iso-8601-gmt->universal-time (string)
  "Convert a full 15 char ISO 8601 GMT string 'YYYYMMDDTHHMMSS' to a Common Lisp universal time"
  (iso-8601->universal-time string :timezone 0))

;; convenience

(defun ut (&optional year month date (hours 0) (minutes 0) (seconds 0) (timezone *timezone*))
  "Convenience function to create Common Lisp universal times"
  (when (or (null year) (null month) (null date))
    (multiple-value-bind (second minute hour current-date current-month current-year)
        (decode-universal-time (get-universal-time) (or timezone 0))
      (declare (ignore second minute hour))
      (unless year (setf year current-year))
      (unless month (setf month current-month))
      (unless date (setf date current-date))))
  (encode-universal-time seconds minutes hours
                         date month year
                         0))

;; date ISO 8601 support

(defun parse-iso-8601-date (string &optional (timezone *timezone*))
  "Convert a full 8 char ISO 8601 date string 'YYYYMMDD' to a Common Lisp universal time"
  (if (= 8 (length string))
      (let ((year (parse-integer string :start 0 :end 4))
            (month (parse-integer string :start 4 :end 6))
            (date (parse-integer string :start 6 :end 8)))
        (encode-universal-time 0 0 0 date month year (or timezone 0)))
    (error "String ~s has wrong length to represent a full 8 char ISO 8601 GMT date" string)))

(defun universal-time->iso-8601-date (&optional (universal-time (get-universal-time)) (timezone *timezone*))
  "Convert a Common Lisp universal time into a full 8 char ISO 8601 date string"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time (or timezone 0))
    (declare (ignore second minute hour))
    (format nil "~4,'0d~2,'0d~2,'0d" year month date)))

(defun parse-iso-8601-gmt-date (string)
  "Convert a full 8 char ISO 8601 GMT date string 'YYYYMMDD' to a Common Lisp universal time"
  (parse-iso-8601-date string 0))

(defun universal-time->iso-8601-gmt-date (&optional (universal-time (get-universal-time)))
  "Convert a Common Lisp universal time into a full 8 char ISO 8601 GMT date string"
  (universal-time->iso-8601-date universal-time 0))

;; conversion

(defun universal-time-date (&optional (reference-universal-time (get-universal-time)) (timezone (or *timezone* 0)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time reference-universal-time timezone)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 date month year timezone)))

;; formatting

(defun print-time (universal-time &optional stream (timezone (or *timezone* 0)))
  (multiple-value-bind (second minute hour)
      (decode-universal-time universal-time timezone)
    (format stream "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun print-date (universal-time &optional stream (timezone (or *timezone* 0)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time timezone)
    (declare (ignore second minute hour))
    (format stream "~2,'0d/~2,'0d/~d" date month year)))

(defun print-duration (seconds &optional stream)
  (let (hours minutes)
    (setf hours (floor seconds (* 60 60)))
    (setf seconds (rem seconds (* 60 60)))
    (setf minutes (floor seconds 60))
    (setf seconds (rem seconds 60))
    (format stream "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds)))

;; time/date access & computation

(defun universal-time-getf (universal-time attribute &optional (timezone (or *timezone* 0)))
  (multiple-value-bind (second minute hour date month year day daylight-p timezone)
      (decode-universal-time universal-time timezone)
    (ecase attribute
      (:second second) ; 0 - 59
      (:minute minute) ; 0 - 59
      (:hour hour) ; 0 - 23
      (:date date) ; 1 - 31
      (:month month) ; 1 - 12
      (:year year) ; integer
      (:day day) ; 0 (monday) - 6 (sunday)
      (:daylight-p daylight-p) ; t means in effect
      (:timezone timezone)))) ; number of hours difference with GMT
      
(defconstant +first-day-of-week+ 0)

(defconstant +seconds-in-a-day+ (* 24 60 60))

(defun start-of-week (&optional (reference-universal-time (get-universal-time)))
  (let* ((date (universal-time-date reference-universal-time))
         (weekday (universal-time-getf date :day)))
    (- date (* +seconds-in-a-day+ (abs (- weekday +first-day-of-week+))))))
  
(defun end-of-week (&optional (reference-universal-time (get-universal-time)))
  (+ (* 6 +seconds-in-a-day+) (start-of-week reference-universal-time)))

(defun week-number (&optional (reference-universal-time (get-universal-time)))
  (let ((date (universal-time-date reference-universal-time)))
    (1+ (floor (- date (ut (universal-time-getf date :year) 1 1)) (* 7 +seconds-in-a-day+)))))

(defun start-of-month (&optional (reference-universal-time (get-universal-time)))
  (let ((month (universal-time-getf reference-universal-time :month))
        (year (universal-time-getf reference-universal-time :year)))
    (ut year month 1)))

(defun end-of-month (&optional (reference-universal-time (get-universal-time)))
  (let ((month (universal-time-getf reference-universal-time :month))
        (year (universal-time-getf reference-universal-time :year)))
    (- (ut (if (= month 12) (1+ year) year) (if (= month 12) 1 (1+ month)) 1)
       +seconds-in-a-day+)))
  
(defun leap-year-p (year)
  "Is this year a leap year?"
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun days-in-month (month year)
  "Days in a certain month"
  (case month
    (2 (if (leap-year-p year) 29 28))
    ((4 6 9 11) 30)
    (t 31)))

(defun universal-date-incf (universal-date attribute &optional (delta 1))
  (let ((date (universal-time-getf universal-date :date))
        (month (universal-time-getf universal-date :month))
        (year (universal-time-getf universal-date :year)))
    (ecase attribute
      (:day (+ universal-date (* delta +seconds-in-a-day+))) 
      (:week (+ universal-date (* delta 7 +seconds-in-a-day+))) 
      (:month (let ((new-month-count (+ (1- month) delta)))
                (multiple-value-bind (year-delta new-month)
                    (floor new-month-count 12)
                  (ut (+ year year-delta) 
                      (1+ new-month) 
                      (min date (days-in-month (1+ new-month) (+ year year-delta)))))))
      (:year (ut (+ year delta) month date)))))

;;;; eof
