#lang sicp


(define (get-record file employee-name)
  (let ((division-type (type-tag file)))
    (let ((op (get 'record division-type)))
      (apply op (contents file) employee-name))))

(define (get-salary employee-record)
  (let ((division-type (type-tag employee-record)))
    (let ((op (get 'salary division-type)))
      (apply op (contents employee-record)))))


; a)
; All divisions have to implement and add a method to the dispatch like (put 'record 'division-a record).
; Each record file from each division have to be tagged with their corresponding division tag.
; In addition, we want to tag every employee record with the division as well, since the structure
; of those also depend on the division.
;
; For example, division-a:

(define (install-division-a-package)

  (define (record file employee-name)
    ; get employee from the records file, the division-a way.
    (if (null? file)
        null   
        (let (current-name (car (car file)))
          (if (eq? current-name employee-name)
              (car file)
              (record (cdr file) employee-name)))))

  ; b)
  ; We assume that an employee record for 'division-a is tagged with 'division-a, and and consists
  ; of a list where the first item is their name (which is also their id), and the second item is
  ; their salary.
  (define (salary employee-record)
      (cadr employee-record))
  
  (put 'record 'division-a record)
  (put 'salary 'division-a salary)
  )

; c)
; We assume we have a filter and a map function available. If we can also assume that the name
; of an employee is unique across the company, then we could return the first element of the list
; instead of a list of records, like we do below.
(define (find-employee-record files employee-name)
  (filter (lambda (x) (not (null? x)))
          (map (lambda (file) (get-record file employee-name)))))


; d)
; To incorporate a new company, they have to make their employee record file available, tag the
; file and individual employee records with their division, and install a 'record and
; 'salary method for their division in their package.
