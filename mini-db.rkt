#lang at-exp racket/base
(require (for-syntax racket/base)
         racket/stxparam)

(provide make-database)

#| Use: (make-database NAME)
takes an identifier and defines
* NAME (a vector which never shrinks but sometimes doubles in size)
* add-NAME-data! (-> data index-where-stored)
* clear-NAME! (-> void void)
* NAME-max-index (a number indicating the maximum storage)
* NAME-current-index (a number indicating the current empty position)
* register-NAME-changed-callback (-> (-> db evt any) void)
      Procedure which takes a procedure accepting two arguments which
      is called when the underlying data changes. The 'evt' is the symbol
      'add or the symbol 'clear indicating what happened, and db is the
      database vector. Growing the database doesn't count as an event.
* [gensym]-NAME-callback is the storage location for the registered callback
* NAME-null-value is the (gensym) created at the time of database creation
      and is the "empty" value the vector is filled with so that #f values can
      be stored literally.

See the (module+ test ...) below for an example.

Future:
  use of channels for adding/clearing to make the database threadsafe
|#

(define-syntax (make-database stx)
  (syntax-case stx ()
    ((_ name)
     (let* ((N (syntax->datum #'name))
            (grow-name (datum->syntax stx (string->symbol (format "increase-~a-storage" N))))
            (add-name (datum->syntax stx (string->symbol (format "add-~a-data!" N))))
            (clear-name (datum->syntax stx (string->symbol (format "clear-~a!" N))))
            (max-name (datum->syntax stx (string->symbol (format "~a-max-index" N))))
            (current-name (datum->syntax stx (string->symbol (format "~a-current-index" N))))
            (change-name (datum->syntax stx (string->symbol (format "register-~a-changed-callback" N))))
            (callback-name (datum->syntax stx (string->symbol (format "~a-~a-callback" (gensym) N))))
            (false-value-name (datum->syntax stx (string->symbol (format "~a-null-value" N)))))
       (with-syntax ((grow grow-name)
                     (add add-name)
                     (clear clear-name)
                     (max max-name)
                     (current current-name)
                     (change change-name)
                     (callback callback-name)
                     (false-value false-value-name))
         #'(make-db 8 name grow add clear max current change callback false-value))))))


(define-syntax-rule (make-db start-storage db grower adder clearer max-index current-index change-callback callback false-value)
  (begin
    (define false-value (gensym))
    (define db (make-vector start-storage false-value))
    (define max-index (sub1 start-storage ))
    (define current-index 0)
    (define callback (box #f))
    (define (change-callback proc)
      (set-box! callback proc))
    (define (grower)
      (let ((new-count (* 2 (add1 max-index))))
        (let ((new-db (make-vector new-count false-value)))
          (set! max-index (sub1 new-count))
          (vector-copy! new-db 0 db)
          (set! db new-db))))
    (define (clearer)
      (vector-fill! db #f)
      (set! current-index 0)
      (when (unbox callback)
        (thread (λ() ((unbox callback) db 'clear))))
      (void))
    (define (adder data)
      (when (= current-index max-index)
        (grower))
      (vector-set! db current-index data)
      (set! current-index (add1 current-index))
      (begin0 (sub1 current-index)
              (thread (λ() ((unbox callback) db 'add)))))))

(module+ test
  (require rackunit racket/list)
  (make-database testdb)
  (define add-channel (make-channel))
  (define clear-channel (make-channel))
  (define (callback database event-type)
    (define c (case event-type
                ((add) add-channel)
                ((clear) clear-channel)))
    (channel-put c (list database event-type)))
  (check-true (procedure? increase-testdb-storage))
  (check-true (procedure? add-testdb-data!))
  (check-true (procedure? clear-testdb!))
  (check-true (zero? testdb-current-index))
  (check-true (< 0 testdb-max-index))
  (register-testdb-changed-callback callback)
  (struct dp (data) #:prefab)
  (define index (add-testdb-data! (dp '(1))))
  (check-equal? (dp '(1)) (let ((v (sync/timeout 1 add-channel)))
                       (and v
                            (let ((db (first v)) (type (second v)))
                              (vector-ref db index)))))
  (clear-testdb!)
  (define index-2 (add-testdb-data! (dp '(2))))
  (check-equal? (dp '(2)) (let ((v (sync/timeout 1 add-channel)))
                       (and v
                            (let ((db (first v)) (type (second v)))
                              (vector-ref db index-2)))))
  (check-eq? 'clear (let ((v (sync/timeout 1 clear-channel)))
                      (and v (second v))))
  (check-= index index-2 0)
  )