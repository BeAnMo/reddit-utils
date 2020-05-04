#lang racket/base
(require json net/url)
(require racket/match)


(provide
 get-json
 hash-translate
 get-nested)

(define (get-json a-url)
  (read-json (get-pure-port a-url)))


; Hash, Hash -> Hash
(define (hash-translate shape doc)
  (for/hash ([k:v (in-list (hash->list shape))])
    (let ([key (car k:v)]
          [val (cdr k:v)])
      (values key
              (match val
                [1 (hash-ref doc key)]
                [(? procedure? proc) (proc doc)]
                [_ val])))))

(module+ test
  (require rackunit)

  (define S0 (hash 'weef 1
                   'queef 'whut?
                   'gort (lambda (doc)
                           (+ 10 (hash-ref doc 'weef)))))
  (define D0 (hash 'weef 4 'gorted 4))
  (define A0 (hash 'weef 4
                   'queef 'whut?
                   'gort 14))

  (check-equal? (hash-translate S0 D0)
                A0))

; Hash, List -> JSON
(define (get-nested a-hash keys)
  (define (loop current remaining)
    (cond
      [(or (null? remaining) (null? current)) current]
      [else
       (loop (match-val current (car remaining))
             (cdr remaining))]))

  (loop a-hash keys))


; JSON, Atom -> JSON
(define (match-val object val)
  (match val
    [(? number?) (list-ref object val)]
    [(? string?) (hash-ref object (string->symbol val))]
    [_ (hash-ref object val)]))

(module+ test
  (require rackunit)

  (define T0 (hash 'weef 2))

  (check-equal? (match-val T0 'weef) 2)
  
  (define T1 (list 1 2 T0))

  (check-equal? (get-nested T1 '(2 weef)) 2)
  )