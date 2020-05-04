#lang racket/base
(require 2htdp/batch-io
         racket/string)


(provide
 ; String -> Void
 load-env
 ; Symbol -> String
 get-env)


(define (load-env file-path)
  (define lines (read-lines file-path))

  (for ([line (in-list lines)])
    (let-values ([(key value) (env-key/value line)])
      (env-set! key value))))


; String, String -> Void
(define (env-set! key value)
  (environment-variables-set! (current-environment-variables)
                              (string->bytes/utf-8 key)
                              (string->bytes/utf-8 value)))


; String -> Values<String, String>
(define (env-key/value line)
  (define splitted (string-split line "="))

  (values (car splitted) (cadr splitted)))


; Symbol -> String or False
(define (get-env key)
  (define maybe (environment-variables-ref (current-environment-variables)
                                           (symbol->bytes/utf-8 key)))

  (if maybe
      (bytes->string/utf-8 maybe)
      #f))


(define (symbol->bytes/utf-8 s)
  (string->bytes/utf-8 (symbol->string s)))