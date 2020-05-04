#lang racket/base
(require racket/match
         racket/port)
(require json
         html-parsing
         net/base64
         net/uri-codec
         net/url)


(provide
 ; Attempting to mimic Browser's fetch api.
 fetch
 ; Converts secret to basic auth
 basic-auth-header
 ; Symbol, String -> Pair<Symbol, String>
 header-pair
 ; List<Pair<Symbol, String>> -> List<String>
 header-pairs
 ; List<Pair> -> ByteString
 body-pairs)


(define (fetch a-url
               #:method [method 'GET]
               #:headers [headers '()]
               #:body [body #f]
               #:response-type [type 'json])
  (define res-handler
    (match type
      [(quote json) read-json]
      [(quote text) port->string]
      [(quote html) html->xexp]
      [_ (lambda (a-port) a-port)]))

  (define-values (fetcher args) 
    (case method
      [(GET) (values get-pure-port (list a-url headers))]
      [(PUT) (values put-pure-port
                     (list a-url body headers))]
      [(POST) (values post-pure-port
                     (list a-url body headers))]
      [else (error (string-append "Given an invalid HTTP "
                                  "method of "
                                  (symbol->string method)
                                  "."))]))

  (res-handler (apply fetcher args)))


; Converts pairs to HTTP headers.
; List<Pair> -> List<String>
(define (header-pairs . pairs)
  (define (to-header a-pair)
    (string-append (symbol->string (car a-pair))
                   ": "
                   (cdr a-pair)))
  
  (map to-header pairs))


(define (header-pair key value)
  (cons key value))


; Converts pairs to a form-encoded string
; for HTTP requests.
; List<Pair> -> ByteString
(define (body-pairs . pairs)
  (string->bytes/utf-8
   (alist->form-urlencoded pairs)))


; String, String -> Pair<Symbol, String>
(define (basic-auth-header user password)
  (define basic-auth
    (bytes->string/utf-8
     (base64-encode
      (string->bytes/utf-8
       (string-append user ":" password)) #"")))

  (header-pair 'Authorization
               (string-append "Basic " basic-auth)))