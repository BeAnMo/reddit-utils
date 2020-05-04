#lang racket/base
(require net/url
         json
         2htdp/batch-io)
(require racket/set
         racket/match
         racket/generator
         racket/async-channel)




(define (channel)
  (define ch (make-async-channel))
  
  (letrec ([self
            (lambda (msg . args)
              (case msg
                [(put) (begin
                         (async-channel-put ch (car args))
                         self)]
                [(take) (async-channel-try-get ch)]
                [else (error "Invalid message")]))])
    self))


(define (generator-thread ch gen #:delay [delay 2])
  (define manager (proc-manager))

  (define (run)
    (when (manager 'running?)
      (let ([current (gen)])
        (if current
            (ch 'put current)
            (manager 'stop)))))
          
  (thread
   (lambda ()
     (let loop ()
       (begin
         (sleep delay)
         (run)
         (match (thread-try-receive)
           ['close (begin (manager 'stop))]
           ['pause (begin (manager 'stop)
                          (loop))]
           ['start (begin (manager 'start)
                          (loop))]
           [_ (loop)]))))))


(define (proc-manager)
  (let ([running #f])
    (letrec ([self
              (lambda (m)
                (case m
                  [(start) (begin
                             (set! running #t)
                             self)]
                  [(stop) (begin
                            (set! running #f)
                            self)]
                  [(running?) running]))])
      self)))


(define (writer-thread #:delay [delay 4])
  (define (write content)
    (write-file
     (string-append "test-data/"
                    (number->string
                     (hash-ref content 'scraped))
                    ".json")
     (jsexpr->string content)))
  
  (thread
   (lambda ()
     (let loop ()
       (begin
         (sleep delay)
         (match (thread-try-receive)
           ['close null]
           [(? hash? data) (begin (write data)
                                  (loop))]
           [_ (loop)]))))))
            

(define (main users)
  (define ch (channel))
  (define writer (writer-thread))
  (define producers
    (map (lambda (user)
           (generator-thread ch
                             (paginator user)))
         users))

  (define (write-out content)
    (if content
        (begin (thread-send writer content) #t)
        #f))

  (define (write-remaining)
    (when (write-out (ch 'take))
      (write-remaining)))

  (begin
    (for-each (lambda (t) (thread-send t 'start))
              producers)
    (let loop ()
      (if (all-done? producers)
          (write-remaining)
          (begin (write-out (ch 'take))
                 (sleep 4)
                 (loop))))))
        
          
(define (all-done? thds)
  (andmap thread-dead? thds))

