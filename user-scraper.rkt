#lang racket/base
(require net/url
         json
         2htdp/batch-io)
(require racket/set
         racket/match
         racket/generator
         racket/async-channel)
(require (prefix-in users: "user-reddit-funcs.rkt"))


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


; Buffered-Async-Channel, Generator, delay?: Number -> Thread
(define (generator-thread ch gen #:delay [delay 2])
  (define (next! status)
    (define-values (_ update step) (next-state gen status))
    (begin (put! step) update))

  (define (put! step)
    (unless (null? step) (ch 'put step)))

  (thread
   (lambda ()
     (let loop ([status (status-manager)])
       (cond
         [(status 'done?) null]
         [else
          (begin
            (sleep delay)
            (match (thread-try-receive)
              ['close (next! (status 'done))]
              ['pause (loop (status 'stop))]
              ['start (loop (next! (status 'start)))]
              [_ (loop (next! status))]))])))))


(define (next-state gen status)
  (cond
    [(status 'stopped?) (values gen status null)]
    [else
     (define step (gen))
     (define update
       (match step
         [(? null? _) (status 'done)]
         [__ status]))
     (values gen update step)]))
     

(define (status-manager [current 'PENDING])
  (lambda (m)
    (case m
      [(pending) (status-manager 'PENDING)]
      [(start) (status-manager 'RUNNING)]
      [(stop) (status-manager 'PAUSED)]
      [(done) (status-manager 'DONE)]
      [(running?) (equal? current 'RUNNING)]
      [(done?) (equal? current 'DONE)]
      [(stopped?) (equal? current 'PAUSED)]
      [(pending?) (equal? current 'PENDING)]
      [else (status-manager current)])))


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
                             (users:paginator user)))
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

