#lang racket/base
(require net/url
         json)
(require racket/set
         racket/match
         racket/generator)
(require "user-utils.rkt"
         (prefix-in oauth: "oauth-v2.rkt"))


(provide
 paginator)

; String -> Generator<Hash>
(define (paginator user)
  (define (next-page after)
    (oauth:get `("user" ,user "overview")
               #:params `((sort . "new")
                          (after . ,after))))

  (define (current-page a-url page-num res)
    (hash 'user user
          'scraped (current-milliseconds)
          'data (map set-translation (actions res))
          'url (url->string a-url)
          'pageNum page-num))

  (define (loop current visited count)
    (cond
      [(or (equal? 'null current)
           (set-member? visited current)) null]
      [else
       (define-values (the-url res) (next-page current))
       (begin
         (yield (current-page the-url count res))
         (loop (next-pointer res)
               (set-add visited current)
               (+ count 1)))]))

  (generator () (loop "" (set) 0)))


(define (user-url user #:params [params '()])
  (url "https"
       #f
       "www.reddit.com"
       #f
       #t
       (list
        (path/param "user" '())
        (path/param user '())
        (path/param ".json" '()))
        params
        #f))


(define (next-pointer page)
  (hash-ref (hash-ref page 'data) 'after))


(define (actions page)
  (define children (get-nested
                    page
                    '(data children)))

  (for/list ([item (in-list children)])
    (hash-ref item 'data)))


(define (reddit-post doc)
  (hash-translate
   (hash 'author 1
         'created_ms (lambda (doc)
                       (* (hash-ref doc 'created) 1000))
         'entity_type "post"
         'id 1
         'num_comments 1
         'num_crossposts 1
         'score 1
         'selftext 1
         'title 1
         'subreddit 1)
   doc))


(define (set-translation doc)
  ((if (hash-has-key? doc 'body)
       reddit-comment
       reddit-post) doc))


(define (reddit-comment doc)
  (hash-translate
   (hash 'author 1
         'body 1
         'id 1
         'link_permalink 1
         'link_url 1
         'num_comments 1
         'score 1
         'subreddit 1
         'stickied 1
         'created_ms (lambda (doc)
                       (* (hash-ref doc 'created) 1000))
         'entity_type "comment")
   doc))
