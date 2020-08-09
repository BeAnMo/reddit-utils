#lang racket/base

(require racket/date net/url)
(require (prefix-in http: "http-utils.rkt")
         "dotenv.rkt")

(provide
 current-token-response
 
 get)

(load-env ".env")

(define ID (get-env 'REDDIT_APP_ID))
(define SECRET (get-env 'REDDIT_APP_SECRET))
(define USER (get-env 'REDDIT_USER))
(define PASS (get-env 'REDDIT_PASS))
(define REDIRECT-URI (get-env 'REDDIT_REDIRECT_URI))
(define SCOPES (get-env 'REDDIT_APP_SCOPES))
(define USER-AGENT (get-env 'REDDIT_APP_USER_AGENT))

(define (random-state)
  (substring (number->string (random)) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-token-response
  (make-parameter (hash 'expires_in 0
                        'access_token "")))

(define (get-refresh-token)
  (begin
    (current-token-response (fetch-token))
    (current-token-response)))

(define (expires-in)
  (hash-ref (current-token-response) 'expires_in))

(define (access-token)
  (cond
    [(and (not (expired?)) (not (invalid-token?)))
     (hash-ref (current-token-response) 'access_token)]
    [else (hash-ref (get-refresh-token) 'access_token)]))

(define (invalid-token?)
  (string=? (hash-ref (current-token-response) 'access_token) ""))

(define (expired?)
  (< (expires-in)
     (- (date->seconds (current-date) 1000))))

(define (bearer-header)
  `(Authorization . ,(string-append "Bearer "
                                    (access-token))))


(define (fetch-token)
  (http:fetch (reddit-url #:path '("api" "v1" "access_token"))
              #:method 'POST
              #:response-type 'json
              #:headers (http:header-pairs (http:basic-auth-header ID SECRET)
                                           (http:header-pair 'User-Agent USER-AGENT))
              #:body (http:body-pairs '(grant_type . "password")
                                      `(username . ,USER)
                                      `(password . ,PASS))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auth'ed HTTP Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get [path '()] #:params [params '()])
  (define the-url (oauth-reddit-url #:path path #:params params))
  (define res (http:fetch the-url
                          #:headers
                          (http:header-pairs (bearer-header))))
  (values the-url res))


(define (post-comment #:text text
                      #:thing_id thing_id)
  (http:fetch (oauth-reddit-url #:path '("api" "comment"))
              #:headers (http:header-pairs (bearer-header))
              #:response-type 'html
              #:method 'POST
              #:body `((api_type . "json")
                       (text . ,text)
                       (thing_id . ,thing_id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Url Builders ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Authorize
;; 2. Get access/refresh-token
;; 3. Refresh token
(define (authorize-url #:client-id [client-id ID]
                       #:redirect-uri [redirect REDIRECT-URI]
                       #:duration [duration "temporary"]
                       #:response-type [response-type "code"]
                       #:scope [scope SCOPES]
                       #:state [state ""])
  (reddit-url #:path '("api" "v1" "authorize")
              #:params `((client_id . ,client-id)
                         (response_type . ,response-type)
                         (state . ,state)
                         (redirect_uri . ,redirect)
                         (duration . ,duration)
                         (scope . ,scope))))


(define (reddit-url #:user [user #f]
                    #:path [a-path '()]
                    #:params [params '()])
  (url "https"
       user
       "www.reddit.com"
       #f
       #t
       (map (lambda (p) (path/param p '())) a-path)
       params
       #f))


(define (oauth-reddit-url #:path [a-path '()]
                          #:params [params '()])
  (url "https"
       #f
       "oauth.reddit.com"
       #f
       #t
       (map (lambda (p) (path/param p '())) a-path)
       params
       #f))