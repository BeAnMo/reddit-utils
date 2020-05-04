#lang racket
(require racket/date net/url)
(require (prefix-in http: "http-utils.rkt")
         "dotenv.rkt")


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

;;;; URLs
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

(define (post-comment #:text text
                      #:thing_id thing_id
                      #:headers [headers '()])
  (http:fetch (oauth-reddit-url #:path '("api" "comment"))
         #:headers headers
         #:response-type 'html
         #:method 'POST
         #:body `((api_type . "json")
                  (text . ,text)
                  (thing_id . ,thing_id))))


(define (auth-flow #:user-agent ua
                   #:client-id id
                   #:secret secret
                   #:username user
                   #:password pass)
  (define auth-header (http:basic-auth-header id secret))
  (define ua-header (http:header-pair 'User-Agent ua))
  
  (let ([token #f]
        [expires-at #f])
    
    (define (access-token)
      (cond
        [(or (false? token)
             (>= (- (date->seconds (current-date) 1000))
                 expires-at))
         (get-access-token)]
        [else token])) 
    
    (define (get-access-token)
      (define res (http:fetch (reddit-url #:path '("api" "v1" "access_token"))
                              #:method 'POST
                              #:response-type 'json
                              #:headers (http:header-pairs ua-header auth-header)
                              #:body (http:body-pairs '(grant_type . "password")
                                                      `(username . ,user)
                                                      `(password . ,pass))))
      (let ([access_token (hash-ref res 'access_token)]
            [expires_in (hash-ref res 'expires_in)])
        (begin
          (set!-values (token expires-at)
                       (values access_token
                               (+ (date->seconds (current-date))
                                  expires_in)))
          access_token)))

    (define (get path [params '()])
      (http:fetch (oauth-reddit-url #:path path #:params params)
                  #:headers
                  (http:header-pairs
                   `(Authorization . ,(string-append "Bearer "
                                                     (access-token))))))
    
    (lambda (cmd . args)
      (case cmd
        [(token) token]
        [(expires-at) expires-at]
        [(access-token) (access-token)]
        [(get) (apply get args)]
        [else (error (string-append (symbol->string cmd)
                                    " is invalid."))]))))
      
 ;; current flow
(define flow (auth-flow #:user-agent USER-AGENT
                        #:client-id ID
                        #:secret SECRET
                        #:username USER
                        #:password PASS))