#lang racket

(require irc
         json
         net/http-client
         net/uri-codec
         (planet neil/html-parsing:3:0)
         net/url
         racket/sandbox)

;;;;;;;;;;;;;
;; Structures
;;;;;;;;;;;;;

(struct message (connection source command rest text original))

;;;;;;;
;; Util
;;;;;;;

(define/contract (parse-status-code status)
  (-> bytes? integer?)
  (string->number (cadr (string-split (bytes->string/utf-8 status)))))

(define/contract (create-logger topic level)
  (-> symbol? symbol? logger?)
  (define lg (make-logger topic))
  (define rc (make-log-receiver lg level))
  (void
   (thread
    (λ () (let loop ()
            (define v (sync rc))
            (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1))
            (loop)))))
  lg)

(define/contract (verbosity->symbol verbosity)
  (-> boolean? symbol?)
  (if verbosity 'debug 'info))

(define/contract (truncate s [len 500])
  (->* (string?) (exact-positive-integer?) string?)
  (if (<= (string-length s) len)
      s
      (string-append (substring s 0 (sub1 (min (string-length s) len))) "…")))

(define/contract (parse-headers headers)
  (-> (listof bytes?) (or/c (hash/c string? string?)))
  (for/hash ([h headers])
    (apply values (map bytes->string/utf-8 (rest (regexp-match "([^:]*): (.*)" h))))))


(define/contract (extract-html-title h)
  (-> list? (or/c false? string?))
  (map (λ (element)
         (extract-html-title element))))

(define-syntax command
  (syntax-rules ()
    [(command trigger code ...)
     (hash 'event trigger
           'body (λ (m) code ...))]))

;;;;;;;;;;;
;; Commands
;;;;;;;;;;;


(define/contract (read-url m)
  (-> message? void?)
  (with-handlers ([exn:fail? (λ (e) (reply m (format "An error occured: ~a" e)))])
    (define url (string->url (message-rest m)))
    (define-values (status byte-headers content) (http-sendrecv/url url #:method "HEAD"))
    (define code (parse-status-code status))
    (define headers (parse-headers byte-headers))
    (reply m (string-join (flatten (list "[URL]"
                                         (cond [(not (= code 200)) (~a code)]
                                               [else (list (hash-ref headers "Content-Type" "(no MIME type)")
                                                           (hash-ref headers "Content-Length" "(no size)"))])))))))

(define/contract (wiki m)
  (-> message? void?)
  (define hc (http-conn-open "en.wikipedia.org" #:ssl? #t))
  (define path (string-append "/w/api.php?action=opensearch&format=json&redirects=resolve&search="
                              (uri-encode (message-rest m))))
  (http-conn-send! hc path)
  (define-values (status headers port) (http-conn-recv! hc))
  (define jo (read-json port))
  (http-conn-close! hc)
  (reply m (if (eq? (parse-status-code status) 200)
               (with-handlers ([exn:fail:contract? (λ (e) "No results found.")]
                               [exn? (λ (e) (exn-message e))])
                 (string-append (first (third jo))
                                " — "
                                (string-replace (first (fourth jo)) "en.wikipedia" "enwp" #:all? #f)))
               "Something went wrong!")))

(define/contract (bot-version m)
  (-> message? void?)
  (reply m "Reporting in! [Racket] https://github.com/winny-/moistbot"))

(define/contract (eval-lang m [lang 'racket])
  ((message?) (symbol?) . ->* . void?)
  (with-handlers [(exn:fail? (λ (e) (reply m (format "Sandbox terminated: ~a" (string-replace (exn-message e) "\n" " ")))))]
             (reply m (~s ((make-evaluator lang) (message-rest m))))))

(define/contract (do-rpn m)
  (message? . -> . void?)
  (define (rpn lst [stack (list)])
    (if (empty? lst)
        stack
        #;(if (empty? stack) 0 (take-right stack 1))
        (match (car lst)
          [(? number? n) (rpn (cdr lst) (append stack (list n)))]
          [(? symbol? sym) (define-values (l r) (split-at stack (- (length stack) 2)))
           (rpn (cdr lst) (append l (list (apply (symbol->op sym) r))))])))
  (define (symbol->op sym)
    (match sym
      ['+ +]
      ['- -]
      ['* *]
      ['/ /]))
  (with-handlers [(exn:fail? (λ (e) (reply m (format "Bad expression (~a)" (string-replace (exn-message e) "\n" " ")))))]
    (reply m (~s (rpn (map (λ (s) (define n (string->number s)) (or n (string->symbol s))) (string-split (message-rest m))))))))

;(command "echo" (reply m (message-rest m)))

(define commands
  (hash "echo" (λ (m) (reply m (message-rest m)))
        "wiki" wiki
        "read-url" read-url
        "eval" eval-lang
        "eval-typed" (curryr eval-lang 'typed/racket)
        "help" (λ (m) (reply m (string-append "Commands available are: " (string-join (hash-keys commands)))))
        "rpn" do-rpn))

;;;;;;;;;;;;;;;;;;;
;; IRC Bot routines
;;;;;;;;;;;;;;;;;;;

(define/contract (reply m text)
  (-> message? string? void?)
  (log-debug (format "OUT> ~a: ~a" (message-source m) text))
  (irc-send-message (message-connection m) (message-source m) text)
  (void))

(define/contract (find-trigger msg dispatch-table)
  (irc-message? hash? . -> . hash?)
  (define (want-trigger? trig)
    (match trig
      [(? string?) (string=? (string-downcase (message-command msg)) trig)]
      [(? regexp?) (regexp-match (message-text msg))]
      [_ #f]))
  (define h (for/hash ([(k v) (in-hash dispatch-table)]
                       #:when (want-trigger? k))
              (values k v)))
  (and (not (hash-empty? h)) h))

(define/contract (handle-irc-message irc-msg connection config)
  (-> irc-message? irc-connection? hash? void?)
  (let* ([prefix (hash-ref config 'prefix)]
         [params (irc-message-parameters irc-msg)]
         [source (first params)]
         [text (string-trim (second params) #:left? #f)]
         [parts (string-split text #:repeat? #f)]
         [is-command (string-prefix? (first parts) prefix)]
         [command (if is-command (string-trim (first parts) prefix #:right? #f) #f)]
         [mtext (if is-command (string-join (rest parts)) text)]
         [m (message connection source command mtext text irc-msg)])
    (cond
      [is-command (log-debug "Is a command: '~a'" command)
                  (thread (thunk ((hash-ref commands command (λ () identity)) m)))]
      [(string=? (first parts) ".bots") (bot-version m)]))
  (void))


(define/contract (bot-loop connection config)
  (-> irc-connection? hash? void?)
  (define inc (irc-connection-incoming connection))
  (let loop ()
    (let* ([msg (sync inc)]
           [command (irc-message-command msg)])
      (log-debug (format " IN> ~a" (irc-message-content msg)))
      (match (string-downcase command)
        ["privmsg" (displayln (log-debug (format "Handling privmsg ~a" msg)))
         (handle-irc-message msg connection config)]
        [_ #f])
      (loop)))
    (void))

(define/contract (bot-main config #:verbose? [verbose #f])
  (-> hash? #:verbose? boolean? void?)
  (current-logger (create-logger 'irc (verbosity->symbol verbose)))
  (define nick (hash-ref config 'nick))
  (define-values (connection ready)
    (irc-connect (hash-ref config 'server)
                 (hash-ref config 'port 6667)
                 nick
                 nick
                 nick
                 #:ssl (hash-ref config 'ssl #f)
                 #:return-eof #t))

  (let ([timeout (hash-ref config 'timeout 5)])
    (unless (sync/timeout timeout ready)
      (log-error (format "Connection not ready after ~a seconds." timeout))
      (exit 1)))

  (log-info "Connection ready.")

  (map (curry irc-join-channel connection) (hash-ref config 'channels))

  (bot-loop connection config)
  (void))

;;;;;;;;;;;;;;;
;; Main program
;;;;;;;;;;;;;;;

(define config-path (make-parameter "config.rktd"))
(define verbose (make-parameter #f))

(define (main)
  (command-line #:program "moistbot"
                #:once-each
                [("-c" "--config") c "Config file" (config-path c)]
                [("-v" "--verbose") "Verbose" (verbose #t)])
  (define config (file->value (config-path)))
  (bot-main config #:verbose? (verbose)))

(module+ main (main))
