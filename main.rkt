#lang racket/base

(require ffi/unsafe
         racket/format
         racket/port
         (for-syntax racket/base)
         zmq/unsafe)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------
;; version

(define (zmq-version)
  (let ([major (box -1)]
        [minor (box -1)]
        [patch (box -1)])
    (zmq_version major minor patch)
    (~a (unbox major) "." (unbox minor) "." (unbox patch))))

(provide zmq-version)

(module+ test
  (check-regexp-match "[0-9]+\\.[0-9]+\\.[0-9]+$" (zmq-version)))

;; ---------------------------------------------------------------------------
;; context

(struct context (ptr) #:property prop:cpointer 0)

(define (make-context)
  (context (zmq_ctx_new)))

(define (context-get name [ctx (current-context)])
  (zmq_ctx_get ctx name))

(define (context-set! name value [ctx (current-context)])
  (void (zmq_ctx_set ctx name value)))

(define (context-shutdown [ctx (current-context)])
  (void (zmq_ctx_shutdown ctx)))

(define (context-term [ctx (current-context)])
  (void (zmq_ctx_term ctx)))

(provide make-context context-get context-set! context-shutdown context-term)

;; ...

(define current-context (make-parameter (make-context)))

(define-syntax-rule (with-context ctx body ...)
  (parameterize ([current-context ctx]) body ...))

(define-syntax-rule (with-new-context body ...)
  (with-context (make-context)
    (begin0 (begin body ...) (context-shutdown) (context-term))))

(provide current-context with-context with-new-context)

(module+ test
  (with-new-context
    (check = (context-get 'IO_THREADS) 1)
    (check = (context-get 'MAX_SOCKETS) 1023)
    (check = (context-get 'SOCKET_LIMIT) 65535)
    (check = (context-get 'IPV6) 0)
    (check-equal? (context-set! 'IO_THREADS 3) (void))
    (check-equal? (context-set! 'MAX_SOCKETS 511) (void))
    (check-equal? (context-set! 'THREAD_PRIORITY 4095) (void))
    (check-equal? (context-set! 'THREAD_SCHED_POLICY 98) (void))
    (check-equal? (context-set! 'IPV6 1) (void))
    (check = (context-get 'IO_THREADS) 3)
    (check = (context-get 'MAX_SOCKETS) 511)
    (check = (context-get 'SOCKET_LIMIT) 65535)
    (check = (context-get 'IPV6) 1)))

;; ---------------------------------------------------------------------------
;; socket

(struct socket (ptr) #:property prop:cpointer 0)

(define (make-socket type [ctx (current-context)])
  (socket (zmq_socket ctx type)))

(define (socket-close [sock (current-socket)])
  (void (zmq_close sock)))

(define (socket-bind addr [sock (current-socket)])
  (void (zmq_bind sock (string->bytes/utf-8 addr))))

(define (socket-connect addr [sock (current-socket)])
  (void (zmq_connect sock (string->bytes/utf-8 addr))))

(provide make-socket socket-close socket-bind socket-connect)

;; -- getsockopt --

(define (getsockopt name [sock (current-socket)])
  (let ([val (zmq_getsockopt sock name)])
    (case name
      [(IDENTITY LAST_ENDPOINT) (bytes->string/utf-8 val)]
      [else val])))

(provide getsockopt)

(module+ test
  (define-syntax-rule (check-getsockopt sock name binop val)
    (check binop (getsockopt name sock) val))

  (define-syntax-rule (check-getsockopt-len sock name len)
    (check = (bytes-length (getsockopt name sock)) len))

  (with-new-context
   (let-socket ([P 'REP] [Q 'REQ])
     (check-equal? (socket-bind "tcp://*:6555" P) (void))
     (check-equal? (socket-connect "tcp://localhost:6555" Q) (void))
     (check-getsockopt P 'AFFINITY        =    0)
     (check-getsockopt P 'IDENTITY      equal? "")
     (check-getsockopt P 'FD              >    2)
     (check-getsockopt P 'EVENTS        equal? null)
     (check-getsockopt P 'TYPE          equal? 'REP)
     (check-getsockopt P 'LINGER          =    -1)
     (check-getsockopt P 'BACKLOG         =    100)
     (check-getsockopt P 'RCVHWM          =    1000)
     (check-getsockopt P 'LAST_ENDPOINT equal? "tcp://0.0.0.0:6555")
     (check-getsockopt Q 'LAST_ENDPOINT equal? "tcp://localhost:6555")
     (check-getsockopt P 'IMMEDIATE      eq?   #f)
     (check-getsockopt P 'IPV6           eq?   #f)
     (check-getsockopt-len P 'CURVE_PUBLICKEY      32)
     (check-getsockopt-len P 'CURVE_PUBLICKEY->BIN 32)
     (check-getsockopt-len P 'CURVE_PUBLICKEY->Z85 40)
     (check-getsockopt-len P 'CURVE_PRIVATEKEY      32)
     (check-getsockopt-len P 'CURVE_PRIVATEKEY->BIN 32)
     (check-getsockopt-len P 'CURVE_PRIVATEKEY->Z85 40)
     (check-getsockopt-len P 'CURVE_SERVERKEY      32)
     (check-getsockopt-len P 'CURVE_SERVERKEY->BIN 32)
     (check-getsockopt-len P 'CURVE_SERVERKEY->Z85 40)
     (check = (zmq_getsockopt P 'HANDSHAKE_IVL) 30000)
     )))

(define (setsockopt name val [sock (current-socket)])
  (void (zmq_setsockopt sock name val)))

(provide setsockopt)

;; syntax

(define current-socket (make-parameter #f))

(define-syntax-rule (with-socket sock body ...)
  (parameterize ([current-socket sock]) body ...))

(define-syntax-rule (with-new-socket type body ...)
  (parameterize ([current-socket (make-socket type)])
    (begin0 (begin body ...) (socket-close))))

(define-syntax-rule (let-socket ([name type] ...) body ...)
  (let ([name (make-socket type)] ...)
    (begin0 (begin body ...) (socket-close name) ...)))

(provide current-socket with-socket with-new-socket let-socket)

(module+ test
  (define-syntax-rule (check-setsockopt sock name binop val)
    (begin
      (check-equal? (setsockopt name val sock) (void))
      (check binop (getsockopt name sock) val)))

  (with-new-context
   (let-socket ([S 'REP])
     (check-setsockopt S 'AFFINITY = 3)
     (check-setsockopt S 'LINGER   = 30)
     (check-setsockopt S 'RCVHWM   = 500))))

;; ---------------------------------------------------------------------------
;; buffer

(define (socket-send-bytes buf [flags null] [sock (current-socket)])
  (zmq_send sock buf (bytes-length buf) flags))

(define (socket-send-string str [flags null] [sock (current-socket)])
  (socket-send-bytes (string->bytes/utf-8 str) flags sock))

(define (socket-recv-bytes [size 256] [flags null] [sock (current-socket)])
  (let* ([buf (make-bytes size)]
         [len (zmq_recv sock buf size flags)])
    (subbytes buf 0 len)))

(provide socket-send-bytes socket-send-string socket-recv-bytes)

(module+ test
  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (check-equal? (socket-bind "inproc://test1" P) (void))
      (check-equal? (socket-connect "inproc://test1" Q) (void))
      (check = (socket-send-bytes #"abc123" null Q) 6)
      (check-equal? (socket-recv-bytes 10 null P) #"abc123")
      (check = (socket-send-string "ok" null P) 2)
      (check-equal? (socket-recv-bytes 5 null Q) #"ok"))))

;; ---------------------------------------------------------------------------
;; message (internal)

(struct message (ptr) #:property prop:cpointer 0)

(define (make-message [size #f])
  (let ([msg (alloc-msg)])
    (if size (zmq_msg_init_size msg size) (zmq_msg_init msg))
    (message msg)))

(define (bytes->message buf)
  (let ([msg (alloc-msg)])
    (zmq_msg_init_data msg buf (bytes-length buf) cvoid cnull)
    (message msg)))

(define (message-size [msg (current-message)])
  (zmq_msg_size msg))

(define (message-data [msg (current-message)])
  (zmq_msg_data msg))

(define (set-message-data! buf [msg (current-message)])
  (memcpy (message-data msg) buf (bytes-length buf)))

(define (message-send [flags null]
                      [msg (current-message)] [sock (current-socket)])
  (zmq_msg_send msg sock flags))

(define (message-recv [flags null]
                      [msg (current-message)] [sock (current-socket)])
  (zmq_msg_recv msg sock flags))

(define (message-close [msg (current-message)])
  (void (zmq_msg_close msg)))

(define (message-copy from [to (current-message)])
  (void (zmq_msg_copy to from)))

(define (message-move from [to (current-message)])
  (void (zmq_msg_move to from)))

;; syntax

(define current-message (make-parameter #f))

(define-syntax-rule (with-message msg body ...)
  (parameterize ([current-message msg]) body ...))

(define-syntax-rule (with-new-message body ...)
  (parameterize ([current-message (make-message)])
    (begin0 (begin body ...) (message-close))))

(define-syntax-rule (with-new-message-size size body ...)
  (parameterize ([current-message (make-message size)])
    (begin0 (begin body ...) (message-close))))

(define-syntax-rule (with-new-message-data buf body ...)
  (parameterize ([current-message (bytes->message buf)])
    (begin0 (begin body ...) (message-close))))

(define-for-syntax (mdef->def stx)
  (syntax-case stx (empty size data)
    [(name empty) #'(name (make-message))]
    [(name size siz) #'(name (make-message siz))]
    [(name data buf) #'(name (bytes->message buf))]))

(define-for-syntax (mdef->close stx)
  (syntax-case stx (empty size data)
    [(name empty) #'(message-close name)]
    [(name size _) #'(message-close name)]
    [(name data _) #'(message-close name)]))

(define-syntax (let-message stx)
  (syntax-case stx ()
    [(_ defs body ...)
     (let* ([def-list (syntax-e #'defs)]
            [defs* (datum->syntax stx (map mdef->def def-list))]
            [closes (datum->syntax stx (map mdef->close def-list))])
       #`(let #,defs* (begin0 (begin body ...) #,@closes)))]))

(module+ test
  (with-new-message (check = (message-size) 0))
  (with-new-message-size 512 (check = (message-size) 512))
  (with-new-message-data #"abc"
    (check = (message-size) 3)
    (check-equal? (message-data) #"abc"))

  (with-message (datum->message '(98 76 vut))
    (check-equal? (message-data) #"(98 76 vut)"))

  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (check-equal? (socket-bind "inproc://message-test" P) (void))
      (check-equal? (socket-connect "inproc://message-test" Q) (void))

      (with-new-message
        (with-new-message-size 3
          (check-equal? (set-message-data! #"987") (void))
          (check-equal? (message-data) #"987")
          (with-socket Q (check = (message-send) 3)))
        (with-socket P (check = (message-recv) 3))
        (check-equal? (message-data) #"987"))

      (let-message ([M size 10] [N empty])
        (check = (message-size M) 10)
        (check = (message-size N) 0)
        (let ([buf (message-data M)])
          (bytes-fill! buf 0)
          (bytes-copy! buf 0 #"987zyx"))
        (check-equal? (message-data M) #"987zyx\0\0\0\0")
        (check-equal? (message-data N) #"")
        (check = (message-send null M P) 10)
        (check = (message-recv null N Q) 10)
        (check-equal? (message-data N) #"987zyx\0\0\0\0")
        (check-equal? (message-data M) #""))

      (let-message ([M data #"654mon"] [N1 empty] [N2 empty])
        (check = (message-size M) 6)
        (check = (message-size N1) 0)
        (check = (message-size N2) 0)
        (check-equal? (message-data M) #"654mon")
        (check-equal? (message-data N1) #"")
        (check-equal? (message-data N2) #"")
        (check-equal? (message-copy M N1) (void))
        (check = (message-size M) 6)
        (check = (message-size N1) 6)
        (check = (message-size N2) 0)
        (check-equal? (message-data M) #"654mon")
        (check-equal? (message-data N1) #"654mon")
        (check-equal? (message-data N2) #"")
        (check-equal? (message-move M N2) (void))
        (check = (message-size M) 0)
        (check = (message-size N1) 6)
        (check = (message-size N2) 6)
        (check-equal? (message-data M) #"")
        (check-equal? (message-data N1) #"654mon")
        (check-equal? (message-data N2) #"654mon"))
      )))

;; ---------------------------------------------------------------------------
;; messaging (external)

(define (socket-send obj [flags null] [sock (current-socket)])
  (message-send flags (datum->message obj) sock))

(define (datum->message obj)
  (bytes->message (with-output-to-bytes (λ () (write obj)))))

(define (socket-recv [flags null] [sock (current-socket)])
  (let-message ([msg empty])
    (message-recv flags msg sock)
    (with-input-from-bytes (message-data msg) read)))

(provide socket-send socket-recv)

(module+ test
  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (socket-bind "inproc://messaging-test" P)
      (socket-connect "inproc://messaging-test" Q)
      (check = (socket-send '(1 (2 . 3) four) null Q) 16)
      (check-equal? (socket-recv null P) '(1 (2 . 3) four)))))

;; ---------------------------------------------------------------------------
;; concurrency

(module+ test
  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (socket-bind "inproc://concurrency-test" P)
      (socket-connect "inproc://concurrency-test" Q)
      (thread (λ () (sleep 30) (socket-send 'ok null Q)))
      (check-equal? (sync P) 'ok))))
