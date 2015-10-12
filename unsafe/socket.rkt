#lang racket/base

(provide zmq_socket zmq_close zmq_bind zmq_connect)

(provide zmq_getsockopt)

(provide zmq_setsockopt)

(provide zmq_send zmq_recv zmq_send_const)

(provide zmq_msg_init zmq_msg_init_size zmq_msg_init_data cvoid cnull
         zmq_msg_size zmq_msg_data zmq_msg_send zmq_msg_recv zmq_msg_close
         zmq_msg_copy zmq_msg_move alloc-msg cvoid cnull)

(provide pollitems pollitems-revents zmq_poll)

(require ffi/unsafe
         ffi/unsafe/cvector
         zmq/private/ext
         zmq/unsafe/context
         zmq/unsafe/define
         [for-syntax racket/base
                     racket/syntax])

;; ---------------------------------------------------------------------------
;; socket

(define _socket (_cpointer 'socket))

(define _socket_type
  (_enum '(PAIR   = 0
           PUB    = 1
           SUB    = 2
           REQ    = 3
           REP    = 4
           DEALER = 5
           ROUTER = 6
           PULL   = 7
           PUSH   = 8
           XPUB   = 9
           XSUB   = 10
           STREAM = 11)
         _fixint))

(define-zmq zmq_socket
  (_efun _context _socket_type
         -> (obj : _socket)
         -> (or obj (croak 'zmq_socket))))

(define-zmq-check zmq_close _socket)
(define-zmq-check zmq_bind _socket _bytes)
(define-zmq-check zmq_connect _socket _bytes)

;; -- zmq_getsockopt --

(define _get_socket_option
  (_enum '(AFFINITY                 =  4
           IDENTITY                 =  5
           FD                       = 14
           EVENTS                   = 15
           TYPE                     = 16
           LINGER                   = 17
           BACKLOG                  = 19
           RCVHWM                   = 24
           LAST_ENDPOINT            = 32
           IMMEDIATE                = 39
           IPV6                     = 42
           CURVE_PUBLICKEY          = 48
           CURVE_PUBLICKEY->BIN     = 48
           CURVE_PUBLICKEY->Z85     = 48
           CURVE_PRIVATEKEY         = 49
           CURVE_PRIVATEKEY->BIN    = 49
           CURVE_PRIVATEKEY->Z85    = 49
           CURVE_SERVERKEY          = 50
           CURVE_SERVERKEY->BIN     = 50
           CURVE_SERVERKEY->Z85     = 50
           ;; GSSAPI_SERVER            = 62
           ;; GSSAPI_PRINCIPAL         = 63
           ;; GSSAPI_SERVICE_PRINCIPAL = 64
           ;; GSSAPI_PLAINTEXT         = 65
           HANDSHAKE_IVL            = 66
           )))

(define-syntax (define-getsockopt->ctype stx)
  (syntax-case stx ()
    [(_ type)
     (with-syntax ([alias (format-id #'type "getsockopt->~a" #'type)]
                   [_type (format-id #'type "_~a" #'type)])
       #'(define-zmq-alias [alias zmq_getsockopt]
           (_efun _socket _get_socket_option
                  (val : (_ptr o _type))
                  (siz : (_ptr io _size) = (ctype-sizeof _type))
                  -> (rc : _fixint)
                  -> (if (= rc -1) (croak 'zmq_getsockopt) val))))]))

(define-getsockopt->ctype uint64)
(define-getsockopt->ctype fixint)
(define-getsockopt->ctype bool)

(define-zmq-alias [getsockopt->char* zmq_getsockopt]
  (_efun _socket _get_socket_option
         (val : _bytes)
         (siz : (_ptr io _size) = (bytes-length val))
         -> (rc : _fixint)
         -> (if (= rc -1) (croak 'zmq_getsockopt) val)))

(define-zmq-alias [getsockopt->charN zmq_getsockopt]
  (_efun _socket _get_socket_option
         (val : _bytes)
         (siz : (_ptr io _size) = (bytes-length val))
         -> (rc : _fixint)
         -> (if (= rc -1)
                (croak 'zmq_getsockopt)
                (values val siz))))

(define (getsockopt->string obj name [size 256])
  (define-values (buf len) (getsockopt->charN obj name (make-bytes size)))
  (if (= len 0) #"" (subbytes buf 0 (sub1 len))))

(define (getsockopt->bin obj name)
  (getsockopt->char* obj name (make-bytes 32)))

(define (getsockopt->z85 obj name)
  (subbytes (getsockopt->char* obj name (make-bytes 41)) 0 40))

(define _event_state (_bitmask '(POLLIN POLLOUT POLERR)))

(define _event_state_short (_bitmask '(POLLIN POLLOUT POLERR) _short))

(define (zmq_getsockopt obj name)
  (case name
    [(AFFINITY     ) (getsockopt->uint64 obj name)]
    [(IDENTITY     ) (getsockopt->string obj name)]
    [(FD           ) (getsockopt->fixint obj name)]
    [(EVENTS       ) (cast (getsockopt->fixint obj name) _fixint _event_state)]
    [(TYPE         ) (cast (getsockopt->fixint obj name) _fixint _socket_type)]
    [(LINGER       ) (getsockopt->fixint obj name)]
    [(BACKLOG      ) (getsockopt->fixint obj name)]
    [(RCVHWM       ) (getsockopt->fixint obj name)]
    [(LAST_ENDPOINT) (getsockopt->string obj name)]
    [(IMMEDIATE    ) (getsockopt->bool obj name)]
    [(IPV6         ) (getsockopt->bool obj name)]
    [(CURVE_PUBLICKEY     ) (getsockopt->bin obj 'CURVE_PUBLICKEY)]
    [(CURVE_PUBLICKEY->BIN) (getsockopt->bin obj 'CURVE_PUBLICKEY)]
    [(CURVE_PUBLICKEY->Z85) (getsockopt->z85 obj 'CURVE_PUBLICKEY)]
    [(CURVE_PRIVATEKEY     ) (getsockopt->bin obj 'CURVE_PRIVATEKEY)]
    [(CURVE_PRIVATEKEY->BIN) (getsockopt->bin obj 'CURVE_PRIVATEKEY)]
    [(CURVE_PRIVATEKEY->Z85) (getsockopt->z85 obj 'CURVE_PRIVATEKEY)]
    [(CURVE_SERVERKEY     ) (getsockopt->bin obj 'CURVE_SERVERKEY)]
    [(CURVE_SERVERKEY->BIN) (getsockopt->bin obj 'CURVE_SERVERKEY)]
    [(CURVE_SERVERKEY->Z85) (getsockopt->z85 obj 'CURVE_SERVERKEY)]
    [(HANDSHAKE_IVL) (getsockopt->fixint obj name)]
    ))

(module+ test
  (require rackunit)

  (define-syntax-rule (check-getsockopt obj name binop val)
    (check binop (zmq_getsockopt obj name) val))

  (define-syntax-rule (check-getsockopt-len obj name len)
    (check = (bytes-length (zmq_getsockopt obj name)) len))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)])
    (check = (zmq_bind P #"tcp://*:6555") 0)
    (check = (zmq_connect Q #"tcp://localhost:6555") 0)
    (check-getsockopt P 'AFFINITY        =    0)
    (check-getsockopt P 'IDENTITY      equal? #"")
    (check-getsockopt P 'FD              >    2)
    (check-getsockopt P 'EVENTS        equal? null)
    (check-getsockopt P 'TYPE          equal? 'REP)
    (check-getsockopt P 'LINGER          =    -1)
    (check-getsockopt P 'BACKLOG         =    100)
    (check-getsockopt P 'RCVHWM          =    1000)
    (check-getsockopt P 'LAST_ENDPOINT equal? #"tcp://0.0.0.0:6555")
    (check-getsockopt Q 'LAST_ENDPOINT equal? #"tcp://localhost:6555")
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
    (check = (zmq_close P) 0)
    (check = (zmq_close Q) 0)
    ))

;; -- zmq_setsockopt --

(define _set_socket_option
  (_enum '(AFFINITY = 4
           LINGER   = 17
           RCVHWM   = 24
           )))

(define-syntax (define-setsockopt->ctype stx)
  (syntax-case stx ()
    [(_ type)
     (with-syntax ([alias (format-id #'type "setsockopt->~a" #'type)]
                   [_type (format-id #'type "_~a" #'type)])
       #'(define-zmq-alias [alias zmq_setsockopt]
           (_efun _socket _set_socket_option
                  (val : (_ptr i _type))
                  (_size = (ctype-sizeof _type))
                  -> (rc : _fixint)
                  -> (if (= rc -1) (croak 'zmq_setsockopt) rc))))]))

(define-setsockopt->ctype uint64)
(define-setsockopt->ctype fixint)

(define (zmq_setsockopt obj name val)
  (case name
    [(AFFINITY) (setsockopt->uint64 obj name val)]
    [(LINGER  ) (setsockopt->fixint obj name val)]
    [(BACKLOG ) (setsockopt->fixint obj name val)]
    [(RCVHWM  ) (setsockopt->fixint obj name val)]
    ))

(module+ test
  (define-syntax-rule (check-setsockopt obj name binop val)
    (begin
      (check = (zmq_setsockopt obj name val) 0)
      (check binop (zmq_getsockopt obj name) val)))

  (let* ([C (zmq_ctx_new)]
         [S (zmq_socket C 'REP)])
    (check-setsockopt S 'AFFINITY = 3)
    (check-setsockopt S 'LINGER   = 30)
    (check-setsockopt S 'RCVHWM   = 500)
    ))

;; ---------------------------------------------------------------------------
;; buffer

(define _send_flags
  (_bitmask '(DONTWAIT = 1
              SNDMORE  = 2)))

(define _recv_flags
  (_bitmask '(DONTWAIT = 1)))

(define-zmq-check zmq_send _socket _bytes _size _send_flags)
(define-zmq-check zmq_recv _socket _bytes _size _recv_flags)
(define-zmq-check zmq_send_const _socket _bytes _size _send_flags)

(module+ test
  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [buf (make-bytes 10)])
    (check = (zmq_bind P #"inproc://test1") 0)
    (check = (zmq_connect Q #"inproc://test1") 0)
    (check = (zmq_send Q #"abc123" 6 null) 6)
    (check = (zmq_recv P buf 10 null) 6)
    (check-equal? buf #"abc123\0\0\0\0")
    (check = (zmq_send_const P #"ok" 2 null) 2)
    (check = (zmq_recv Q buf 10 null) 2)
    (check-equal? buf #"okc123\0\0\0\0")
    (check = (zmq_close Q) 0)
    (check = (zmq_close P) 0)
    (check = (zmq_ctx_shutdown C) 0)
    (check = (zmq_ctx_term C) 0)
    ))

;; ---------------------------------------------------------------------------
;; message

(define-cstruct _msg ([_ (_array _uint8 64)]))

(define-zmq-check zmq_msg_init _msg-pointer)
(define-zmq-check zmq_msg_init_size _msg-pointer _size)

(define-zmq-check zmq_msg_init_data
  _msg-pointer _pointer _size (_fun _pointer _pointer -> _void) _pointer)

(define-zmq zmq_msg_size (_fun _msg-pointer -> _size))

(define-zmq zmq_msg_data
  (_fun (msg : _msg-pointer)
        -> (buf : _pointer)
        -> (make-sized-byte-string buf (zmq_msg_size msg))))

(define-zmq-check zmq_msg_send _msg-pointer _socket _send_flags)
(define-zmq-check zmq_msg_recv _msg-pointer _socket _recv_flags)
(define-zmq-check zmq_msg_close _msg-pointer)
(define-zmq-check zmq_msg_copy _msg-pointer _msg-pointer)
(define-zmq-check zmq_msg_move _msg-pointer _msg-pointer)

(define (alloc-msg)
  (let ([msg (malloc _msg 'atomic)])
    (set-cpointer-tag! msg msg-tag)
    msg))

(define cvoid (λ _ (void)))
(define cnull (cast 0 _uint64 _pointer))

(module+ test

  (let ([M1 (alloc-msg)]
        [M2 (alloc-msg)]
        [M3 (alloc-msg)])
    (check = (zmq_msg_init M1) 0)
    (check = (zmq_msg_init_size M2 512) 0)
    (check = (zmq_msg_init_data M3 #"abc" 3 cvoid cnull) 0)
    (check = (zmq_msg_size M1) 0)
    (check = (zmq_msg_size M2) 512)
    (check = (zmq_msg_size M3) 3)
    (check-equal? (zmq_msg_data M3) #"abc")
    (check = (zmq_msg_close M1) 0)
    (check = (zmq_msg_close M2) 0)
    (check = (zmq_msg_close M3) 0))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [M1 (alloc-msg)]
         [M2 (alloc-msg)])
    (check = (zmq_bind P #"inproc://msg-test") 0)
    (check = (zmq_connect Q #"inproc://msg-test") 0)
    (check = (zmq_msg_init_size M1 3) 0)
    (memcpy (zmq_msg_data M1) #"987" 3)
    (check = (zmq_msg_init M2) 0)
    (check = (zmq_msg_send M1 Q null) 3)
    (check = (zmq_msg_recv M2 P null) 3)
    (check-equal? (zmq_msg_data M2) #"987"))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [M1 (alloc-msg)]
         [M2 (alloc-msg)])
    (check = (zmq_msg_init M1) 0)
    (check = (zmq_msg_init_size M2 10) 0)
    (check = (zmq_msg_size M1) 0)
    (check = (zmq_msg_size M2) 10)
    (check = (zmq_bind P #"inproc://test2") 0)
    (check = (zmq_connect Q #"inproc://test2") 0)
    (let ([buf (zmq_msg_data M2)])
      (bytes-fill! buf 0)
      (bytes-copy! buf 0 #"987zyx")
      (check = (zmq_msg_send M2 Q null) 10)
      (check = (zmq_msg_recv M1 P null) 10)
      (check = (zmq_msg_size M1) 10)
      (check-equal? (zmq_msg_data M1) #"987zyx\0\0\0\0")
      (check-equal? (zmq_msg_data M2) #""))
    (check = (zmq_msg_close M1) 0)
    (check = (zmq_msg_close M2) 0))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [M3 (alloc-msg)]
         [M4 (alloc-msg)])
    (check = (zmq_msg_init_data M3 #"654mon" 6 cvoid cnull) 0)
    (check = (zmq_msg_init M4) 0)
    (check = (zmq_msg_size M3) 6)
    (check = (zmq_msg_size M4) 0)
    (check = (zmq_bind P #"inproc://test2") 0)
    (check = (zmq_connect Q #"inproc://test2") 0)
    (check = (zmq_send_const Q #"ok" 2 null) 2)
    (let ([buf (make-bytes 5)])
      (bytes-fill! buf 0)
      (check = (zmq_recv P buf 5 null) 2)
      (check-equal? buf #"ok\0\0\0"))
    (check = (zmq_msg_close M3) 0)
    (check = (zmq_msg_close M4) 0))

  (let ([M5 (alloc-msg)]
        [M6 (alloc-msg)]
        [M7 (alloc-msg)])
    (check = (zmq_msg_init_data M5 #"567vut" 6 cvoid cnull) 0)
    (check = (zmq_msg_init M6) 0)
    (check = (zmq_msg_init M7) 0)
    (check = (zmq_msg_copy M6 M5) 0)
    (check-equal? (zmq_msg_data M6) (zmq_msg_data M5))
    (check-equal? (zmq_msg_data M6) #"567vut")
    (check = (zmq_msg_move M7 M6) 0)
    (check-equal? (zmq_msg_data M7) (zmq_msg_data M5))
    (check-equal? (zmq_msg_data M7) #"567vut")
    (check = (zmq_msg_close M5) 0)
    (check = (zmq_msg_close M6) 0)
    (check = (zmq_msg_close M7) 0)))

;; ----

  ;; (let* ([C (zmq_ctx_new)]
  ;;        [S (zmq_socket C 'REP)])
  ;;   (check-equal? (zmq_bind S #"tcp://*:6555") (void))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PLAINTEXT)))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PRINCIPAL)))
  ;;   ))

;; ---------------------------------------------------------------------------
;; events

(define-cstruct _pollitem ([socket  _socket]
                           [fd      _fixint]
                           [events  _event_state_short]
                           [revents _event_state_short]))

(define-syntax-rule (pollitems [sock flags] ...)
  (cvector _pollitem (make-pollitem sock 0 flags null) ...))

(define (pollitems-revents items k)
  (pollitem-revents (cvector-ref items k)))

(define (zmq_poll items nitems timeout)
  (zmq_poll* (cvector-ptr items) nitems timeout))

(module+ test
  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [s (thread (λ ()
                      (check = (zmq_bind P #"inproc://poll-test") 0)
                      (let ([items (pollitems [P 'POLLIN])]
                            [buf (make-bytes 10)])
                        (bytes-fill! buf 0)
                        (check = (zmq_poll items 1 -1) 1)
                        (check = (zmq_recv P buf 10 null) 5)
                        (check-equal? buf #"PING!\0\0\0\0\0"))))]
         [c (thread (λ ()
                      (check = (zmq_connect Q #"inproc://poll-test") 0)
                      (sleep 0.1)
                      (check = (zmq_send_const Q #"PING!" 5 null) 5)))])
    (thread-wait s)
    (thread-wait c)))