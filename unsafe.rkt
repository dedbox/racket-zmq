#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         ffi/unsafe
         ffi/unsafe/define
         racket/function)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------
;; library loader

(define zmq-lib (ffi-lib "libzmq"))

(define-ffi-definer define-zmq zmq-lib)

(define-syntax-rule (define-zmq-alias (alias actual) def)
  (define alias (get-ffi-obj 'actual zmq-lib def)))

(define-syntax-rule (define-zmq-check name args ...)
  (define-zmq name
    (_efun args ...
           -> (rc : _fixnum)
           -> (when (= rc -1) (croak 'name)))))

(define-syntax-rule (define-zmq-check-ret name args ...)
  (define-zmq name
    (_efun args ...
           -> (rc : _fixnum)
           -> (if (= rc -1) (croak 'name) rc))))

;; ---------------------------------------------------------------------------
;; error handling

(define-syntax-rule (_efun args ...)
  (_fun #:save-errno 'posix args ...))

(define-zmq zmq_strerror (_fun _fixint -> _string))

(define (croak caller)
  (error caller (zmq_strerror (saved-errno))))

;; ---------------------------------------------------------------------------
;; version

(define-zmq zmq_version
  (_fun (major : (_ptr o _fixint))
        (minor : (_ptr o _fixint))
        (patch : (_ptr o _fixint))
        -> _void
        -> (values major minor patch)))

(provide zmq_version)

;; ---------------------------------------------------------------------------
;; context

(struct context (obj) #:property prop:cpointer 0)

(define _context (_cpointer 'context))

(define-zmq zmq_ctx_new
  (_efun -> (obj : _context)
         -> (if obj (context obj) (croak 'zmq_ctx_new))))

(define _ctx_get_option
  (_enum '(IO_THREADS   = 1
           MAX_SOCKETS  = 2
           SOCKET_LIMIT = 3
           IPV6         = 42)
         _fixint))

(define _ctx_set_option
  (_enum '(IO_THREADS          = 1
           MAX_SOCKETS         = 2
           THREAD_PRIORITY     = 3
           THREAD_SCHED_POLICY = 4
           IPV6                = 42)
         _fixint))

(define-zmq-check-ret zmq_ctx_get _context _ctx_get_option)
(define-zmq-check zmq_ctx_set _context _ctx_set_option _fixint)
(define-zmq-check zmq_ctx_shutdown _context)
(define-zmq-check zmq_ctx_term _context)

(provide (struct-out context)
         zmq_ctx_new zmq_ctx_get zmq_ctx_set zmq_ctx_shutdown zmq_ctx_term)

(module+ test
  (let* ([C (zmq_ctx_new)])
    (check = (zmq_ctx_get C 'IO_THREADS) 1)
    (check = (zmq_ctx_get C 'MAX_SOCKETS) 1023)
    (check = (zmq_ctx_get C 'SOCKET_LIMIT) 65535)
    (check = (zmq_ctx_get C 'IPV6) 0)

    (check-equal? (zmq_ctx_set C 'IO_THREADS 3) (void))
    (check-equal? (zmq_ctx_set C 'MAX_SOCKETS 511) (void))
    (check-equal? (zmq_ctx_set C 'THREAD_PRIORITY 4095) (void))
    (check-equal? (zmq_ctx_set C 'THREAD_SCHED_POLICY 98) (void))
    (check-equal? (zmq_ctx_set C 'IPV6 1) (void))

    (check = (zmq_ctx_get C 'IO_THREADS) 3)
    (check = (zmq_ctx_get C 'MAX_SOCKETS) 511)
    (check = (zmq_ctx_get C 'SOCKET_LIMIT) 65535)
    (check = (zmq_ctx_get C 'IPV6) 1)

    (check-equal? (zmq_ctx_shutdown C) (void))
    (check-equal? (zmq_ctx_term C) (void))))

;; ---------------------------------------------------------------------------
;; socket

(struct socket (obj) #:property prop:cpointer 0)

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
         -> (if obj (socket obj) (croak 'zmq_socket))))

(define-zmq-check zmq_close _socket)
(define-zmq-check zmq_bind _socket _bytes)
(define-zmq-check zmq_connect _socket _bytes)

(provide (struct-out socket) zmq_socket zmq_close zmq_bind zmq_connect)

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

;; (define-zmq-alias [getsockopt->chars zmq_getsockopt]
;;   (_efun _socket _get_socket_option
;;          (val : (_ptr o _byte))
;;          (siz : (_ptr io _size) = (ctype-sizeof _byte))
;;          -> (rc : _fixint)
;;          -> (if (= rc -1) (croak 'zmq_getsockopt) val)))

(define _event_state (_bitmask '(POLLIN POLLOUT POLERR)))

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

(provide zmq_getsockopt)

(module+ test
  (define-syntax-rule (check-getsockopt obj name binop val)
    (check binop (zmq_getsockopt obj name) val))

  (define-syntax-rule (check-getsockopt-len obj name len)
    (check = (bytes-length (zmq_getsockopt obj name)) len))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)])
    (check-equal? (zmq_bind P #"tcp://*:6555") (void))
    (check-equal? (zmq_connect Q #"tcp://localhost:6555") (void))

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

    (check-true (= (zmq_getsockopt P 'HANDSHAKE_IVL) 30000))

    (check-equal? (zmq_close P) (void))
    (check-equal? (zmq_close Q) (void))))

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
                  -> (when (= rc -1) (croak 'zmq_setsockopt)))))]))

(define-setsockopt->ctype uint64)
(define-setsockopt->ctype fixint)

(define (zmq_setsockopt obj name val)
  (case name
    [(AFFINITY) (setsockopt->uint64 obj name val)]
    [(LINGER  ) (setsockopt->fixint obj name val)]
    [(BACKLOG ) (setsockopt->fixint obj name val)]
    [(RCVHWM  ) (setsockopt->fixint obj name val)]
    ))

(provide zmq_setsockopt)

(module+ test
  (define-syntax-rule (check-setsockopt obj name binop val)
    (begin
      (check equal? (zmq_setsockopt obj name val) (void))
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

(define-zmq-check-ret zmq_send
  _socket (msg : _bytes) (_size = (bytes-length msg)) _send_flags)

(define-zmq-check-ret zmq_recv
  _socket (msg : _bytes) (_size = (bytes-length msg)) _recv_flags)

(define-zmq-check-ret zmq_send_const
  _socket (msg : _bytes) (_size = (bytes-length msg)) _send_flags)

(provide zmq_send zmq_recv zmq_send_const)

(module+ test
  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [buf (make-bytes 10)])
    (check-equal? (zmq_bind P #"inproc://test1") (void))
    (check-equal? (zmq_connect Q #"inproc://test1") (void))
    (check = (zmq_send Q #"abc123" null) 6)
    (check = (zmq_recv P buf null) 6)
    (check-equal? buf #"abc123\0\0\0\0")
    (check-equal? (zmq_close Q) (void))
    (check-equal? (zmq_close P) (void))
    (check-equal? (zmq_ctx_shutdown C) (void))
    (check-equal? (zmq_ctx_term C) (void))))

;; ---------------------------------------------------------------------------
;; message

(struct message (obj) #:property prop:cpointer 0)

(define (make-message)
  (let ([obj (malloc _msg 'raw)])
    (set-cpointer-tag! obj msg-tag)
    (message obj)))

(define (set-message-data! M buf)
  (memcpy (zmq_msg_data M) buf (min (bytes-length buf) (zmq_msg_size M))))

(define-cstruct _msg ([_ (_array _uint8 64)]))

(define-zmq-check zmq_msg_init _msg-pointer)
(define-zmq-check zmq_msg_init_size _msg-pointer _size)

(define-zmq-check zmq_msg_init_data
  _msg-pointer _pointer _size
  ((_fun _pointer _pointer -> _void) = (λ _ (void)))
  (_pointer = (cast 0 _uint64 _pointer)))

(define-zmq zmq_msg_size (_fun _msg-pointer -> _size))
(define-zmq zmq_msg_data (_fun _msg-pointer -> _bytes))
(define-zmq-check-ret zmq_msg_send _msg-pointer _socket _send_flags)
(define-zmq-check-ret zmq_msg_recv _msg-pointer _socket _recv_flags)
(define-zmq-check zmq_msg_close _msg-pointer)

(provide zmq_msg_init zmq_msg_init_size zmq_msg_init_data
         zmq_msg_size zmq_msg_data zmq_msg_send zmq_msg_recv zmq_msg_close)

(module+ test
  (let ([M1 (make-message)]
        [M2 (make-message)]
        [M3 (make-message)])
    (check-equal? (zmq_msg_init M1) (void))
    (check-equal? (zmq_msg_init_size M2 512) (void))
    (check-equal? (zmq_msg_init_data M3 #"abc" 3) (void))
    (check = (zmq_msg_size M1) 0)
    (check = (zmq_msg_size M2) 512)
    (check = (zmq_msg_size M3) 3)
    (check-equal? (zmq_msg_data M3) #"abc")
    (check-equal? (zmq_msg_close M1) (void))
    (check-equal? (zmq_msg_close M2) (void))
    (check-equal? (zmq_msg_close M3) (void)))

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [M1 (make-message)]
         [M2 (make-message)])
    (check-equal? (zmq_bind P #"inproc://msg-test") (void))
    (check-equal? (zmq_connect Q #"inproc://msg-test") (void))
    (check-equal? (zmq_msg_init_size M1 3) (void))
    (check-equal? (set-message-data! M1 #"987") (void))
    (check-equal? (zmq_msg_init M2) (void))
    (check = (zmq_msg_send M1 Q null) 3)
    (check = (zmq_msg_recv M2 P null) 3)
    (check-equal? (zmq_msg_data M2) #"987")))

;; ----


  ;; (let* ([C (zmq_ctx_new)]
  ;;        [S (zmq_socket C 'REP)])
  ;;   (check-equal? (zmq_bind S #"tcp://*:6555") (void))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PLAINTEXT)))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PRINCIPAL)))
  ;;   ))

;; ---------------------------------------------------------------------------

;;   (let* ([C (zmq_ctx_new)]
;;          [P (zmq_socket C 4)]
;;          [Q (zmq_socket C 3)]
;;          [M1 (zmq_msg_init)]
;;          [M2 (zmq_msg_init_size 10)]
;;          [M3 (zmq_msg_init_data #"654mon")]
;;          [M4 (zmq_msg_init)]
;;          [M5 (zmq_msg_init)]
;;          [M6 (zmq_msg_init)])
;;     (check-equal? (zmq_bind P #"inproc://test2") (void))
;;     (check-equal? (zmq_connect Q #"inproc://test2") (void))
;;     (check = (zmq_msg_size M1) 0)
;;     (check = (zmq_msg_size M2) 10)
;;     (check = (zmq_msg_size M3) 6)

;;     (let ([buf (zmq_msg_data M2)])
;;       (bytes-fill! buf 0)
;;       (check-equal? (zmq_msg_data M2) #"\0\0\0\0\0\0\0\0\0\0")
;;       (bytes-copy! buf 0 #"987zyx")
;;       (check-equal? (zmq_msg_data M2) #"987zyx\0\0\0\0"))
;;     (check = (zmq_msg_send M2 Q 0) 10)
;;     (check = (zmq_msg_recv M1 P 0) 10)
;;     (check-equal? (zmq_msg_data M1) #"987zyx\0\0\0\0")
;;     (check-equal? (zmq_msg_close M1) (void))
;;     (check-equal? (zmq_msg_close M2) (void))

;;     (check = (zmq_send_const P #"ok" 0) 2)
;;     (let ([buf (make-bytes 5)])
;;       (bytes-fill! buf 0)
;;       (check = (zmq_recv Q buf 0) 2)
;;       (check-equal? buf #"ok\0\0\0"))

;;     (check = (zmq_msg_send M3 Q 0) 6)
;;     (check = (zmq_msg_recv M4 P 0) 6)
;;     (check = (zmq_msg_get M4 1) 0)
;;     (check-equal? (zmq_msg_data M4) #"654mon")

;;     (check-equal? (zmq_msg_copy M5 M4) (void))
;;     (check-equal? (zmq_msg_data M5) #"654mon")

;;     (check-equal? (zmq_msg_move M6 M5) (void))
;;     (check-equal? (zmq_msg_data M6) #"654mon")

;;     (check-equal? (zmq_msg_close M3) (void))
;;     (check-equal? (zmq_msg_close M4) (void))
;;     (check-equal? (zmq_msg_close M5) (void))
;;     (check-equal? (zmq_msg_close M6) (void)))

;;   )
