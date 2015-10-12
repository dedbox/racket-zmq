#lang racket/base

(provide zmq_getsockopt)

(require ffi/unsafe
         zmq/unsafe/ctypes
         zmq/unsafe/define
         [for-syntax racket/base
                     racket/syntax])

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
  (require rackunit
           zmq/unsafe/context
           zmq/unsafe/socket)

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

;; ----

  ;; (let* ([C (zmq_ctx_new)]
  ;;        [S (zmq_socket C 'REP)])
  ;;   (check-equal? (zmq_bind S #"tcp://*:6555") (void))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PLAINTEXT)))
  ;;   ;; (check-exn exn:fail? (λ () (zmq_getsockopt S 'GSSAPI_PRINCIPAL)))
  ;;   ))
