#lang racket

(provide (all-defined-out))

(require ffi/unsafe)

;; ---------------------------------------------------------------------------
;; context

(define _context (_cpointer 'context))

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

;; ---------------------------------------------------------------------------
;; getsockopt

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

(define _event_state (_bitmask '(POLLIN POLLOUT POLERR)))

(define _event_state_short (_bitmask '(POLLIN POLLOUT POLERR) _short))

;; ---------------------------------------------------------------------------
;; setsockopt

(define _set_socket_option
  (_enum '(AFFINITY = 4
           LINGER   = 17
           RCVHWM   = 24
           )))

;; ---------------------------------------------------------------------------
;; message

(define-cstruct _msg ([_ (_array _uint8 64)]))

(define cvoid (λ _ (void)))
(define cnull (cast 0 _uint64 _pointer))

(define _send_flags
  (_bitmask '(DONTWAIT = 1
              SNDMORE  = 2)))

(define _recv_flags
  (_bitmask '(DONTWAIT = 1)))

;; ---------------------------------------------------------------------------
;; poll

(define-cstruct _pollitem ([socket  _socket]
                           [fd      _fixint]
                           [events  _event_state_short]
                           [revents _event_state_short]))
