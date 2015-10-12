#lang racket/base

(provide getsockopt)

(require zmq/dynamic
         zmq/socket
         zmq/unsafe/getsockopt)

;; -- getsockopt --

(define (getsockopt name [sock (current-socket)])
  (let ([val (zmq_getsockopt sock name)])
    (case name
      [(IDENTITY LAST_ENDPOINT) (bytes->string/utf-8 val)]
      [else val])))

(module+ test
  (require rackunit
           zmq/context)

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
