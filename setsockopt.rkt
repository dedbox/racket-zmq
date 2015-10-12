#lang racket/base

(provide setsockopt)

(require zmq/dynamic
         zmq/socket
         zmq/unsafe/setsockopt)

(define (setsockopt name val [sock (current-socket)])
  (void (zmq_setsockopt sock name val)))

(module+ test
  (require rackunit
           zmq/context
           zmq/getsockopt)

  (define-syntax-rule (check-setsockopt sock name binop val)
    (begin
      (check-equal? (setsockopt name val sock) (void))
      (check binop (getsockopt name sock) val)))

  (with-new-context
   (let-socket ([S 'REP])
     (check-setsockopt S 'AFFINITY = 3)
     (check-setsockopt S 'LINGER   = 30)
     (check-setsockopt S 'RCVHWM   = 500))))
