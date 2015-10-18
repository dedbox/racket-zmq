#lang racket/base

(provide (all-defined-out))

(require ffi/unsafe
         racket/port
         zmq/dynamic
         zmq/unsafe/message
         [for-syntax racket/base])

(struct message (ptr) #:property prop:cpointer 0)

(define (make-message [size #f])
  (let ([msg (alloc-msg)])
    (if size (zmq_msg_init_size msg size) (zmq_msg_init msg))
    (message msg)))

(define (bytes->message buf)
  (let ([msg (alloc-msg)])
    (zmq_msg_init_size msg (bytes-length buf))
    (set-message-data! buf msg)
    (message msg)))

(define (datum->message obj)
  (bytes->message (with-output-to-bytes (λ () (write obj)))))

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

(define-syntax-rule (with-message msg body ...)
  (parameterize ([current-message msg]) body ...))

(define-syntax-rule (with-new-message body ...)
  (parameterize ([current-message (make-message)])
    (begin0 (let () body ...) (message-close))))

(define-syntax-rule (with-new-message-size size body ...)
  (parameterize ([current-message (make-message size)])
    (begin0 (let () body ...) (message-close))))

(define-syntax-rule (with-new-message-data buf body ...)
  (parameterize ([current-message (bytes->message buf)])
    (begin0 (let () body ...) (message-close))))

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
  (require rackunit)

  (with-new-message (check = (message-size) 0))
  (with-new-message-size 512 (check = (message-size) 512))
  (with-new-message-data #"abc"
    (check = (message-size) 3)
    (check-equal? (message-data) #"abc"))

  (with-message (datum->message '(98 76 vut))
    (check-equal? (message-data) #"(98 76 vut)")))
