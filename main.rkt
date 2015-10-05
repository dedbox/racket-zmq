#lang racket/base

(require ffi/unsafe
         racket/format
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

(define (context-get name)
  (zmq_ctx_get (current-context) name))

(define (context-set! name value)
  (void (zmq_ctx_set (current-context) name value)))

(define (context-shutdown)
  (void (zmq_ctx_shutdown (current-context))))

(define (context-term)
  (void (zmq_ctx_term (current-context))))

;; ...

(define current-context (make-parameter (make-context)))

(define-syntax-rule (with-context ctx body ...)
  (parameterize ([current-context ctx]) body ...))

(define-syntax-rule (with-new-context body ...)
  (with-context (make-context) body ...))

;; ...

(provide make-context context-get context-set! context-shutdown context-term
          current-context with-context with-new-context)

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
   (check = (context-get 'IPV6) 1)
   (check-equal? (context-shutdown) (void))
   (check-equal? (context-term) (void))))
