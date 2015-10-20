#lang racket/base

(provide make-socket socket-close socket-bind socket-connect with-socket
         with-new-socket let-socket socket-send-bytes socket-send-string
         socket-recv-bytes socket-send socket-recv)

;;; XXX if I put this in the above `provide`, right after `socket-recv-bytes`,
;;; it won't be automatically re-exported by zmq/main.rkt. BUG?
(provide socket-recv-string)

(require ffi/unsafe
         racket/port
         zmq/context
         zmq/dynamic
         zmq/message
         zmq/unsafe/ctypes
         zmq/unsafe/message
         zmq/unsafe/poll
         zmq/unsafe/socket)

;;; XXX garbage collect threads of dead sockets
(define (socket-recv-evt [sock (current-socket)])
  (thread (λ ()
            (let ([items (pollitems [sock 'POLLIN])])
              (zmq_poll (pollitems [sock 'POLLIN]) 1 -1)
              (channel-put (socket-ch sock) (socket-recv 'DONTWAIT sock)))))
  (socket-ch sock))

(struct socket (ptr ch)
        #:property prop:cpointer 0
        #:property prop:evt socket-recv-evt)

(define (make-socket type [ctx (current-context)])
  (socket (zmq_socket ctx type) (make-channel)))

(define (socket-close [sock (current-socket)])
  (void (zmq_close sock)))

(define (socket-bind addr [sock (current-socket)])
  (void (zmq_bind sock (string->bytes/utf-8 addr))))

(define (socket-connect addr [sock (current-socket)])
  (void (zmq_connect sock (string->bytes/utf-8 addr))))

(define (socket-send-bytes buf [flags null] [sock (current-socket)])
  (zmq_send_const sock buf (bytes-length buf) flags))

(define (socket-send-string str [flags null] [sock (current-socket)])
  (socket-send-bytes (string->bytes/utf-8 str) flags sock))

(define (socket-recv-bytes [size 256] [flags null] [sock (current-socket)])
  (let* ([buf (make-bytes size)]
         [len (zmq_recv sock buf size flags)])
    (subbytes buf 0 len)))

(define (socket-recv-string [size 256] [flags null] [sock (current-socket)])
  (bytes->string/utf-8 (socket-recv-bytes size flags sock)))

;; syntax

(define-syntax-rule (with-socket sock body ...)
  (parameterize ([current-socket sock]) body ...))

(define-syntax-rule (with-new-socket type body ...)
  (parameterize ([current-socket (make-socket type)])
    (begin0 (let () body ...) (socket-close))))

(define-syntax-rule (let-socket ([name type] ...) body ...)
  (let ([name (make-socket type)] ...)
    (begin0 (let () body ...) (socket-close name) ...)))

(module+ test
  (require rackunit)

  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (check-equal? (socket-bind "inproc://test1" P) (void))
      (check-equal? (socket-connect "inproc://test1" Q) (void))
      (check = (socket-send-bytes #"abc123" null Q) 6)
      (check-equal? (socket-recv-bytes 10 null P) #"abc123")
      (check = (socket-send-string "ok" null P) 2)
      (check-equal? (socket-recv-bytes 5 null Q) #"ok")))

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
        (check-equal? (message-data N2) #"654mon")))))

;; ---------------------------------------------------------------------------
;; message

;; Beware: zmq_msg_send de-allocates the message automatically -- malloc the
;; buffer in raw mode or the garbage collector will segfault.
(define (socket-send obj [flags null] [sock (current-socket)])
  (let* ([buf (with-output-to-bytes (λ _ (write obj)))]
         [len (bytes-length buf)]
         [msg (malloc _msg 'raw)])
    (set-cpointer-tag! msg msg-tag)
    (zmq_msg_init_size msg len)
    (memcpy (zmq_msg_data msg) buf len)
    (zmq_msg_send msg sock flags)))

(define (socket-recv [flags null] [sock (current-socket)])
  (let ([msg (malloc _msg 'atomic)])
    (set-cpointer-tag! msg msg-tag)
    (zmq_msg_init msg)
    (zmq_msg_recv msg sock flags)
    (with-input-from-bytes (zmq_msg_data msg) read)))

(module+ test
  (with-new-context
    (let-socket ([P 'REP] [Q 'REQ])
      (socket-bind "inproc://messaging-test" P)
      (socket-connect "inproc://messaging-test" Q)
      (check = (socket-send '(1 (2 . 3) four) null Q) 16)
      (check-equal? (socket-recv null P) '(1 (2 . 3) four)))))

;; ---------------------------------------------------------------------------
;; poll

(module+ test
  (define (sync-server)
    (with-new-socket 'REP
      (socket-bind "inproc://sync-test")
      (check-equal? (sync (socket-recv-evt)) 'PING!)))

  (define (sync-client)
    (with-new-socket 'REQ
      (socket-connect "inproc://sync-test")
      (sleep 0.1)
      (socket-send 'PING!)))

  (with-new-context
    (let ([server (thread sync-server)]
          [client (thread sync-client)])
      (thread-wait server)
      (thread-wait client))

    (let ([server (thread sync-server)]
          [client (thread sync-client)])
      (thread-wait client)
      (thread-wait server))

    (let ([server (thread sync-client)]
          [client (thread sync-server)])
      (thread-wait client)
      (thread-wait server))

    (let ([server (thread sync-client)]
          [client (thread sync-server)])
      (thread-wait server)
      (thread-wait client))))
