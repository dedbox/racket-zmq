#lang racket/base

(provide (all-defined-out))

(define current-context (make-parameter #f))

(define current-socket (make-parameter #f))

(define current-message (make-parameter #f))
