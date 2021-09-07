#lang racket

(define (nobake flavor)
  string-append flavor "jello")

(define (halfbake flavor)
  (string-append flavor " creme brulee")))