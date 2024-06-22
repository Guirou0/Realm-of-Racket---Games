#lang racket

(require 2htdp/universe "server.rkt" "client.rkt")

(define (run2)
  (launch-many-worlds (launch-guess-client "Eva")))