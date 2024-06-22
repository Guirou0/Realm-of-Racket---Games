#lang racket

;OBS: Se quiser jogar a versão que possui somente os desafios easy e medium, use o arquivo "server.rkt" no require abaixo
;Caso queira jogar a versão completa com os três desafios, use o arquivo "serverIA.rkt" 

(require (only-in "serverIA.rkt" bon-appetit)
         (only-in "client.rkt" lets-eat)
         2htdp/universe)

(define (server-dinner)
  (launch-many-worlds (bon-appetit)
                      (lets-eat "Gui" LOCALHOST)
                      (lets-eat "Will" LOCALHOST)))