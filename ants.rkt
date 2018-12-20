#lang racket

(require racket/gui/base)
(require racket/draw)
(require racket/draw/arrow)

(define WIDTH 600)
(define HEIGHT 600)
(define NUM_ANTS 35)
(define ANT_SPEED 5)
(define TIMESTEP_LEN 0.05)
(define ARROW_LEN 2)
(define MAX_TURN_ANG 30)

(define frame (new frame% [label "Ant Model"]
		    [width WIDTH]
		    [height HEIGHT]))
(define canvas (new canvas% [parent frame]))
(define dc (send canvas get-dc))

(define (init-board) 
  (random-seed 1011001)
  (send frame show #t)
  (advance-sim (generate-ants NUM_ANTS)))

(define (generate-ants n)
  (if (= n 0)
    '()
    (cons (list (degrees->radians (random -180 180)) (- (/ WIDTH 2)) (- (/ HEIGHT 2))) (generate-ants (- n 1)))))

(define (clamp-value n min-val max-val)
  (cond [(> n max-val) max-val]
	[(< n min-val) min-val]
	[else n]))

(define (turn-angle ant)
  (let ([ang (list-ref ant 2)]) (+ ang (degrees->radians (random (- MAX_TURN_ANG) MAX_TURN_ANG)))))

(define (behave ants) 
  (if (null? ants)
    '()
    (let ([ang (turn-angle (car ants))] [x (list-ref (car ants) 1)] [y (list-ref (car ants) 2)])
     (cons (list ang 
		 ;(modulo (exact-round (+ (* ANT_SPEED (cos ang)) x)) WIDTH) 
		 ;(modulo (exact-round (+ (* ANT_SPEED (sin ang)) y)) HEIGHT)) 
		 (clamp-value (exact-round (+ (* ANT_SPEED (cos ang)) x)) (- WIDTH) 0)
		 (clamp-value (exact-round (+ (* ANT_SPEED (sin ang)) y)) (- HEIGHT) 0)) 
	   (behave (cdr ants))))))

(define (draw-ant ant)
  (let ([ang (list-ref ant 0)] [x (list-ref ant 1)] [y (list-ref ant 2)])
  (draw-arrow dc x y (+ x (* ARROW_LEN (cos ang))) (+ y (* ARROW_LEN (sin ang))) WIDTH HEIGHT)))

(define (advance-sim ants)  
  (map draw-ant ants)
  ;(send frame show #t)
  (send canvas on-paint)
  (sleep TIMESTEP_LEN)
  (send dc clear)
  (advance-sim (behave ants)))

(module+ main
  (init-board))
