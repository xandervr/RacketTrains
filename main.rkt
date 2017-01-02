#lang racket

(define (RacketTrains)
	(let ((Z21            (make-Z21))
		  (infrabel       (make-infrabel))
		  (NMBS           (make-nmbs)))
		(define (loop)
			((NMBS 'influence-trains) infrabel)
			((infrabel 'influence-trains') Z21 NMBS)
			(loop))
		(loop)))
