#lang racket

(provide make-train)

(define (make-train id node-A node-B distance)
  (let ((type       'train)
        (target-speed 0)
        (time-table '())
        (contents   '())
        (max-speed  0))

    (define (add! x)
      (set! contents (cons x contents)))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'add!)           add!)
        ((eq? msg 'get-max-speed)  (calculate-max-speed))
        ((eq? msg 'get-target-speed) target-speed)
        ((eq? msg 'set-target-speed!) set-target-speed!)
        ((eq? msg 'get-position)   (get-position))
        ((eq? msg 'get-timetable)  timetable)
        ((eq? msg 'set-timetable!) set-timetable!)
        ((eq? msg 'get-id)         id)
        ((eq? msg 'get-type)       type)))
    dispatch))
