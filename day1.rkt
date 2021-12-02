#lang racket

(define inputs (map string->number (file->lines "day1input.txt")))

; # Part 1

(define (add1-if-increasing n1 n2 count)
    (if (< n1 n2) (add1 count) count))

(define (accumulate curr prev-and-count)
    (let ([prev (car prev-and-count)]
          [count (cdr prev-and-count)])
        (cons curr (add1-if-increasing prev curr count))))

; The first comparison should not increment the count, so make the initial "previous" element bigAF
(define big-af-number 1000000000)

(define part-1 (cdr (foldl accumulate (cons big-af-number 0) inputs)))
(displayln "🌈🌈🌈 Part 1 🌈🌈🌈")
(displayln part-1)

; # Part 2

(define (window l) (take l 3))
(define (sum-window l) (apply + (window l)))
(define (tl l) (list-tail l 1))

(define (part-2 count prev-sum l)
    (if (>= (length l) 3)
        (part-2 (add1-if-increasing prev-sum (sum-window l) count) (sum-window l) (tl l))
        count))

; The first window sum should not increment the count, so make the initial "previous" sum bigAF
(displayln "\n🌈🌈🌈 Part 2 🌈🌈🌈")
(displayln (part-2 0 big-af-number inputs))