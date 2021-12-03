#lang racket

(define course (file->lines "racket/day2input.txt"))

; # Part 1

(define (follow-command position-and-depth command)
    (let* ([direction-and-number (string-split command)]
            [direction (car direction-and-number)]
            [n (string->number (cadr direction-and-number))]
            [position (car position-and-depth)]
            [depth (cdr position-and-depth)])
        (case direction
            [("forward") (cons (+ position n) depth)]
            [("up") (cons position (- depth n))]
            [("down") (cons position (+ depth n))])))

(define (go position-and-depth commands)
    (if (> (length commands) 0)
        (go (follow-command position-and-depth (car commands)) (cdr commands))
        (* (car position-and-depth) (cdr position-and-depth))))

(displayln "🌈🌈🌈 Part 1 🌈🌈🌈")
(displayln (go '(0 . 0) course))

; # Part 2
; Too lazy to do this recursively 😅

(define (take-aim pos-depth-aim num)
    (vector (vector-ref pos-depth-aim 0)
            (vector-ref pos-depth-aim 1)
            (+ (vector-ref pos-depth-aim 2) num)
        ))

(define (go-long pos num) (+ pos num))
(define (go-deep depth num aim) (+ depth (* num aim)))

(define (forward pos-depth-aim num)
    (vector (go-long (vector-ref pos-depth-aim 0) num)
            (go-deep (vector-ref pos-depth-aim 1) num (vector-ref pos-depth-aim 2))
            (vector-ref pos-depth-aim 2)))

(define (parse command pos-depth-aim)
    (let* ([direction-and-number (string-split command)]
            [direction (car direction-and-number)]
            [n (string->number (cadr direction-and-number))])
        (case direction
            [("forward") (forward pos-depth-aim n)]
            [("up") (take-aim pos-depth-aim (* -1 n))]
            [("down") (take-aim pos-depth-aim n)])))

(define final-position-depth-and-aim (foldl parse (vector 0 0 0) course))
(define final-position (vector-ref final-position-depth-and-aim 0))
(define final-depth (vector-ref final-position-depth-and-aim 1))

(displayln "\n🌈🌈🌈 Part 2 🌈🌈🌈")
(displayln (* final-position final-depth))