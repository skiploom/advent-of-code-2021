#lang racket

; # Part 1

; It's easiest for me to think of the input file as an `m x n` matrix of bits.
; So `report` for my particular input should look something like:
; ((1 1 0 ... 1)
;  (    ...    )
;  (0 0 0 ... 0))

(define input-lines (file->lines "corvus/advent-of-code-2021/racket/day3input.txt"))
(define (tokenize str) (string-split str #rx"(?<=.)(?=.)"))
(define (tokens->bits tokens) (map string->number tokens))

(define report (map tokens->bits (map tokenize input-lines)))
; (define report '((1 0 1 1) (1 1 0 0) (0 0 1 0) (1 1 1 1) (0 0 0 0)))
(define m (length report))
(define n (length (car report)))

; I really want to transpose this "matrix", then sum the elements of the resultant row lists.
; So I present to you...the jankiest, hardest-to-read "transpose" function of all time.
;
; It basically iterates through the elements of the `report` matrix--
; first by column (n), then by row (m)--in order to build n lists of m-element lists.

(define (transpose-row matrix col)
    (map (lambda (row)
            (list-ref (list-ref matrix row) col))
        (range (length matrix))))

(define (transpose matrix)
    (map (lambda (col)
            (transpose-row matrix col))
        (range (length (car matrix)))))

; Now, to get the first bit of the gamma rate (aka the most common bit in the first report column),
; I can simply sum the elements of the first row in the transposed matrix. (Basically counts the 1s.)
;
; If the sum is greater than half of the number of report rows,
; then 1 must be the most common bit. (0, otherwise.)
(define sums (map (lambda (lst) (apply + lst)) (transpose report)))

(define (is-1-most-common? sum total-bits) (> sum (/ total-bits 2)))
(define (gamma-bit sum total-bits) (if (is-1-most-common? sum total-bits) 1 0))
(define (epsilon-bit sum total-bits) (if (is-1-most-common? sum total-bits) 0 1))

(define gamma-bits (map (lambda (sum) (gamma-bit sum m)) sums))
(define epsilon-bits (map (lambda (sum) (epsilon-bit sum m)) sums))

(define (bit->digit pos bit) (* bit (expt 2 pos)))

; '(0 1 0 0 1) -> 9
(define (binary-bits->decimal bits)
    (foldl (lambda (pos bit acc)
            (+ acc (bit->digit pos bit)))
        0
        (range (- n 1) -1 -1)
        bits))

; In decimal. No need to keep these in binary anymore (for Part 1 at least 🤞).
(define gamma-rate (binary-bits->decimal gamma-bits))
(define epsilon-rate (binary-bits->decimal epsilon-bits))

(define part-1 (* gamma-rate epsilon-rate))

(displayln "🌈🌈🌈 Part 1 🌈🌈🌈")
(displayln part-1)

; # Part 2

(define (oxygen-bit sum total-bits)
    (if (equal? sum (/ total-bits 2))
        1
        (gamma-bit sum total-bits)))

(define (is-desired-bit? lst desired-bit bit-position) (equal? desired-bit (list-ref lst bit-position)))

(define (most-common matrix bit-position)
    (oxygen-bit
        (apply + (transpose-row matrix bit-position))
        (length matrix)))

(define (filter-most-common bit-position matrix)
    (filter (lambda (lst)
        (is-desired-bit? lst (most-common matrix bit-position) bit-position))
    matrix))

; This assumes that there truly exists one oxygen generator rating within the report,
; as the problem describes.
; If there were two, then we'd probably get an index out of bounds error,
; so this can definitely be refactored to avoid that. But I'm trusting the problem prompt here.
(define (oxygen-generator-rating filtered-report bit-position)
    (if (> (length filtered-report) 1)
        (oxygen-generator-rating
            (filter-most-common bit-position filtered-report)
            (add1 bit-position))
        (car filtered-report)))

; These are basically copypasta from the "most-common" functions. Could refactor later.
(define (co2-bit sum total-bits)
    (if (equal? sum (/ total-bits 2))
        0
        (epsilon-bit sum total-bits)))

(define (least-common matrix bit-position)
    (co2-bit
        (apply + (transpose-row matrix bit-position))
        (length matrix)))

(define (filter-least-common bit-position matrix)
    (filter (lambda (lst)
        (is-desired-bit? lst (least-common matrix bit-position) bit-position))
    matrix))

(define (co2-scrubber-rating filtered-report bit-position)
    (if (> (length filtered-report) 1)
        (co2-scrubber-rating
            (filter-least-common bit-position filtered-report)
            (add1 bit-position))
        (car filtered-report)))

(define part-2 (*
    (binary-bits->decimal (oxygen-generator-rating report 0))
    (binary-bits->decimal (co2-scrubber-rating report 0))))

(displayln "🌈🌈🌈 Part 2 🌈🌈🌈")
(displayln part-2)