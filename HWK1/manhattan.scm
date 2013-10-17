(define sum-list 
  (lambda (l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))])))

(define find_index
 (lambda (state value)
   (- (length (flatten state)) (length (member value (flatten state))))))

;;This code is not quite as fast but allows new goal states to be defined.
(define manhattan2
  (lambda (state)
    (sum-list (map (lambda (elem)
                     (if (equal? elem 0) 0 ;do not count the distance for the blank
                         (let ([x (find_index state elem)][y (find_index goal elem)])
                           (+ (abs (- (modulo x 3) (modulo y 3)))
                              (abs (- (quotient x 3) (quotient y 3))) ))))
                     (flatten state)))))

;;This code is faster at finding manhattan distance.
(define manhattan
  (lambda (board)
    (begin
      (define count 0)
      (define place (one board))
      (cond
        ((equal? place 1) (set! count (+ count 0)))
        ((equal? place 2) (set! count (+ count 1)))
        ((equal? place 3) (set! count (+ count 2)))
        ((equal? place 4) (set! count (+ count 1)))
        ((equal? place 5) (set! count (+ count 2)))
        ((equal? place 6) (set! count (+ count 3)))
        ((equal? place 7) (set! count (+ count 2)))
        ((equal? place 8) (set! count (+ count 3))))
      (set! place (two board))
      (cond
        ((equal? place 1) (set! count (+ count 1)))
        ((equal? place 2) (set! count (+ count 0)))
        ((equal? place 3) (set! count (+ count 1)))
        ((equal? place 4) (set! count (+ count 2)))
        ((equal? place 5) (set! count (+ count 1)))
        ((equal? place 6) (set! count (+ count 2)))
        ((equal? place 7) (set! count (+ count 3)))
        ((equal? place 8) (set! count (+ count 2))))
      (set! place (three board))
      (cond
        ((equal? place 1) (set! count (+ count 2)))
        ((equal? place 2) (set! count (+ count 1)))
        ((equal? place 3) (set! count (+ count 0)))
        ((equal? place 4) (set! count (+ count 3)))
        ((equal? place 5) (set! count (+ count 2)))
        ((equal? place 6) (set! count (+ count 1)))
        ((equal? place 7) (set! count (+ count 4)))
        ((equal? place 8) (set! count (+ count 3))))
      (set! place (four board))
      (cond
        ((equal? place 1) (set! count (+ count 1)))
        ((equal? place 2) (set! count (+ count 2)))
        ((equal? place 3) (set! count (+ count 3)))
        ((equal? place 4) (set! count (+ count 0)))
        ((equal? place 5) (set! count (+ count 1)))
        ((equal? place 6) (set! count (+ count 6)))
        ((equal? place 7) (set! count (+ count 1)))
        ((equal? place 8) (set! count (+ count 2))))
      (set! place (five board))
      (cond
        ((equal? place 1) (set! count (+ count 2)))
        ((equal? place 2) (set! count (+ count 1)))
        ((equal? place 3) (set! count (+ count 2)))
        ((equal? place 4) (set! count (+ count 1)))
        ((equal? place 5) (set! count (+ count 0)))
        ((equal? place 6) (set! count (+ count 1)))
        ((equal? place 7) (set! count (+ count 2)))
        ((equal? place 8) (set! count (+ count 1))))
      (set! place (six board))
      (cond
        ((equal? place 1) (set! count (+ count 3)))
        ((equal? place 2) (set! count (+ count 2)))
        ((equal? place 3) (set! count (+ count 1)))
        ((equal? place 4) (set! count (+ count 4)))
        ((equal? place 5) (set! count (+ count 1)))
        ((equal? place 6) (set! count (+ count 0)))
        ((equal? place 7) (set! count (+ count 3)))
        ((equal? place 8) (set! count (+ count 2))))
      (set! place (seven board))
      (cond
        ((equal? place 1) (set! count (+ count 2)))
        ((equal? place 2) (set! count (+ count 3)))
        ((equal? place 3) (set! count (+ count 4)))
        ((equal? place 4) (set! count (+ count 1)))
        ((equal? place 5) (set! count (+ count 2)))
        ((equal? place 6) (set! count (+ count 3)))
        ((equal? place 7) (set! count (+ count 0)))
        ((equal? place 8) (set! count (+ count 1))))
      (set! place (eight board))
      (cond
        ((equal? place 1) (set! count (+ count 3)))
        ((equal? place 2) (set! count (+ count 2)))
        ((equal? place 3) (set! count (+ count 3)))
        ((equal? place 4) (set! count (+ count 2)))
        ((equal? place 5) (set! count (+ count 1)))
        ((equal? place 6) (set! count (+ count 2)))
        ((equal? place 7) (set! count (+ count 1)))
        ((equal? place 8) (set! count (+ count 0))))
      (set! place (nine board))
      (cond
        ((equal? place 1) (set! count (+ count 4)))
        ((equal? place 2) (set! count (+ count 3)))
        ((equal? place 3) (set! count (+ count 2)))
        ((equal? place 4) (set! count (+ count 3)))
        ((equal? place 5) (set! count (+ count 2)))
        ((equal? place 6) (set! count (+ count 1)))
        ((equal? place 7) (set! count (+ count 2)))
        ((equal? place 8) (set! count (+ count 1))))
      count)))
    