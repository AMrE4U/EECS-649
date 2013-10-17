#lang racket

(include "./manhattan.scm")
(include "./expand.rkt")

(define goal '((1 2 3)(4 5 6)(7 8 0)))
(define test1 '((6 4 2)(1 5 3)(7 0 8)))
(define test2 '((6 4 2)(8 5 3)(1 0 7)))
(define test3 '((6 4 7)(8 5 0)(3 2 1)))
(define test4 '((8 0 7)(6 5 4)(3 2 1)))
(define test5 '((8 6 7)(2 5 4)(3 0 1)))
(define start-time 0)
(define closed (make-hash))

(require data/heap)
(define sort
  (lambda (puznode1 puznode2)
    (<= (puzzle_node-f puznode1) (puzzle_node-f puznode2))))

(define open (make-heap sort))

(define solve
  (lambda (int_state)
    (set! closed (make-hash))
    (set! open (make-heap sort))
    (set! start-time (current-seconds))
    (define state0 (build_node int_state '() '() 0))
    (heap-add! open state0)
    (hash-set! closed int_state 0)
    (search state0)))

(define search
  (lambda (node)
    (cond
      ((equal? (heap-count open) 0)
       (begin
         (set! start-time (- (current-seconds) start-time))
         (printf "No solution can be found. Runtime: ~a seconds." start-time)))
    (else
      (begin
        (define n (heap-min open))
        (heap-remove-min! open)
        (hash-set! closed (puzzle_node-state n) '())
        (cond 
          ((equal? (puzzle_node-state n) goal) 
           (begin
             (set! start-time (- (current-seconds) start-time))
             (show_sol n)))
        (else
         (begin
           (for-each (lambda (child)
                       (add_to_open child))
                     (expand n))
           (search node)))))))))

;;Show steps to get from start to goal.
(define show_sol
  (lambda (node)
    (if (equal? (puzzle_node-h node) 0) (printf "Number of steps ~a: in ~v seconds. ~n" (puzzle_node-g node) start-time) '())
    (cond
      ((equal? (puzzle_node-pred node) '()) (display "Start "))
      (else
       (begin
         (show_sol (puzzle_node-pred node))
         (printf "-> ~a " (puzzle_node-action node)))))))

;;Takes a node and adds it to the open list 
;;if not already in graph or open list.
(define add_to_open
  (lambda (node)
    (define cur_state (puzzle_node-state node))
    (cond
      ((not (hash-has-key? closed cur_state))
       (begin
         (heap-add! open node)
         (hash-set! closed cur_state node)))
      ((equal? (hash-ref closed cur_state) '()) '())
      ((< (puzzle_node-g node) (puzzle_node-g (hash-ref closed cur_state)))
       (begin
         (set-puzzle_node-action! (hash-ref closed cur_state) (puzzle_node-action node))
         (set-puzzle_node-pred! (hash-ref closed cur_state) (puzzle_node-pred node))
         (set-puzzle_node-g! (hash-ref closed cur_state) (puzzle_node-g node))
         (set-puzzle_node-f! (hash-ref closed cur_state) (puzzle_node-f node))
         ;;need to add and remove to get the heap to update, using built in heap.
         (heap-add! open (puzzle_node '(()()()) '() '() 0 0 0))
         (heap-remove-min! open))))))

;;Functions to return a position in the given state.
(define one
  (lambda (s)
    (car (car s))))
(define two
  (lambda (s)
    (car (cdr (car s)))))
(define three
  (lambda (s)
    (last (car s))))
(define four
  (lambda (s)
    (car (car (cdr s)))))
(define five
  (lambda (s)
    (car (cdr (car (cdr s))))))
(define six
  (lambda (s)
    (last (car (cdr s)))))
(define seven
  (lambda (s)
    (car (last s))))
(define eight
  (lambda (s)
    (car (cdr (last s)))))
(define nine
  (lambda (s)
    (last (last s))))
