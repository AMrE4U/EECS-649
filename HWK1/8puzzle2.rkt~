#lang racket

(define sum-list 
  (lambda (l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))])))

(define find_index
 (lambda (state value)
   (- (length (flatten state)) (length (member value (flatten state))))))

;;This code is not quite as fast but allows new goal states to be defined.
(define manhattan
  (lambda (state)
    (sum-list (map (lambda (elem)
                     (if (equal? elem 0) 0 ;do not count the distance for the blank
                         (let ([x (find_index state elem)][y (find_index goal elem)])
                           (+ (abs (- (modulo x 3) (modulo y 3)))
                              (abs (- (quotient x 3) (quotient y 3))) ))))
                     (flatten state)))))

(struct puzzle_node (state action pred g h f) #:transparent #:mutable)

(define build_node
  (lambda (s a p g)
    (puzzle_node s a p g (manhattan s) (+ g (manhattan s)))))

;;Given a state returns the position of the blank.
(define zero_pos
  (lambda (s)
    (cond
      ((equal? 0 (one s)) 1)
      ((equal? 0 (two s)) 2)
      ((equal? 0 (three s)) 3)
      ((equal? 0 (four s)) 4)
      ((equal? 0 (five s)) 5)
      ((equal? 0 (six s)) 6)
      ((equal? 0 (seven s)) 7)
      ((equal? 0 (eight s)) 8)
      ((equal? 0 (nine s)) 9))))

;;Returns a list of the children nodes of the given node.
(define expand
  (lambda (node)
    (case (zero_pos (puzzle_node-state node))
      [(1) (list (move_right node 1) (move_down node 1))]
      [(2) (list (move_left node 2) (move_right node 2) (move_down node 2))]
      [(3) (list (move_left node 3) (move_down node 3))]
      [(4) (list (move_up node 4) (move_right node 4) (move_down node 4))]
      [(5) (list (move_up node 5) (move_right node 5) (move_left node 5) (move_down node 5))]
      [(6) (list (move_up node 6) (move_left node 6) (move_down node 6))]
      [(7) (list (move_up node 7) (move_right node 7))]
      [(8) (list (move_up node 8) (move_right node 8) (move_left node 8))]
      [(9) (list (move_up node 9) (move_left node 9))])))

;;Returns the node after moving the blank up one space.
(define move_up
  (lambda (node pos)
    (define cur_state (puzzle_node-state node))
    (define cur_g (puzzle_node-g node))
    (case pos
      [(4) (build_node (list (list 0
                                   (two cur_state)
                                   (three cur_state))
                             (list (one cur_state)
                                   (five cur_state)
                                   (six cur_state))
                             (last cur_state))
                       'U node (+ cur_g 1))]
      [(5) (build_node (list (list (one cur_state)
                                   0
                                   (three cur_state))
                             (list (four cur_state)
                                   (two cur_state)
                                   (six cur_state))
                             (last cur_state))
                       'U node (+ cur_g 1))]
      [(6) (build_node (list (list (one cur_state)
                                   (two cur_state)
                                   0)
                             (list (four cur_state)
                                   (five cur_state)
                                   (three cur_state))
                             (last cur_state))
                       'U node (+ cur_g 1))]
      [(7) (build_node (list (car cur_state)
                             (list 0
                                   (five cur_state)
                                   (six cur_state))
                             (list (four cur_state)
                                   (eight cur_state)
                                   (nine cur_state)))
                       'U node (+ cur_g 1))]
      [(8) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   0
                                   (six cur_state))
                             (list (seven cur_state)
                                   (five cur_state)
                                   (nine cur_state)))
                       'U node (+ cur_g 1))]
      [(9) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   (five cur_state)
                                   0)
                             (list (seven cur_state)
                                   (eight cur_state)
                                   (six cur_state)))
                       'U node (+ cur_g 1))])))

;;Returns the node after moving the blank down one space.
(define move_down
  (lambda (node pos)
    (define cur_state (puzzle_node-state node))
    (define cur_g (puzzle_node-g node))
    (case pos
      [(1) (build_node (list (list (four cur_state)
                                   (two cur_state)
                                   (three cur_state))
                             (list 0
                                   (five cur_state)
                                   (six cur_state))
                             (last cur_state))
                       'D node (+ cur_g 1))]
      [(2) (build_node (list (list (one cur_state)
                                   (five cur_state)
                                   (three cur_state))
                             (list (four cur_state)
                                   0
                                   (six cur_state))
                             (last cur_state))
                       'D node (+ cur_g 1))]
      [(3) (build_node (list (list (one cur_state)
                                   (two cur_state)
                                   (six cur_state))
                             (list (four cur_state)
                                   (five cur_state)
                                   0)
                             (last cur_state))
                       'D node (+ cur_g 1))]
      [(4) (build_node (list (car cur_state)
                             (list (seven cur_state)
                                   (five cur_state)
                                   (six cur_state))
                             (list 0
                                   (eight cur_state)
                                   (nine cur_state)))
                       'D node (+ cur_g 1))]
      [(5) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   (eight cur_state)
                                   (six cur_state))
                             (list (seven cur_state)
                                   0
                                   (nine cur_state)))
                       'D node (+ cur_g 1))]
      [(6) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   (five cur_state)
                                   (nine cur_state))
                             (list (seven cur_state)
                                   (eight cur_state)
                                   0))
                       'D node (+ cur_g 1))])))

;;Returns the node after moving the blank right one space.
(define move_right
  (lambda (node pos)
    (define cur_state (puzzle_node-state node))
    (define cur_g (puzzle_node-g node))
    (case pos
      [(1) (build_node (list (list (two cur_state)
                                   0
                                   (three cur_state))
                             (car (cdr cur_state))
                             (last cur_state))
                       'R node (+ cur_g 1))]
      [(2) (build_node (list (list (one cur_state)
                                   (three cur_state)
                                   0)
                             (car (cdr cur_state))
                             (last cur_state))
                       'R node (+ cur_g 1))]
      [(4) (build_node (list (car cur_state)
                             (list (five cur_state)
                                   0
                                   (six cur_state))
                             (last cur_state))
                       'R node (+ cur_g 1))]
      [(5) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   (six cur_state)
                                   0)
                             (last cur_state))
                       'R node (+ cur_g 1))]
      [(7) (build_node (list (car cur_state)
                             (car (cdr cur_state))
                             (list (eight cur_state)
                                   0
                                   (nine cur_state)))
                       'R node (+ cur_g 1))]
      [(8) (build_node (list (car cur_state)
                             (car (cdr cur_state))
                             (list (seven cur_state)
                                   (nine cur_state)
                                   0))
                       'R node (+ cur_g 1))])))

;;Returns the node after moving the blank left one space.
(define move_left
  (lambda (node pos)
    (define cur_state (puzzle_node-state node))
    (define cur_g (puzzle_node-g node))
    (case pos
      [(2) (build_node (list (list 0
                                   (one cur_state)
                                   (three cur_state))
                             (car (cdr cur_state))
                             (last cur_state))
                       'L node (+ cur_g 1))]
      [(3) (build_node (list (list (one cur_state)
                                   0
                                   (two cur_state))
                             (car (cdr cur_state))
                             (last cur_state))
                       'L node (+ cur_g 1))]
      [(5) (build_node (list (car cur_state)
                             (list 0
                                   (four cur_state)
                                   (six cur_state))
                             (last cur_state))
                       'L node (+ cur_g 1))]
      [(6) (build_node (list (car cur_state)
                             (list (four cur_state)
                                   0
                                   (five cur_state))
                             (last cur_state))
                       'L node (+ cur_g 1))]
      [(8) (build_node (list (car cur_state)
                             (car (cdr cur_state))
                             (list 0
                                   (seven cur_state)
                                   (nine cur_state)))
                       'L node (+ cur_g 1))]
      [(9) (build_node (list (car cur_state)
                             (car (cdr cur_state))
                             (list (seven cur_state)
                                   0
                                   (eight cur_state)))
                       'L node (+ cur_g 1))])))

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
