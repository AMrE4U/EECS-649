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