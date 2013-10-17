(struct puzzle_node (state action pred g h f) #:transparent #:mutable)
 
;;Heuristic function is the number of claused left to infer.
(define build_node
  (lambda (s a p g heuristic)
    (puzzle_node s a p g (heuristic s) (+ g (heuristic s)))))

(define start-time 0)

(require data/heap)
(define mysort
  (lambda (puznode1 puznode2)
    (<= (puzzle_node-f puznode1) (puzzle_node-f puznode2))))

(define A*-graph-search
  (lambda ( start goal? moves heuristic)
    (define closed (make-hash))
    (define open (make-heap mysort))
    (set! start-time (current-seconds))
    (define startlst
      (append-map(λ(state) (list (build_node (list state '()) 'start '() 0 heuristic))) start))

    (map (λ(startnode) (heap-add! open startnode)) startlst)
    (map (λ(startnode) (hash-set! closed (puzzle_node-state startnode) 0)) startlst)
    (define (loop)
      (if(<(heap-count open)1)
         '()
         (let((n (heap-min open)))
           (heap-remove-min! open)
           (hash-set! closed (puzzle_node-state n) '())
           (cond((goal?(puzzle_node-state n))
                 (cons(reverse(path n))
                      (begin(loop))))
                (else (for-each (lambda (child)
                                  (add_to_open child open closed))
                                (moves n))
                      (loop) )))))
    (loop)))

;;Needed 2nd A* to show minimal solution.
(define 2ndA*-graph-search
  (lambda ( start goal? moves heuristic)
    (define closed (make-hash))
    (define open (make-heap mysort))
    (set! start-time (current-seconds))
    (define startlst
      (append-map(λ(state) (list (build_node (list state '()) 'start '() 0 heuristic))) start))

    (map (λ(startnode) (heap-add! open startnode)) startlst)
    (map (λ(startnode) (hash-set! closed (puzzle_node-state startnode) 0)) startlst)
    (define (loop)
      (if(<(heap-count open)1)
         '()
         (let((n (heap-min open)))
           (heap-remove-min! open)
           (hash-set! closed (puzzle_node-state n) '())
           (cond((goal?(puzzle_node-state n))
                 (write(reverse(path n))))
                (else (for-each (lambda (child)
                                  (add_to_open child open closed))
                                (moves n))
                      (loop) )))))
    (loop)))

;;Show steps to get from start to goal.
(define path
  (lambda (node)
    (if (equal? node '()) '()
        (cons (list (puzzle_node-action node)
                    (puzzle_node-state node)
                    (puzzle_node-g node)
                    (puzzle_node-h node)
                    (puzzle_node-f node))
              (path (puzzle_node-pred node))))))

;;Takes a node and adds it to the open list 
;;if not already in graph or open list.
(define add_to_open
  (lambda (node open closed)
    ;;(newline)
    ;;(print "New node ******")
    ;;(print node)
    ;;(newline)
    (cond
      ((equal? node '()) '())
      (else
       (define cur_state (puzzle_node-state node))
       (cond
         ((not (hash-has-key? closed cur_state))
          (begin
            (heap-add! open node)
            (hash-set! closed cur_state node)))
         ((equal? (hash-ref closed cur_state) '()) '())
         ((< (puzzle_node-g node) (puzzle_node-g (hash-ref closed cur_state)))
          (begin
            ;(print "duplicate")
            ;(newline)
            ;(print node)
            (set-puzzle_node-action! (hash-ref closed cur_state) (puzzle_node-action node))
            (set-puzzle_node-pred! (hash-ref closed cur_state) (puzzle_node-pred node))
            (set-puzzle_node-g! (hash-ref closed cur_state) (puzzle_node-g node))
            (set-puzzle_node-f! (hash-ref closed cur_state) (puzzle_node-f node))
            ;;need to add and remove to get the heap to update, using built in heap.
            (heap-add! open (puzzle_node '(()()()) '() '() 0 0 0))
            (heap-remove-min! open))))))))