#lang racket
(require (lib "trace.ss"))

(define goal '())

(struct puzzle_node (state action pred g h f) #:transparent #:mutable)

;;Heuristic function is the number of claused left to infer.
(define build_node
  (lambda (s a p g)
    (puzzle_node s a p g (length s) (+ g (length s)))))

(define start-time 0)
(define closed (make-hash))

(require data/heap)
(define sort
  (lambda (puznode1 puznode2)
    (<= (puzzle_node-f puznode1) (puzzle_node-f puznode2))))

(define open (make-heap sort))

(define (clean-list lst)
  (cond
    ((and (equal? (length lst) 1) (equal? (car lst) #f)) '())
    ((equal? (length lst) 1) lst)
    ((equal? lst '(#f) ) '())
    ((equal? (car lst) #f ) (clean-list (cdr lst)))
   (else (append (list (car lst)) (clean-list(cdr lst)) ))))

;;Returns a list of nodes that represent the next available moves.
(define res-moves
  (lambda (init-state defhornc)
    (define topc (puzzle_node-state init-state))
    (define cur-g (puzzle_node-g init-state))
    (define new_states (clean-list (map (λ y (resolve topc (car y))) defhornc)))
    (map (λ s (build_node (caar s) (cdar s) init-state (+ cur-g 1))) new_states)))

(define deduce
  (lambda ( def-horn-clauses int_state)
    (set! closed (make-hash))
    (set! open (make-heap sort))
    (set! start-time (current-seconds))
    (define state0 (build_node int_state '() '() 0))
    (heap-add! open state0)
    (hash-set! closed int_state 0)
    (search state0 def-horn-clauses)))

(define search
  (lambda (node defhc)
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
                     (res-moves n defhc))
           (search node defhc)))))))))

;;Show steps to get from start to goal.
(define show_sol
  (lambda (node)
    (if (equal? (puzzle_node-h node) 0) (printf "Number of steps ~a: in ~v seconds. ~n" (puzzle_node-g node) start-time) '())
    (cond
      ((equal? (puzzle_node-pred node) '()) (printf "Start \n ~a" (puzzle_node-state node)))
      (else
       (begin
         (show_sol (puzzle_node-pred node))
         (printf "\n ~a --> ~s " (puzzle_node-action node) (puzzle_node-state node)))))))

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

(define axioms1 '(
  ((Criminal x) (¬(American x)) (¬(Weapon y)) (¬(Sells x y z)) (¬(Hostile z)))
  ((Enemy (Nono) (America) ))
  ((Owns (Nono) (M1) ))
  ((Missile (M1) ))
  ((Sells (West) x (Nono)) (¬(Missile x)) (¬(Owns (Nono) x)))
  ((American (West) ))
  ((Weapon x) (¬(Missile x)))
  ((Hostile x) (¬(Hostile (Mother-of x) )))
  ((Hostile x) (¬(Enemy x (America) )))
  ((Hostile x) (¬(Hostile (Father-of x) )))
  ))

(define conjs1 '((¬(Criminal (West)))))

(define axioms2 '(
  ((=(* x(Identity))x))
  ((=(* (Identity)x)x))
  ((=(* x(/ x))(Identity)))
  ((=(* (/ x)x)(Identity)))
  ((=(* x w)v)(¬(=(* x y)u))(¬(=(* y z)w))(¬(=(* u z)v)))
  ((=(* u z)v)(¬(=(* x y)u))(¬(=(* y z)w))(¬(=(* x w)v)))
  ((=(* x x)(Identity)))
  ((=(* (F)(G))(H)))
  ))

(define conjs2 '((¬(=(* (G)(F))(H)))))

(define (Unify x y θ)
  (cond
    ((eq? θ #f) #f)
    ((and (Var? x)(isBound x θ)) (Unify (value x θ) y θ))
    ((and (Var? y)(isBound y θ)) (Unify x (value y θ) θ))
    ((eq? x y) θ)
    ((Var? x) (if (occur? x y θ) #f (cons (cons y x) θ)))
    ((Var? y) (if (occur? y x θ) #f (cons (cons y x) θ)))
    ((equal? x y) θ)
    ((not (and (pair? x)(pair? y))) #f)
    ((not (eq? (car x)(car y))) #f)
    ((not (= (length x)(length y))) #f)
   (else (foldl Unify θ (cdr x)(cdr y)))))

(define (occur? v x θ)
  (cond
    ((and (Var? x)(isBound x θ)) (occur? v (value x θ) θ))
    ((eq? v x) #t)
    ((Var? v) #f)
    ((not (pair? x)) #f)
   (else (ormap (λ y (occur? v y θ)) (cdr x)))))

(define (Var? v)
  (symbol? v))

(define (isBound x θ)
  (if (eq? (assoc x θ) #f) #f #t))

(define (value x θ)
  (cdr (assoc x θ)))

(define (instantiate x θ)
  (cond
    ((and (Var? x)(isBound x θ)) (instantiate (value x θ) θ))
    ((Var? x) x)
   (else (map (λ y (instantiate (car y) θ)) x)))
    )

(define (freevarsin x)
  (cond
    ((Var? x) (list x))
    ((eq? (length x) 1) '())
   (else (foldr union '() (map (λ y (freevarsin (car y))) (cdr x))))))

;;Union, element? function pulled from racket-lang.org
(define (union a b) 
  (cond ((null? b) a) 
        ((element? (car b) a) 
         (union a (cdr b))) 
        (#t (union (cons (car b) a) (cdr b))))) 

(define (element? x lst) 
  (cond ((null? lst) #f) 
        ((eq? x (car lst)) #t) 
        (#t (element? x (cdr lst)))))
  
(define (rename ids body)
  (cond
    ((equal? ids '()) body)
   (else (instantiate body (map (λ y (cons (car y) (gensym (car y)))) ids)))))

(define (resolve negc hornc)
  (define newc (rename (freevarsin hornc) hornc))
  (define bind (Unify (cadar negc) (car newc) '()))
  (cond
    ((equal? bind #f) #f)
   (else (list (union (instantiate (cdr negc) bind ) (instantiate (cdr newc) bind )) hornc) )))

(trace)