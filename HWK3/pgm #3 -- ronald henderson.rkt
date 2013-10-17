#lang racket
(require (lib "trace.ss"))
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
                 (reverse(path n)))
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
            (set-puzzle_node-action! (hash-ref closed cur_state) (puzzle_node-action node))
            (set-puzzle_node-pred! (hash-ref closed cur_state) (puzzle_node-pred node))
            (set-puzzle_node-g! (hash-ref closed cur_state) (puzzle_node-g node))
            (set-puzzle_node-f! (hash-ref closed cur_state) (puzzle_node-f node))
            ;;need to add and remove to get the heap to update, using built in heap.
            (heap-add! open (puzzle_node '(()()()) '() '() 0 0 0))
            (heap-remove-min! open))))))))

(define (Unify x y θ)
  (cond
    ((not θ) θ)
    ((and (Var? x)(isBound x θ)) (Unify (value x θ) y θ))
    ((and (Var? y)(isBound y θ)) (Unify x (value y θ) θ))
    ((equal? x y) θ)
    ((Var? x) (if (occur? x y θ) #f (cons (cons x y) θ)))
    ((Var? y) (if (occur? y x θ) #f (cons (cons y x) θ)))
    (else (and (pair? x) (pair? y) (= (length x) (length y))
               (equal? (car x) (car y))
               (foldl Unify θ (cdr x)(cdr y)) )) ))

(define (occur? v x θ)
  (cond
    ((and (Var? x)(isBound x θ)) (occur? v (value x θ) θ))
    ((Var? x) (eq? v x))
    (else (and (pair? x)
               (ormap (λ (y) (occur? v y θ)) (cdr x)))) ))

(define Var? symbol?)

(define isBound assoc)

(define (value x θ)
  (cdr (isBound x θ)))

(define (instantiate x θ)
  (cond
    ((and (Var? x)(isBound x θ)) (instantiate (value x θ) θ))
    ((or (Var? x) (not (pair? x))) x)
    (else (cons (car x) (map (λ (y) (instantiate y θ)) (cdr x))))))

(define (freevarsin x)
  (cond
    ((Var? x) (list x))
    ((pair? x) (remove-duplicates(append-map freevarsin(cdr x))))
    (else '()) ))
  
(define (rename ids body fresh-id)
  (instantiate body (map (λ (id) (cons id (fresh-id id))) ids)))

(define (resolve s c)
  (define (equate x y e)
    (let ((t (Unify x y e))) (if t(list t) '())))
  (define (equate-lit x y t)
    (cond((and(¬? x)(not(¬? y))) (equate(cadr x)y t))
         ((and(¬? y)(not(¬? x))) (equate x(cadr y)t))
         (else '()) ))
  (map(λ(th)(list(instantiate-clause(append(cdar s)(cdr c))th) th))
      (equate-lit(car c)(caar s)(cadr s)) ))

(define(¬? x) (and(pair? x)(eq?(car x)'¬)))

(define(freevarsin-clause c)
  (remove-duplicates(append-map freevarsin c)))

(define(rename-clause c fresh-id)
  (cdr(rename(freevarsin-clause c)(cons 'or c) fresh-id)))

(define(instantiate-clause c e)(map(λ(p)(instantiate p e))c))

(define(ATP axioms ¬conjecs)
  (define(move-pos-lit-to-front c)
    (append(filter(λ(l)(not(¬? l)))c)(filter ¬? c)))
  (define +axioms(map move-pos-lit-to-front axioms))
  (define fresh-id gensym)
  (define (res-heuristic state)
    (length (car state)))
  (define(res-moves node)
    (append-map(λ(c)(map(λ(r)(build_node r c node (+ (puzzle_node-g node) 1) res-heuristic))
                        (resolve (puzzle_node-state node)(rename-clause c fresh-id)) ))
               +axioms))
  (define (res-goal? state) 
     (null? (car state)))
  (A*-graph-search ¬conjecs res-goal? res-moves res-heuristic))


(define start-state1
  '((on(a)(table))(on(b)(table))(on(c)(a))
    (block(a))(block(b))(block(c))(clear(b))(clear(c))
    (=(a)(a))(=(b)(b))(=(c)(c))(=(table)(table))))
(define goals1 '((on(a)(b))(on(b)(c))))
(define goals2 '((on(a) x)(on x (c))))
(define actions1
  '(((move b x y)
    ((on b x)(clear b)(clear y)(block b)(block y)
     (¬(= b x))(¬(= b y))(¬(= x y)))
    ((on b y)(clear x)(¬(on b x))(¬(clear y))) )
    ((movetotable b x)
     ((on b x)(clear b)(block b)(¬(= b x))(¬(= x (table))))
     ((on b(table))(clear x)(¬(on b x)))) ))

(define (lexiographic>=? a b)
  (cond
    ((and(number? a)(number? b)) (> a b))
    ((and(symbol? a)(symbol? b)) (string>? (symbol->string a) (symbol->string b)))
    ((and(string? a)(string? b)) (string>? a b))
    ((and(pair? a)(pair? b))
     (if (equal? (car a)(car b)) (lexiographic>=? (cdr a)(cdr b))
         (lexiographic>=? (car a) (car b))))))


(define (inconsistent -pre bind)
  (not (ormap (λ(g) (equal? (car g) (cadr g))) (map cdadr (instantiate-clause -pre bind)))))

(define (plan start goals actions)
  (define (action-quintuples action)
    (list (car action) (filter (λ(c) (not (¬? c))) (cadr action)) (filter ¬? (cadr action))
          (filter (λ(c) (not (¬? c))) (caddr action)) (filter ¬? (caddr action))  ))
  (define(cl l) (map list l))
  (define(negated-clause l) (map(λ(x)(list '¬ x))l))
  (define(getbindings p) (cadr(cadr(car(reverse p)))))
  (define(prove s c)
    (ATP s(list(negated-clause c))))
  (define(heuristic s)
   (length (remove* (map car (car s)) goals)))
  (define(move-action a node)
        (let((action-name (car a))
         (+pre (cadr a))
         (-pre (caddr a))
         (results (cadddr a))
         (deletes (map cadr(car(cddddr a)))) )
      (map(λ(e) (build_node (list (sort (map list (append (instantiate-clause results e) 
                                             (remove* (instantiate-clause deletes e) 
                                             (map car (car (puzzle_node-state node))))))
                           lexiographic>=?) '()) 
                           (instantiate-clause action-name e) 
                           node 
                           (+ (puzzle_node-g node) 1) 
                           heuristic))
                (filter (λ(bind) (inconsistent -pre bind))
                        (map getbindings
                             (prove(car (puzzle_node-state node)) +pre) )))))
  (define(moves node)
        (append-map(λ(act) (move-action (action-quintuples act) node)) actions))
  (define(goal? s)
    (not(null? (prove (car s) goals))))
  (time(map(λ(p) (printf "~a\n\n" p))
  	(2ndA*-graph-search (list(cl (sort start lexiographic>=?))) goal? moves heuristic)))
  (newline))

(trace)
