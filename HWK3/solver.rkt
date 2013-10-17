(include "./a-star-search.rkt")

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
