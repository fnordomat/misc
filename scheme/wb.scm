(define head car)
(define tail cdr)
(define empty? null?)
(define (nth list n) (if (<= n 0) (car list) (nth (cdr list) (- n 1))))

(define (test-check cc quoted x)
  (if (not x)
      (begin (print "failure: " (car quoted)) (cc))
      (begin (print "success: " (car quoted)))))

(define-for-syntax (assert->test-check-in-expr is-assert-symbol? inject-replacement expr)
  (if (not (list? expr)) expr
  (if (null? expr) '()
  (if (is-assert-symbol? (car expr))
      (inject-replacement (map (lambda (e) (assert->test-check-in-expr is-assert-symbol? inject-replacement e)) (cdr expr)))
                          (map (lambda (e) (assert->test-check-in-expr is-assert-symbol? inject-replacement e)) expr)))))

(define symbol<? (lambda (s1 s2) (string<? (symbol->string s1) (symbol->string s2))))

(define-syntax test-steps!
  (ir-macro-transformer (lambda (form inject compare?)
    (let ((body
           (map (lambda (expr)
                  (assert->test-check-in-expr
                   ; so we can replace expression if it starts with the symbol 'assert ...
                   (lambda (x) (compare? 'assert x))
                   ; ... by a call to test-check, on the call/cc's return, the quoted form and the test expression itself.
                   (lambda (x) (cons (inject 'test-check) (cons (inject 'return) (cons `(quote ,x) x))))
                   expr))
                (cdr form))))
    ; make a call/cc expression so we can jump out of execution on the first falsified test assertion
    `(call/cc (lambda (,(inject 'return)) (begin ,@body)))))))

; weight-balanced binary trees, a type of self-balancing binary trees.

; a leaf is represented as 'nil of size 0
; an inner node is key, left, right, size
(define (wb-empty) '())
; serves as ('wb-tree #f #f #f 0))
(define (wb-key object) (nth object 1))
(define (wb-l object) (nth object 2))
(define (wb-r object) (nth object 3))
(define (wb-size object) (if (wb-empty? object) 0 (nth object 4)))
(define (wb-singleton k)
  (list 'wb-tree k (wb-empty) (wb-empty) 1))
(define (wb-construct k l r)
  (list 'wb-tree k l r (+ 1 (wb-size l) (wb-size r))))
(define (wb-empty? tree) (eq? '() tree))

(define (wb-set < list)
  (foldr (lambda (x tree) (wb-insert < tree x)) (wb-empty) list))
; traverse in order
(define (wb->list tree)
  (if (wb-empty? tree) '()
      (cons (wb-key tree) (append (wb->list (wb-l tree)) (wb->list (wb-r tree))))))

; for debugging purposes
(define (balanced? tree)
  (if (wb-empty? tree) #t
      (and (balanced? (wb-l tree))
           (balanced? (wb-r tree))
           (balancing-method-is-balanced? (wb-l tree) (wb-r tree))
           (balancing-method-is-balanced? (wb-r tree) (wb-l tree)))))

; depends on isBalanced predicate "depending on wb algorithm"
; invariant: size[n] = size[n.left] + size[n.right] + 1
(define (wb-insert < tree kx)
  (if (wb-empty? tree) (wb-singleton kx)
      (let ((ky (wb-key tree)))
            (if (< kx ky) (balance-r ky (wb-insert < (wb-l tree) kx) (wb-r tree))
            (if (< ky kx) (balance-l ky (wb-l tree) (wb-insert < (wb-r tree) kx))
                (wb-construct kx (wb-l tree) (wb-r tree)))))))

; useful for sets to check if contained, or for maps to retrieve value
(define (wb-lookup < tree kx)
  (if (wb-empty? tree) #f ; is enough ... (wb-fail 'nil)
      (let ((ky (wb-key tree)))
        (if (< kx ky) (wb-lookup < (wb-l tree) kx)
        (if (< ky kx) (wb-lookup < (wb-r tree) kx)
            ky)))))

(define (is-wb-fail? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'wb-fail)))
(define (wb-fail info) (list 'wb-fail info))
(define (wb-fail-info f) (cadr f))

(define (wb-insert-if-not-present < tree kx)
  (if (wb-empty? tree) (wb-singleton kx)
      (let ((ky (wb-key tree)))
        (if (< kx ky)
            (let ((new-subtree-or-fail (wb-insert-if-not-present < (wb-l tree) kx)))
              (if (is-wb-fail? new-subtree-or-fail) new-subtree-or-fail
                  (balance-r ky new-subtree-or-fail (wb-r tree))))
        (if (< ky kx)
            (let ((new-subtree-or-fail (wb-insert-if-not-present < (wb-r tree) kx)))
              (if (is-wb-fail? new-subtree-or-fail) new-subtree-or-fail
                  (balance-l ky (wb-l tree) new-subtree-or-fail))) 
        (wb-fail ky))))))

(define <-by-fst (lambda (kv1 kv2) (< (car kv1) (car kv2))))
(define (<-by-fst-by <) (lambda (kv1 kv2) (< (car kv1) (car kv2))))

(define (wbmap-insert tree kv)
  (wb-insert <-by-fst tree kv))

; by key only in case of maps:
(define (wbmap-delete tree kx)
  (wb-delete <-by-fst tree (list kx)))
(define (wbmap-lookup tree kx)
  (wb-lookup <-by-fst tree (list kx)))

(define (wbmap-lookup-by < tree kx)
  (wb-lookup (<-by-fst-by <) tree (list kx)))
(define (wbmap-insert-by < tree kv)
  (wb-insert (<-by-fst-by <) tree kv))
(define (wbmap-insert-by-if-not-present < tree kv)
  (wb-insert-if-not-present (<-by-fst-by <) tree kv))

(define (wb-delete < tree kx)
  ; element not present, ignore (or better, return error if desired)
  (if (wb-empty? tree) tree
      (let ((ky (wb-key tree)))
           (if (< kx ky) (balance-l ky (wb-delete < (wb-l tree) kx) (wb-r tree))
           (if (< ky kx) (balance-r ky (wb-l tree) (wb-delete < (wb-r tree) kx))
               (wb-build-from-deleted (wb-l tree) (wb-r tree)))))))

(define (wb-build-from-deleted l r)
  (cond ((wb-empty? l) r)
        ((wb-empty? r) l)
        (else (let
           ((x (wb-pop-minimum r)))
           (balance-r (car x) l (cdr x))))))

(define (wb-pop-minimum tree)
  (let ((k (wb-key tree))
        (l (wb-l tree)))
    (if (wb-empty? l) (cons k (wb-r tree))        
        (let ((x (wb-pop-minimum l)))
          (cons (car x) (balance-l k (cdr x) (wb-r tree)))))))

(define (balance-l k l r)
  (if (balancing-method-is-balanced? l r)
      (wb-construct k l r)
      (rotate-l k l r)))

(define (balance-r k l r)
  (if (balancing-method-is-balanced? r l)
      (wb-construct k l r)
      (rotate-r k l r)))

(define (rotate-l k l r)
  (if (balancing-method-is-single? (wb-l r) (wb-r r))
      (single-l k l r)
      (double-l k l r)))

(define (rotate-r k l r)
  (if (balancing-method-is-single? (wb-r l) (wb-l l))
      (single-r k l r)
      (double-r k l r)))

(define (single-l k l r)
  (wb-construct (wb-key r) (wb-construct k l (wb-l r)) (wb-r r)))

(define (single-r k l r)
  (wb-construct (wb-key l) (wb-l l) (wb-construct k (wb-r l) r)))

(define (double-l k l r)
  (let ((rl (wb-l r)))
    (wb-construct (wb-key rl) (wb-construct k l (wb-l rl)) (wb-construct (wb-key r) (wb-r rl) (wb-r r)))))

(define (double-r k l r)
  (let ((lr (wb-r l)))
    (wb-construct (wb-key lr) (wb-construct (wb-key l) (wb-l l) (wb-l lr)) (wb-construct k r (wb-r lr)))))

; Parameters chosen according to:
; Hirai and Yamamoto, ``Balancing weight-balanced trees'', JFP 21 (3), 2011
; Nievergelt & Reingold: (+ 1 (sqrt 2)), (sqrt 2)
(define delta (+ 1 (sqrt 2))) ; 3)
(define gamma (sqrt 2)) ; 2)

(define (balancing-method-is-balanced? a b)
  (>= (* delta (+ 1 (wb-size a))) (+ 1 (wb-size b))))
(define (balancing-method-is-single? a b)
  (< (+ 1 (wb-size a)) (* gamma (+ 1 (wb-size b)))))

(define (depth tree)
  (if (wb-empty? tree) 0 (+ 1 (max (depth (wb-l tree)) (depth (wb-r tree))))))

; ----------------------

(define unit-test-wb
  (test-steps!
   (print "weight-balanced tree tests")
   (define a #f)
   (set! a (wb-empty))
   (assert (wb-empty? a))
   (set! a (wb-insert < a 1))
   (assert (not (wb-empty? a)))
   (assert (= (wb-size a) 1))
   (set! a (wb-insert < a 1))
   (assert (= (wb-size a) 1))
   (set! a (wb-insert < a 2))
   (assert (= (wb-size a) 2))
   (set! a (wb-delete < a 1))
   (assert (= (wb-size a) 1))
   (assert (= 16 (wb-size (wb-set < (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))))))

; lo and behold: a test suite!
(define (unit-tests-run-all)
  unit-test-wb)

