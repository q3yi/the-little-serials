(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (if (null? x)
	#t
	(and (atom? (car x)) (lat? (cdr x))))))

;;; Chapter 2

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

;; (member? 'a '(a b c))
;; (member? 'a '(b c d))

;;; Chapter 3

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

;; (rember 'a '(a b c))
;; (rember 'a '(b c a d e))
;; (rember 'a '(b a a a c d))
;; (rember 'a '())

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

;; (firsts '((a b c) (d e f) (g h i)))
;; (firsts '())

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

;; (insertR 'a 'b '(b c d))
;; (insertR 'a 'b '(e f g))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

;; (insertL 'a 'b '(b c d))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

;; (subst 'a 'b '(b c d))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else (let ((first (car lat))
		 (rest (cdr lat)))
	     (cond
	      ((or (eq? first o1) (eq? first o2)) (cons new rest))
	      (else (cons first (subst2 new o1 o2 rest)))))))))

;; (subst2 'a 'b 'c '(e g f c b d))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

;; (multirember 'a '(a b c a d e a))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;; (multiinsertR 'a 'b '(b c b d b e))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;; (multiinsertL 'a 'b '(b c b d b e))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

;; (multisubst 'a 'b '(b c b d b e))

;;; Chapter 4. Number Games

(define add1
  (lambda (n) (+ n 1)))

(define sub1
  (lambda (n) (- n 1)))

;; (define o+
;;   (lambda (n m)
;;     (cond
;;      ((zero? m) n)
;;      (else (add1 (o+ n (sub1 m)))))))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o+ (add1 n) (sub1 m))))))

;; (define o-
;;   (lambda (n m)
;;     (cond
;;      ((zero? m) n)
;;      (else (sub1 (o- n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

;; (addtup '(1 2 3 4 5))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

;; (o* 3 4)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons
	    (o+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

;; (tup+ '(1 2 3) '(3 2 1))
;; (tup+ '( 1 2 3) '(1 2 3 4 5))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((o< n m) #f)
     ((o> n m) #f)
     (else #t))))

;; (define o=
;;   (lambda (n m)
;;     (not (or (o> n m) (o< n m)))))

;; (define o=
;;   (lambda (n m)
;;     (and (o> m n) (o< m n))))

(define o-expt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (o-expt n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

;; (o/ 15 3)

(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (len (cdr lat)))))))

;; (len '(a b c d))
;; (len '(1 2))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

;; (pick 1 '(a b c))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;; (rempick 3 '(a b c))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

;; (no-nums '(1 a 3 b c 4))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

;; (all-nums '(1 a 2 b 3 c 4 d))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (o= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

;; (occur 'a '(b c d 1 a))
;; (occur 1 '(b c d 1 1 1 c 1))

(define one?
  (lambda (n)
    (= n 1)))

;;; Chapter 5. *Oh My Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons
	    (rember* a (car l))
	    (rember* a (cdr l)))))))

;; (rember* 'a '(a (b a) (c d (a e)) f g))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons old (insertR* new old (cdr l))))))
     (else (cons
	    (insertR* new old (car l))
	    (insertR* new old (cdr l)))))))

;; (insertR* 'a 'b '((b) b (c) ((b d)) b))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (o+ (occur* a (car l))
	  (occur* a (cdr l)))))))

;; (occur* 'a '(a (a b) ((c b a) a) a b))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons new (subst* new old (cdr l))))
       (else
	(cons (car l) (subst* new old (cdr l))))))
     (else
      (cons
       (subst* new old (car l))
       (subst* new old (cdr l)))))))

;; (subst* 'a 'b '(b (a b c (d e b)) f g))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l))
	(cons new
	      (cons old
		    (insertL* new old (cdr l)))))
       (else
	(cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
	    (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? a (car l)) (member* a (cdr l))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

;; (member* 'a '(((b a)) b c d))
;; (member* 'a '((c d e f (g h i)) b c))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((null? l1) #f)
     ((null? l2) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((atom? (car l1)) #f)
     ((atom? (car l2)) #f)
     (else (and (eqlist? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2)))))))

;; (eqlist? '() '())
;; (eqlist? '(a) '(a))
;; (eqlist? '(a) '(a b))
;; (eqlist? '(a b) '())
;; (eqlist? '() '(a b))
;; (eqlist? '((a)) '((a)))
;; (eqlist? '(((a) b)) '(((a) b)))
;; (eqlist? '(((a) b)) '(((a) b) b))
;; (eqlist? '(1 2 3) '(1 2 3))

;;; I don't want to override builtin `equal?'
(define +equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

;; (+equal? 'a 'a)
;; (+equal? 'a 1)
;; (+equal? 1 2)
;; (+equal? 1 1)
;; (+equal? '() '())
;; (+equal? '() 1)
;; (+equal? '((a) b (c (d))) '((a) b (c (d))))

;;; Override `rember' defined in Chapter 3
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((+equal? s (car l)) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

;; (rember '(a b) '((a b) (c d) e f))

;;; Chapter 6. Shadows

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

;; (numbered? '(1 + 2))
;; (numbered? '((3 * 4) + 5))
;; (numbered? '1)
;; (numbered? '(1 + a))

(define value
  (lambda (nexp)
    (if (atom? nexp)
	nexp
	(let ((first (car nexp))
	      (op (car (cdr nexp)))
	      (second (car (cdr (cdr nexp)))))
	  (cond
	   ((eq? op '+)
	    (+ (value first) (value second)))
	   ((eq? op '*)
	    (* (value first) (value second)))
	   (else
	    (expt (value first) (value second))))))))

;; (value 3)
;; (value '(3 + 4))
;; (value '(2 ^ 4))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp) (car aexp)))

(define value
  (lambda (nexp)
    (if (atom? nexp)
	nexp
	(let ((op (operator nexp))
	      (1st (1st-sub-exp nexp))
	      (2nd (2nd-sub-exp nexp)))
	  (cond
	   ((eq? op '+) (+ (value 1st) (value 2nd)))
	   ((eq? op '*) (* (value 1st) (value 2nd)))
	   (else (expt (value 1st) (value 2nd))))))))

;; (value '(+ 1 3))

(define sero?
  (lambda (n) (null? n)))

(define edd1
  (lambda (n) (cons '() n)))

(define zub1
  (lambda (n) (cdr n)))

(define s+
  (lambda (n m)
    (cond ((sero? m) n)
	  (else
	   (s+ (edd1 n) (zub1 m))))))

;; (s+ '(()) '(() ()))

;;; Chapter 7. Firends and Relations

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

;; (makeset '(a a b c b d c d e f e))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
		 (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) set2))
     (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

;; (intersect '(a b c) '(c d e))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

;; (union '(a b c) '(b c d))

(define intersectall
  (lambda (l-set)
    (cond
     ;; We should test (car l-set) here to avoid return empty set
     ;; which only produce empty set when intersected with any set
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		      (intersectall (cdr l-set)))))))

;; (intersectall '((a b c) (b c e) (b f g)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p) (car p)))

(define second
  (lambda (p) (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel)) (first (car rel)))
		 (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

;;; Chapter 8. Lambda the Ultimate

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
		 (rember-f test? a (cdr l)))))))

;; (rember-f eq? 'a '(b a c a))

(define eq?-c
  (lambda (a)
    (lambda (x) (eq? x a))))

;; ((eq?-c 'a) 'a)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
		   (((rember-f test?) a) (cdr l))))))))

;; ((rember-f eq?) 'a '(a b c))

(define rember-eq? (rember-f eq?))

(define insert-g
  (lambda (build)
    (lambda (test?)
      (lambda (new old l)
	(cond
	 ((null? l) '())
	 ((test? old (car l))
	  (build new old (cdr l)))
	 (else (cons (car l)
		     (((insert-g build) test?) new old (cdr l)))))))))

(define insertL-f
  (insert-g (lambda (new old l)
	      (cons new (cons old l)))))

;; ((insertL-f eq?) 'a 'b '(a b c b))

(define insertR-f
  (insert-g (lambda (new old l)
	      (cons old (cons new l)))))

;; ((insertR-f eq?) 'a 'b '(a b c b))

(define subst
  (insert-g (lambda (new old l)
	      (cons new l))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x '*) *)
     (else expt))))

;; (atom-to-function (operator '(+ 1 3)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
	    (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp)))))))

;; (value '(* 8 4))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
	((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
		   ((multirember-f test?) a (cdr lat))))))))

;; ((multirember-f eq?) 'a '(a b a c a d))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-a
  (eq?-c 'a))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat) (multiremberT test? (cdr lat))))
     (else (cons (car lat)
		 (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;; (multirember&co 'a '(b a) a-friend)
;; 1st: (lambda (newlat seen) (a-friend (cons 'b newlat) seen))
;; 2nd: (lambda (newlat seen) (1st newlat (cons 'a seen)))
;; 3rd: (2nd '() '()) => (a-friend (cons 'b '()) (cons 'a '()))

;; (define a-friend+
;;   (lambda (x y) x))

;; (multirember&co 'b '(a b c d) a-friend+)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? oldL (car lat))
      (cons new
	    (cons oldL
		  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? oldR (car lat))
      (cons oldR
	    (cons new
		  (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertLR new oldL oldR (cdr lat)))))))

;; (multiinsertLR 'a 'b 'c '(a b c d))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '() 0 0))
     ((eq? oldL (car lat))
      (multiinsertLR&co new oldL oldR
			(cdr lat)
			(lambda (newlat l r)
			  (col (cons new (cons oldL newlat))
			       (add1 l) r))))
     ((eq? oldR (car lat))
      (multiinsertLR&co new oldL oldR
			(cdr lat)
			(lambda (newlat l r)
			  (col (cons oldR (cons new newlat))
			       l (add1 r)))))
     (else
      (multiinsertLR&co new oldL oldR
			(cdr lat)
			(lambda (newlat l r)
			  (col (cons (car lat) newlat)
			       l r)))))))

;; (multiinsertLR&co 'a 'b 'c '(a b b c b c) (lambda (newlat l r) `(,newlat ,l ,r)))

(define even?
  (lambda (n)
    (= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
	(cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
		 (evens-only* (cdr l)))))))

;; (evens-only* '((1 2 3) (1 (2 3 4) 5 6) 7 8))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*&co (cdr l)
			(lambda (newl p s)
			  (col (cons (car l) newl)
			       (* (car l) p) s))))
       (else
	(evens-only*&co (cdr l)
			(lambda (newl p s)
			  (col newl p (+ (car l) s)))))))
     (else
      (evens-only*&co (car l)
		      (lambda (al ap as)
			(evens-only*&co (cdr l)
					(lambda (dl dp ds)
					  (col (cons al dl)
					       (* ap dp)
					       (+ as ds))))))))))

;; (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (l p s) `(,s ,p ,l)))

;;; Chapter 9. ...and Again, and Again, and Again,...

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? a sorn)))))

;; partial functions
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;; (looking 'caviar '(6 2 4 caviar 5 7 3))

(define eternity
  (lambda (x) (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
		  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (length* (first pora))
	      (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (* (weight* (first pora)) 2)
	      (weight* (second pora)))))))

;; (length* '((a b) (c d))) ;; 4
;; (weight* '((a b) (c d))) ;; 9
;; (shift '((a b) (c d)))   ;; (a (b (c d)))
;; (length* '(a (b (c d)))) ;; 4
;; (weight* '(a (b (c d)))) ;; 7

(define shuffle
  ;; shuffle is a partial function
  ;; (shuffle '((a b) (c d)))
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
		  (shuffle (second pora)))))))

(define C
  ;; Collatz conjecture
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (/ n 2)))
       (else (C (add1 (* 3 n)))))))))

(define A
  ;; Ackermann function
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
	      (A n (sub1 m)))))))

;;; paradox on will-stop?

;; (define will-stop?
;;   (lambda (x) ...))

;; (define last-try
;;   (lambda (x)
;;     (and (will-stop? last-try) (eternity x))))

;; if (will-stop? last-try) return #t then
;;   (eternity x) would be executed which will-stop? is #f
;;   and the outer last-try will not stop
;; if (will-stop? last-try) return #f then
;;   outer last-try will return #f and stop

;; length-0
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

;; length-1
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1 ((lambda (l-0)
	    (cond
	     ((null? l-0) 0)
	     (else (add1
		    (eternity (cdr l-0))))))
	   (cdr l))))))

;; length-2
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1 ((lambda (l-1)
	     (cond
	      ((null? l-1) 0)
	      (else
	       (add1 ((lambda (l-0)
			(cond
			 ((null? l-0) 0)
			 (else
			  (add1 (eternity (cdr l-0))))))
		      (cdr l-1))))))
	   (cdr l))))))

;; length-0
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

;; length-1
((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (g (cdr l)))))))
  eternity))

;; length-2
((lambda (length)
   (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
   eternity)))

;; length-0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; length-1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; length-2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; `eternity' should never be called, replace it with `mk-length'
;; length-0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ;; the `mk-length' is parameter of second lambda, not the same as upper lambda.
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (mk-length (cdr l))))))))

;; length-1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length eternity) (cdr l))))))))

;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l))))))))

;; extract `(mk-length mk-length)' using lambda
;; last `(mk-length mk-length)' will lead infinte loop here, need lazy evaluation
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    ((lambda (length)
;;      (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l)))))))
;;     (mk-length mk-length))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; extract `length' function using lambda
(((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))) '(a b c d))

;; Finally, the Y combinator
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x)
	     ((f f) x)))))))

;;; Chapter 10. What Is the Value of All of This?

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name) (car values))
     (else (lookup-in-entry-help name
				 (cdr names)
				 (cdr values)
				 entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))

;; (lookup-in-entry 'a
;; 		 '((b c a d) (e f g h))
;; 		 (lambda (name) 'nil))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
			    (car table)
			    (lambda (name)
			      (lookup-in-table name
					       (cdr table)
					       table-f)))))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
  (cond
   ((atom? (car e))
    (cond
     ((eq? (car e) 'quote) *quote)
     ((eq? (car e) 'lambda) *lambda)
     ((eq? (car e) 'cond) *cond)
     (else *application)))
   (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define value
  (lambda (e)
    (meaning e (quote ()))))

;;; primitive

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

;;; quote

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

;;; identifier

;; `car' on an emtpy list will raise a exception
;; so `initial-table' function should never be used
;; as it means we get unbinded symbol in our code
(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

;;; lambda

(define *lambda
  (lambda (e table)
    (build 'non-primitive
	   (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define third
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define body-of third)

;;; cond

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)

(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else
      (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;;; application

(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else (cons (meaning (car args) table)
		 (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons) (cons (first vals) (second vals)))
     ((eq? name 'car) (car (first vals)))
     ((eq? name 'cdr) (cdr (first vals)))
     ((eq? name 'null?) (null? (first vals)))
     ((eq? name 'eq?) (eq? (first vals) (second vals)))
     ((eq? name 'atom?) (:atom? (first vals)))
     ((eq? name 'zero?) (zero? (first vals)))
     ((eq? name 'add1) (add1 (first vals)))
     ((eq? name 'sub1) (sub1 (first vals)))
     ((eq? name 'number?) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure)
				      vals)
			   (table-of closure)))))

(define apply+
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive  (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

(define *application
  (lambda (e table)
    (apply+ (meaning (function-of e) table)
	    (evlis (arguments-of e) table))))

;; (meaning '(lambda (x y) (cons x y)) '())
;; (value '((lambda (x y) (cons x y)) (quote a) (quote b)))
;; (value '((lambda (x) (add1 x)) 3))
;; (meaning 'a '(((a) (3))))
;; (value '(zero? 0))

;; (value '(((lambda (le)
;; 	    ((lambda (mk-length)
;; 	       (mk-length mk-length))
;; 	     (lambda (mk-length)
;; 	       (le (lambda (x)
;; 		     ((mk-length mk-length) x))))))
;; 	  (lambda (length)
;; 	    (lambda (l)
;; 	      (cond
;; 	       ((null? l) 0)
;; 	       (else (add1 (length (cdr l)))))))) (quote (a b c d d))))
