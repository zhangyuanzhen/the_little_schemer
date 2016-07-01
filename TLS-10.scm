(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))
;lat是一个单纯由atom原子构成的列表
(define lat?
    (lambda (l)
      (cond ((null? l) #t)
            (else (and (atom? (car l)) (lat? (cdr l)))))))
(define member?
    (lambda (a l)
      (cond ((null? l) #f)
            (else (or (eq? a (car l)) (member? a (cdr l)))))))
;删除lat中的第一个和参数a相同的原子
(define rember
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((eq? a (car l)) (cdr l))
            (else (cons (car l) (rember a (cdr l)))))))
;firsts函数输入一个lists表参数，null空表，或者只包含非空表。
;它抽取list表中每一个成员的第一个 S-expression表达式构建新的list表
(define firsts
    (lambda (l)
      (cond ((null? l) (quote ()))
            (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((eq? old (car l)) (cons old (cons new (cdr l))))
            (else (cons (car l) (insertR new old (cdr l)))))))
(define insertL
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((eq? old (car l)) (cons new l))
            (else (cons (car l) (insertL new old (cdr l)))))))
;subst函数(把old替换成new)
(define subst
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (cdr lat)))
            (else (cons (car lat) (subst new old (cdr lat)))))))
;把lat中第一个出现的o1或者第一个出现的o2替换为new
(define subst2
    (lambda (new o1 o2 lat)
      (cond ((null? lat) (quote ()))
            ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
            (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
;multirember ，它把lat中所有的a都删除
(define multirember
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((eq? a (car lat)) (multirember a (cdr lat))
            (else (cons (car lat) (multirember a (cdr lat))))))))
(define multiinsertR
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
            (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
(define multiinsertL
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
            (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
(define multisubst
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
            (else (cons (car lat) (multisubst new old (cdr lat)))))))
(define add1
  (lambda (n)
    (+ n 1)))
(define sub1
    (lambda (n)
      (- n 1)))
(define o+
    (lambda (n m)
      (cond ((zero? m) n)
            (else (add1 (o+ n (sub1 m)))))))
(define o-
    (lambda (n m)
      (cond ((zero? m) n)
            (else (sub1 (o- n (sub1 m)))))))
(define o*
    (lambda (n m)
      (cond ((zero? m) n)
            (else (o+ n (o* n (sub1 m)))))))
;tup: 列表中包含的每个s表达式都是数字的列表。
(define addtup
    (lambda (tup)
      (cond ((null? tup) 0)
            (else (o+ (car tup) (addtup (cdr tup)))))))
;将两个tup中相对的数字相加,然后返回相加后的tup
(define tup+
    (lambda (tup1 tup2)
      (cond ((and (null? tup1) (null? tup2)) (quote ()))
            (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(define tup1+
    (lambda (tup1 tup2)
      (cond ((null? tup1) tup2)
            ((null? tup2) tup1)
            (else (cons (o+ (car tup1) (car tup2)) (tup1+ (cdr tup1) (cdr tup2)))))))

(define o<
    (lambda (n m)
      (cond ((zero? m) #f)
            ((zero? n) #t)
            (else (o< (sub1 n) (sub1 m))))))
(define o>
    (lambda (n m)
      (cond ((zero? n) #f)
            ((zero? m) #t)
            (else (o> (sub1 n) (sub1 m))))))
(define o=
    (lambda (n m)
      (cond ((zero? m) (zero? n))
            ((zero? n) #f)
            (else (o= (sub1 n) (sub1 m))))))
(define o^
    (lambda (n m)
      (cond ((zero? m) 1)
            (else (o* n (o^ n (sub1 m)))))))
(define o/
    (lambda (n m)
      (cond ((o< n m) 0)
            (else (add1 (o/ (o- n m) m))))))
(define length
    (lambda (lat)
      (cond ((null? lat) 0)
            (else (add1 (length (cdr lat)))))))

;根据传入的数字,获取其对应在lat中位置的S表达式
(define pick
    (lambda (n lat)
      (cond ((one? n) (car lat))
            (else (pick (sub1 n) (cdr lat))))))
(define one?
    (lambda (n)
      (o= n 1)))
(define rempick
    (lambda (n lat)
      (cond ((one? n) (cdr lat)
            (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))))
;它删除一个lat中的所有出现的数
(define no-nums
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))
(define all-nums
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
            (else (all-nums (cdr lat))))))
(define eqan?
    (lambda (a1 a2)
      (cond ((and (number? a1) (number? a2)) (o= a1 a2))
            ((or (number? a1) (number a2)) #f)
            (else (eq? a1 a2)))))
;写一个函数occur描述一个lat中出现原子a的次数
(define occur
    (lambda (a lat)
      (cond ((null? lat) 0)
            ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat))))))
(define rember*
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((atom? (car l)) (cond ((eq? a (car l)) (rember* a (cdr l)))
                                   (else (cons (car l) (rember* a (cdr l))))))
            (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
(define insertR*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l)) (cond ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                                   (else (cons (car l) (insertR* new old (cdr l))))))
            (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
    (lambda (a l)
      (cond ((null? l) 0)
            ((atom? (car l)) (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
                                   (else (occur* a (cdr l)))))
            (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))
(define subst*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l)) (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                                   (else (cons (car l) (subst* new old (cdr l))))))
            (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
(define insertL*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l)) (cond ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                                   (else (cons (car l) (insertL* new old (cdr l))))))
            (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
(define member*
    (lambda (a l)
      (cond ((null? l) #f)
            ((atom? (car l)) (cond ((eq? a (car l)) #t)
                                   (else (member* a (cdr l)))))
            (else (or (member* a (car l)) (member* a (cdr l)))))))
(define leftmost
    (lambda (l)
      (cond ((atom? (car l)) (car l))
            (else (leftmost (car l))))))
(define eqlist?
    (lambda (l1 l2)
      (cond ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
            ((or (atom? (car l1)) (atom? (car l2))) #f)
            (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
(define equal?
    (lambda (s1 s2)
      (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))
(define eqlist?
    (lambda (l1 l2)
      (cond ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define rember
    (lambda (s l)
      (cond ((null? l) (quote ()))
            ((equal? s (car l)) (cdr l))
            (else (cons (car l) (rember s (cdr l)))))))
;查询一个算术表达式是否只包含有在 + ，× ，和 ^ ，及其后边的数。
(define numbered?
    (lambda (aexp)
      (cond ((atom? aexp) (number? aexp))
            ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) (quote -)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
(define numbered?
    (lambda (aexp)
      (cond ((atom? aexp) (number? aexp))
            ((or (eq? (car (cdr aexp)) (quote +)) (eq? (car (cdr aexp)) (quote -)) (eq? (car (cdr aexp)) (quote *)))
              (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
            (else #f))))
(define operator
    (lambda (aexp)
      (car aexp)))
(define 1st-sub-exp
    (lambda (aexp)
      (car (cdr aexp))))
(define 2st-sub-exp
    (lambda (aexp)
      (car (cdr (cdr aexp)))))
(define value
    (lambda (nexp)
      (cond ((atom? nexp) nexp)
            ((eq? (operator nexp) (quote +)) (o+ (1st-sub-exp nexp) (2st-sub-exp nexp)))
            ((eq? (operator nexp) (quote -)) (o- (1st-sub-exp nexp) (2st-sub-exp nexp)))
            (else (o^ (1st-sub-exp nexp) (2st-sub-exp))))))

;判断一个S表达式是否为set
(define set?
    (lambda (lat)
      (cond ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))
(define makeset
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

;判断set1是否是set2的子集
(define subst?
    (lambda (set1 set2)
      (cond ((null? set1) #t)
            ((null? set2) #f)
            (else (and (member? (car set1) set2) (subst? (cdr set1) set2))))))
;判断两个set是否相等
(define eqset?
    (lambda (set1 set2)
      (and (subst? set1 set2) (subst set2 set1))))
;判断set1是否至少有一个S表达式在set2中
(define intersect?
    (lambda (set1 set2)
      (cond ((null? set1) #f)
            (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))
;求两个set的交集
(define intersect
    (lambda (set1 set2)
      (cond ((null? set1) (quote ()))
            ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2)))))
;求两个set的并集
(define union
    (lambda (set1 set2)
        (cond ((null? set1) set2)
              ((member? (car set1) set2) (union (cdr set1) set2))
              (else (cons (car set1) (union (cdr set1) set2))))))
;获取set中每个子set的交集
(define intersectall
    (lambda (l-set)
        (cond ((null? (cdr l-set)) (car l-set))
              (else (intersect (car l-set) (intersectall (cdr l-set)))))))
;点对, 只包含两个S表达式的列表
(define a-pair?
    (lambda (x)
      (cond ((atom? x) #f)
            ((null? x) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else #f))))
(define first
    (lambda (pair)
        (car pair)))
(define second
    (lambda (pair)
      (car (cdr pair))))
(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 (quote ())))))
(define third
    (lambda (p)
        (car (cdr (cdr p)))))
;rel?  是一个内部嵌套pair的list, 但是其所有子pair是唯一的
(define rel?
    (lambda (l)
      (cond ((null? l) #t)
            (else (and (a-pair? (car l)) (rel? (cdr l)))))))
;fun? 基本同rel, 但其所有子pair的第一个元素也是唯一的
(define fun?
    (lambda (rel)
      (set? (firsts rel))))
;revrel   将rel中所有子pair的两个元素对调
(define revrel
    (lambda (rel)
      (cond ((null? rel) (quote ()))
            (else (cons (build (second rel) (first rel)) (revrel (cdr l)))))))
(define revpair
    (lambda (pair)
      (build (second pair) (first pair))))
(define revrel
    (lambda (rel)
        (cond ((null? rel) (quote ()))
              (else (cons (revpair (car rel)) (revrel (cdr rel)))))))
(define fullfun?
    (lambda (fun)
        (set? (seconds fun))))
(define seconds
    (lambda (l)
      (cond ((null? l) (quote ()))
            (else (cons (second (car l)) (seconds (cdr l)))))))

;fullfun?还可以称作什么 one-to-one
(define one-to-one?
    (lambda (fun)
      (fun? (revrel fun))))

(define rember-f
    (lambda (test? a l)
      (cond ((null? l) (quote ()))
            ((test? a (car l)) (cdr l))
            (else (cons (car l) (rember-f test? a (cdr l)))))))
(define eq?-c
    (lambda (a)
      (lambda (x)
        (eq? x a))))
(define eq?-salad
    (eq?-c 'salad))
(define rember-f
    (lambda (test?)
      (lambda (a l)
        (cond ((null? l) (quote ()))
              ((test? a (car l)) (cdr l))
              (else (cons (car l) ((rember-f test?) a (cdr l))))))))
(define insertL-f
    (lambda (test?)
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((test? old (car l)) (cons new (cons old (cdr l))))
              (else (cons (car l) ((insertL-f test?) a (cdr l))))))))

(define insertR-f
    (lambda (test?)
        (lambda (new old l)
          (cond ((null? l) (quote ()))
                ((test? old (car l)) (cons old (cons new (cdr l))))
                (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))
(define seqL
    (lambda (new old l)
        (cons new (cons old l))))
(define seqR
    (lambda (new old l)
        (cons old (cons new l))))
(define insert-g
    (lambda (seq)
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((eq? old (car l)) (seq new old (cdr l)))
              (else (cons (car l) ((insert-g seq) new old (cdr l))))))))
(define insertL-f (insert-g seqL))
(define insertR-f (insert-g seqR))
(define insertL-f (insert-g (lambda (new old l)
                                      (cons new (cons old l)))))

(define seqS
    (lambda (new old l)
        (cons new l)))
(define subst (insert-g seqS))
(define rember
    (lambda (a l)
      ((insert-g (lambda (new old l) l)) #f a l)))

(define value
    (lambda (nexp)
      (cond ((atom? nexp) nexp)
            ((eq? (operator nexp) (quote +)) (o+ (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
            ((eq? (operator nexp) (quote -)) (o- (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
            ((eq? (operator nexp) (quote ^)) (o^ (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp)))))))

(define atom-to-function
    (lambda (x)
      (cond ((eq? x (quote +)) +)
            ((eq? x (quote -)) -)
            (else ^))))
(define value
    (lambda (nexp)
        (cond ((atom? nexp) nexp)
              (else ((atom-to-function (operator nexp))
                     (value (1st-sub-exp nexp))
                     (value (2st-sub-exp nexp)))))))

(define multirember-f
    (lambda (test?)
      (lambda (a lat)
          (cond ((null? lat) (quote ()))
                ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
                (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))


(define multirember&co
    (lambda (a l col)
      (cond ((null? l) (col (quote ()) (quote ())))
            ((eq? a (car l)) (multirember&co a (cdr l) (lambda (newlat seen)
                                                                (col (cons (car l) newlat) seen))))
            (else (cons (car l) (multirember&co a (cdr l) (lambda (newlat seen)
                                                                (col newlat (cons (car l) seen)))))))))

(define multiinsertLR&co
    (lambda (new oldL oldR l col)
        (cond ((null? l) (col (quote ()) 0 0))
              ((eq? oldL (car l))
                (multiinsertLR&co new oldL oldR (cdr l) (lambda (newlat L R)
                                                                  (col (cons new (cons oldL newlat)) (add1 L) R))))
              ((eq? oldR) (car l)
                (multiinsertLR&co new oldL oldR (cdr l) (lambda (newlat L R)
                                                                  (col (cons oldR (cons new newlat)) L (add1 R)))))
              (else (multiinsertLR&co new oldL oldR (lambda (newlat L R)
                                                                (col (cons (car l) newlat) L R)))))))
(define even?
    (lambda (n)
      (o= (o* (o/ n 2) 2) n)))

(define evens-only*
    (lambda (l)
      (cond ((null? l) (quote ()))
            ((atom? (car l)) (cond ((even? (car l)) (evens-only* (cdr l)))
                                   (else (cons (car l) (evens-only* (cdr l))))))
            (else (cons (evens-only* (car l))
                        (evens-only* (cdr l)))))))

(define evens-only*&co
    (lambda (l col)
        (cond ((null? l) (col (quote ()) 1 0))
              ((atom? (car l))
                (cond ((even? (car l)) (evens-only&co (cdr l) (lambda (newlat p s)
                                                                        (col (cons (car l) newlat) (o* (car l) p) s))))
                      (else (evens-only&co (cdr l) (lambda (newlat p s)
                                                              (col newlat p (o+ (car l) s)))))))

               (else (evens-only&co (car l) (lambda (al ap as)
                                                      (evens-only&co (cdr l) (lambda (bl bp bs)
                                                                                      (col (cons al bl) (o* ap bp) (o+ as bs))))))))))
