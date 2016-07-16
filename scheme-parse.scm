(define build
    (lambda (s1 s2)
      (cons s1 (cons s2 (quote ())))))
(define first
    (lambda (pair)
      (car pair)))
(define second
    (lambda (pair)
      (car (cdr pair))))
(define new-entry build)


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
(define o/
    (lambda (n m)
      (cond ((o< n m) 0)
            (else (add1 (o/ (o- n m) m))))))
;entry是这样一种数据结构：
;它是一个点对，里面包含两个长度相同的子列表。
;第一个子列表中的所有S表达式各不相同（即为一个集合）。

(define lookup-in-entry
    (lambda (name entry entry-f)
        (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
    (lambda (name names values entry-f)
      (cond ((null? names) (entry-f name))
            ((eq? name (car names)) (car values))
            (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;table 也叫环境(environment)。它是这样一种数据结构：
;它是一个列表，里面所有的S表达式都为entry。
;扩展一个table
(define extend-table cons)
;根据name在table中查找其对应的值。
(define lookup-in-table
    (lambda (name table table-f)
      (cond ((null? table) (table-f name))
            (else (lookup-in-entry name (car table) (lambda (name)
                                                              (lookup-in-table name (cdr table) table-f)))))))
(define atom?
    (lambda (e)
      (and (not (pair? e)) (not (null? e)))))
(define expression-to-action
    (lambda (e)
      (cond ((atom? e) (atom-to-function e))
            (else (list-to-function e)))))

(define atom-to-function
    (lambda (e)
      (cond ((number? e) const*)
            ((eq? e #t) const*)
            ((eq? e #f) const*)
            ((eq? e (quote cons)) const*)
            ((eq? e (quote car)) const*)
            ((eq? e (quote cdr)) const*)
            ((eq? e (quote null?)) const*)
            ((eq? e (quote eq?)) const*)
            ((eq? e (quote atom?)) const*)
            ((eq? e (quote zero?)) const*)
            ((eq? e (quote add1)) const*)
            ((eq? e (quote sub1)) const*)
            ((eq? e (quote number?)) const*)
            (else identifier*))))

(define list-to-function
    (lambda (e)
      (cond ((atom? (car e))
             (cond ((eq? (car e) (quote quote)) quote*)
                   ((eq? (car e) (quote lambda)) lambda*)
                   ((eq? (car e)) (quote cond) cond*)
                   (else application*)))
             (else application*))))
(define zyz_value
    (lambda (e)
      (meaning e (quote ()))))
(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))
(define const*
    (lambda (e table)
      (cond ((number? e) e)
            ((eq? e #f) #f)
            ((eq? e #t) #t)
            (else (build (quote primitive) e)))))

(define quote*
    (lambda (e table)
      (text-of e)))

(define text-of
    (lambda (e)
      (car (cdr e))))

(define identifier*
    (lambda (e table)
      (lookup-in-table e table initial-table)))
(define initial-table
    (lambda (name)
      (car (quote ()))))
(define lambda*
    (lambda (e table)
      (build (quote non-primitive) (cons table (cdr e)))))

(define first
    (lambda (p)
      (car p)))
(define second
    (lambda (p)
      (car (cdr p))))
(define third
    (lambda (p)
      (car (cdr (cdr p)))))
(define table-of first)
(define formals-of second)
(define body-of third)

(define cond-lines-of cdr)

(define evcon
    (lambda (lines table)
      (cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
            ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
            (else (evcon (cdr lines) table)))))
(define else?
    (lambda (x)
      (cond ((atom? x) (eq? x (quote else)))
            (else #f))))
(define question-of first)
(define answer-of second)
(define cond*
    (lambda (e table)
      (evcon (cond-lines-of e) table)))


(define function-of car)
(define arguments-of cdr)
(define evlis
    (lambda (args table)
      (cond ((null? args) (quote ()))
            (else (cons (meaning (car args) table) (evlis (cdr args) table))))))

(define application*
    (lambda (e table)
      (apply (meaning (function-of e) table)
             (evlis (arguments-of e) table))))
(define primitive?
    (lambda (l)
      (eq? (first l) (quote primitive))))
(define non-primitive?
    (lambda (l)
      (eq? (first l) (quote non-primitive))))
(define apply
    (lambda (fun vals)
      (cond ((primitive? fun) (apply-primitive (second fun) vals))
            ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive
    (lambda (name vals)
      (cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
            ((eq? name (quote car)) (car (first vals)))
            ((eq? name (quote cdr)) (cdr (first vals)))
            ((eq? name (quote null?)) (null? (first vals)))
            ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
            ((eq? name (quote atom?)) (atom:? (first vals)))
            ((eq? name (quote zero?)) (zero? (first vals)))
            ((eq? name (quote add1)) (add1 (first vals)))
            ((eq? name (quote sub1)) (sub1 (first sub1)))
            ((eq? name (quote number?)) (number? (first vals))))))

(define atom:?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))
(define add1
    (lambda (n)
      (+ n 1)))
(define sub1
    (lambda (n)
      (- n 1)))
(define apply-closure
    (lambda (closure vals)
      (meaning (body-of closure) (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))

(zyz_value (((lambda (n) (lambda (m) (o+ n m))) 8) 5))
