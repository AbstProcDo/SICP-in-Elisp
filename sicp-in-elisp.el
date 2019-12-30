;;; ~/Documents/primary.doom.d/my-config/sicp-in-elisp.el -*- lexical-binding: t; -*-

;;Place little function on the header
;; Constants
(defvar tolerance 0.00001)
(defvar dx 0.00001)

;;---------------------------------------------------------
;;Litte Functions
(defun negp(x)
  (< x 0))


(defun posp(x)
  (> x 0))

(defun sicp-evenp(x)
  (= 0 (% x 2))
  )

(defun sicp-oddp(x)
  (not
   (= 0 (% x 2)))
  )

(defun divides-p (a b)
  (= (% b a) 0))

(defun prime-p (n)
  (= n (smallest-divisor n)))


(defun average(a b)
  ;; Keep the float
  (/ (+ a b) 2.0))


(defun square(x)
  (* x x))


(defun cube(x)
  (* x x x))

(defun remainder(a b)
  (% a b))

(defun inc(n) (+ n 1))

(defun dec(n) (- n 1))



;; alternative abs
(defun sicp-abs(x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ( t (< x 0) (- x))
        ))

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (remainder a b))))

(defun expmod(base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (t
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;case-1.1.8-sicp-sqrt-1
(defun sicp-sqrt-1(x)
  (defun good-enough-p (guess x)
    (< (abs (- (square guess) x)) 0.001))
  (defun improve (guess x)
    (average guess (/ x guess)))
  (defun sqrt-iter (guess x)
    (if (good-enough-p guess x)
        guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;;case-2.1.1-factorial.el
(defun factorial(n)
  (if (= n 1)
      1
    (* n (factorial (- n 1)))))

;;case-1.2.6-smallest_divisor.el
(defun smallest-divisor(n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n);;停止点
         n)
        ((divides-p test-divisor n)
         test-divisor)
        (t (find-divisor
            n
            (+ test-divisor 1)))))

(defun divides-p (a b)
  (= (% b a) 0))

(defun prime-p (n)
  (= n (smallest-divisor n)))



(defun sqrt-iter(old-guess x)
  (defun good-enough-p(old-guess new-guess)
    (> 0.0001
       (/ (abs (- new-guess old-guess))
          old-guess)))
  (let ((new-guess (improve old-guess x)))
    (if (good-enough-p old-guess new-guess)
        new-guess
      (sqrt-iter new-guess x))))
;;(sqrt-iter 1.0 0.0001)

(defun factorial-recur(n)
  (if (= n 1)
      1
    (* n (factorial (- n 1)))))


(defun factorial-iter(n)
  (fact-iter 1 1 n))
;; 三个参数
(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
;;(factorial 10)

(defun fib-recur-bad(n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))


(defun fib(n)
  (fib-iter 1 0 n))

(defun fib-iter (a b count)
  (if (= count 0)
      b
    (fib-iter (+ a b) a (- count 1))))
;; 这是倒车计算的思维.
;;(fib 9)
;;

(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (t (+ (cc amount
                  (- kinds-of-coins 1))
              (cc (- amount
                     (first-denomination kinds-of-coins))
                  kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))



(defun fast-expt(b n)
  (cond ((= n 0) 1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))



(defun smallest-divisor(n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n);;停止点
         n)
        ((divides-p test-divisor n)
         test-divisor)
        (t (find-divisor
            n
            (+ test-divisor 1)))))

(defun divides-p (a b)
  (= (% b a) 0))

(defun primep (n)
  (= n (smallest-divisor n)))


(defun fermat-test(n)
  (defun try(a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))));;
;;
(defun fast-prime-p(n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime-p n (- times 1)))
        (t nil)))


;;-------------------------------------------------------------
;;Helper Functions

(defun pi-sum(a b)
  (if (> a b)
      0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(defun sum(term a next b)
  (if (> a b)
      0
    (+ (funcall term a)
       (sum term (funcall next a) next b))))

(defun cube(n) (* n n n))
(defun inc(n) (+ n 1))
(defun sum-cubes(a b)
  (sum #'cube a #'inc b))                                       ;



;;case-1.3.4-average-damp.el
(defun average-damp(f)
  (lambda (x)
    (average x (funcall f x)))
  )
;;(funcall (average-damp #'square) 10)



(defun fixed-point(f first-guess)
  "doc占位, 不然难看"
  (defun close-enough-p(v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (defun try(guess)
    (let ((next (funcall f guess)))
      (if (close-enough-p guess next)
          next
        (try next))))
  (try first-guess))



(defun bisect_search_std(f neg-point pos-point)
  "doc"
  (defun close-enough-p(x y)
    (< (abs (- x y)) tolerance))

  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough-p neg-point pos-point)
        midpoint
      (let ((test-value (funcall f midpoint)))
        (cond ((posp test-value)
               (bisect_search_std f neg-point midpoint))
              ((negp test-value)
               (bisect_search_std f midpoint pos-point))
              (t midpoint))))))



(defun half-interval-method(f a b)
  "doc"
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))

    (cond ((and (negp a-value) (posp b-value))
           (bisect_search_std f a b))
          ((and (negp b-value) (posp a-value))
           (bisect_search_std f b a))
          (t ;;自动掉下来
           (error "Values are not of opposite sign" a b)))))

;;case-1.3.4-deriv.el
(defun deriv(g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
       dx)))


;;case-1.3.4-newton-transform.el
(defun newton-transform(g)
  (lambda (x)
    (- x (/ (funcall g x)
            (funcall (deriv g) x)))))


;;case-1.3.4-newton-method.el
(defun newton-method(g guess)
  (fixed-point (newton-transform g) guess))


;;case-1.3.4-fixed-point-of-transform.el
(defun fixed-point-of-transform(g transform guess)
  (fixed-point (funcall transform g) guess))



(provide 'sicp-in-elisp)
;;ends of 'sicp-in-elisp
